module Firefox.History

// #r "nuget: FSharp.Data.Dapper"
// #r "nuget: FSharpPlus"
// #r "nuget: Microsoft.Data.SQLite"

[<AutoOpen>]
module Foldable =
    open FSharpPlus

    let andF fs arg : bool = fs |> forall (fun f -> f arg)

    let orF fs arg : bool = fs |> List.exists (fun f -> f arg)

module Regex =
    open System.Text.RegularExpressions

    /// <summary>
    /// Indicates whether the specified regular expression finds a match in the specified input string.
    /// </summary>
    /// <param name="pattern">The regular expression pattern to match.</param>
    /// <param name="text">The string to search for a match.</param>
    let isMatch pattern (text: string) = Regex.IsMatch(text, pattern)

module List =
    let rec intersect list1 list2 : 'a list =
        match list1 with
        | head1 :: tail1 ->
            let rest = intersect tail1 list2
            if List.contains head1 list2 then head1 :: rest else rest
        | [] -> []

module Uri =
    open System
    open FSharpPlus

    let hasQueryParam q (uri: Uri) : bool =
        uri.Query
        |> String.trimStart [ '?' ]
        |> _.Split('&', StringSplitOptions.RemoveEmptyEntries)
        |> map (String.split [ "=" ] >> head)
        |> Array.contains q

    let hasAnyQueryParam qs (uri: Uri) : bool =
        uri.Query
        |> String.trimStart [ '?' ]
        |> _.Split('&', StringSplitOptions.RemoveEmptyEntries)
        |> toList
        |> map (String.split [ "=" ] >> head)
        |> List.intersect qs
        |> List.isEmpty
        |> not

    let withQueryParam (uri: Uri) : bool =
        uri.Query
        |> String.trimStart [ '?' ]
        |> _.Split('&', StringSplitOptions.RemoveEmptyEntries)
        |> Array.isEmpty
        |> not

    let hasFragmentParam f (uri: Uri) =
        uri.Fragment
        |> String.trimStart [ '#' ]
        |> _.Split('&', StringSplitOptions.RemoveEmptyEntries)
        |> map (String.split [ "=" ] >> head)
        |> Array.contains f

type UrlPart =
    { Authority: string
      FirstSegment: string option }

module UrlPart =
    open System
    open FSharpPlus

    let private firstSegment path =
        match path with
        | null
        | ""
        | "/" -> None
        | path -> path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> tryHead

    let create (uri: Uri) : UrlPart =
        { Authority = uri.Authority
          FirstSegment = uri.LocalPath |> firstSegment }

    let toString urlPart =
        match urlPart.FirstSegment with
        | Some segment -> $"{urlPart.Authority}/{segment}"
        | None -> urlPart.Authority

// https://isthisit.nz/posts/2019/sqlite-database-with-dapper-and-fsharp/

module ConnectionFactory =
    open System.Data
    open FSharp.Data.Dapper
    open Microsoft.Data.Sqlite

    let create connectionString () : Connection =
        new SqliteConnection(connectionString) :> IDbConnection
        |> Connection.SqliteConnection

[<CLIMutable>]
type Place = { Id: int; Url: string }

module Place =
    open System
    open FSharpPlus

    let hasQueryParam q place : bool = place.Url |> Uri |> Uri.hasQueryParam q

    let hasAnyQueryParam qs place : bool =
        place.Url |> Uri |> Uri.hasAnyQueryParam qs

    let withQueryParam: Place -> bool = _.Url >> Uri >> Uri.withQueryParam

    let hasFragmentParam f place : bool =
        place.Url |> Uri |> Uri.hasFragmentParam f

    let withFragment: Place -> bool = _.Url >> String.isSubString "#"

    let isNotFirstThreadPost: Place -> bool = _.Url >> Regex.isMatch "/t/.+?/\\d+/\\d+"

module Db =
    open FSharp.Data.Dapper
    open FSharpPlus

    /// <summary>
    /// Deletes place records by their IDs and related records in other tables,
    /// including `moz_annos`, `moz_bookmarks`, `moz_historyvisits`, `moz_inputhistory`, `moz_keywords`,
    /// `moz_places_metadata`, `moz_places`.
    /// </summary>
    /// <param name="querySeqAsync">QuerySeqAsyncBuilder.</param>
    /// <param name="ids">The place IDs to be deleted.</param>
    let deletePlaceIds (querySeqAsync: QuerySeqAsyncBuilder<int>) ids =
        querySeqAsync {
            script
                """delete from moz_annos where id in @Ids;
                   delete from moz_bookmarks where fk in @Ids;
                   delete from moz_historyvisits where place_id in @Ids;
                   delete from moz_inputhistory where place_id in @Ids;
                   delete from moz_keywords where place_id in @Ids;
                   delete from moz_places_metadata where place_id in @Ids;
                   delete from moz_places where id in @Ids;"""

            parameters (dict [ "Ids", box ids ])
        }

    /// <summary>
    /// Deletes place records, given domain or part of the URL and a place filter to match.
    /// This does not delete place records, which are bookmarks.
    /// </summary>
    /// <param name="querySeqAsync">QuerySeqAsyncBuilder.</param>
    /// <param name="deletePlaceIds">Query to delete places IDs.</param>
    /// <param name="urlPart">Part of the URL to match, which will be applied on the database.</param>
    /// <param name="placeFilter">The place filter, which will be applied in-memory, and not on the database.</param>
    let deletePlacesWith (querySeqAsync: QuerySeqAsyncBuilder<Place>) deletePlaceIds urlPart placeFilter =
        async {
            let! places =
                querySeqAsync {
                    script
                        """select distinct P.id, P.url
                           from moz_places P
                             left join moz_bookmarks B on P.id = B.fk
                           where (url like '%' || @UrlPart || '%')
                             and B.id is null"""

                    parameters (dict [ "UrlPart", box urlPart ])
                }

            let places = places |> toList |> filter placeFilter
            let! _ = places |> map _.Id |> deletePlaceIds
            return places
        }

    let private alwaysTrue _ = true

    /// <summary>
    /// Deletes place records, given domain or part of the URL to match.
    /// This does not delete place records, which are bookmarks.
    /// </summary>
    /// <param name="querySeqAsync">QuerySeqAsyncBuilder.</param>
    /// <param name="deletePlaceIds">Query to delete places IDs.</param>
    /// <param name="urlPart">Part of the URL to match.</param>
    let deletePlaces (querySeqAsync: QuerySeqAsyncBuilder<Place>) deletePlaceIds urlPart : Async<Place list> =
        deletePlacesWith querySeqAsync deletePlaceIds urlPart alwaysTrue

    /// <summary>
    /// Deletes untitled places.
    /// This does not delete place records, which are bookmarks.
    /// </summary>
    /// <param name="querySeqAsync">QuerySeqAsyncBuilder.</param>
    /// <param name="deletePlaceIds">Query to delete place IDs.</param>
    let deleteUntitled (querySeqAsync: QuerySeqAsyncBuilder<Place>) deletePlaceIds : Async<Place list> =
        async {
            let! places =
                querySeqAsync {
                    script
                        """select distinct P.id, P.url
                           from moz_places P
                             left join main.moz_bookmarks B on P.id = B.fk
                           where P.title is null
                             and B.parent is null"""
                }

            let places = places |> toList
            let! _ = places |> map _.Id |> deletePlaceIds
            return places
        }

module Tests =
    open System
    open Expecto
    open Expecto.Logging
    open FSharp.Data.Dapper
    open FSharpPlus

    let db =
        @"C:\Users\song\AppData\Roaming\Mozilla\Firefox\Profiles\2ld066b7.default-release\places.sqlite"

    let connectionString = $"Data Source={db};"
    let connectionF = ConnectionFactory.create connectionString

    let querySeqAsync<'R> = querySeqAsync<'R> connectionF
    let querySingleAsync<'R> = querySingleAsync<'R> connectionF
    let querySingleOptionAsync<'R> = querySingleOptionAsync<'R> connectionF

    [<Tests>]
    let specs =
        testList
            "FirefoxHistory"
            [ let logger = Log.createHiera [| "Firefox"; "History" |]
              let writeln = Message.eventX >> logger.info
              let printPlace = _.Url >> sprintf "%A" >> writeln

              let deletePlaceIds = Db.deletePlaceIds querySeqAsync

              testAsync "Delete untitled places" {
                  let! removed = Db.deleteUntitled querySeqAsync deletePlaceIds
                  removed |> map printPlace |> ignore
              }

              // theory data
              let garbageDomainTheoryData: string list =
                  [ "file:///"
                    "?__cf_chl_tk="
                    "?__cf_chl_rt_tk="
                    ".cmstest.optimizely.com"
                    ".logseq-db-demo.pages.dev"
                    ".logseq-db-test.pages.dev"
                    "127.0.0.1"
                    "1tudien.com"
                    "5.vndic.net"
                    "account.proton.me/authorize?"
                    "accounts.firefox.com"
                    "accounts.google.com"
                    "apkdone.com/search/"
                    "app.datadoghq.com/account/login"
                    "app.datadoghq.com/apm"
                    "app.datadoghq.com/dashboard"
                    "app.datadoghq.com/logs?"
                    "app.navan.com"
                    "app.opsgenie.com"
                    "bongban.org/search"
                    "bonus.ly"
                    "bot-verify.onrender.com"
                    "calendar.google.com"
                    "claude.ai/chat"
                    "claude.ai/login"
                    "claude.ai/magic-link"
                    "community.upwork.com"
                    "confluence.sso.episerver.net/login.action?"
                    "confluence.sso.episerver.net/pages/viewpreviousversions.action?"
                    "contacts.google.com/label"
                    "contacts.google.com/person"
                    "contacts.google.com/search"
                    "dichvucong.dancuquocgia.gov.vn/portal"
                    "dictionary.cambridge.org"
                    "docs.google.com/accounts"
                    "download.library.lol"
                    "en.m.wikipedia.org"
                    "engage.cloud.microsoft/main"
                    "episerver99.sharepoint.com/sites/"
                    "eu.docusign.net"
                    "euc-powerpoint.officeapps.live.com"
                    "eur.delve.office.com/?"
                    "fbtag.net"
                    "feedly.com/i/login?"
                    "forms.office.com"
                    "github.com/login"
                    "github.com/orgs"
                    "go.microsoft.com"
                    "hcm55.sapsf.eu"
                    "hoachau.vn/search"
                    "hvdic.thivien.net"
                    "id.atlassian.com"
                    "id.zalo.me/account"
                    "info.hdbank.com.vn/subcriber"
                    "ipfs.io"
                    "jira.sso.episerver.net/issue/CloneIssueProgress.jspa"
                    "jira.sso.episerver.net/login.jsp"
                    "jira.sso.episerver.net/secure"
                    "jr.chat.zalo.me"
                    "jr.nhatkyzalo.vn"
                    "jr.zalo.cloud"
                    "jr.zaloapp.com"
                    "jr.zingmp3.vn"
                    "khachhang.prudential.com.vn"
                    "libgen.is"
                    "www.linkedin.com/authwall"
                    "www.linkedin.com/signup"
                    "localhost:50592"
                    "login.microsoftonline.com"
                    "lucid.app/users/registerOrLogin/"
                    "ludwig.guru/s/"
                    "m.cafebiz.vn"
                    "m.cafef.vn"
                    "m.genk.vn"
                    "m.ruten.com.tw"
                    "m.youtube.com"
                    "mail.google.com"
                    "mail.proton.me/login"
                    "mail.proton.me/u"
                    "mimecast.com"
                    "modyolo.com/download/"
                    "mysignins.microsoft.com/#"
                    "opti-dxp.datadoghq.com/account/login"
                    "opti-dxp.datadoghq.com/apm"
                    "opti-dxp.datadoghq.com/logs?"
                    "optimizely.litmos.com"
                    "optimizely.okta.com"
                    "outlook.office.com/mail/?"
                    "outlook.office.com/mail/archive/"
                    "outlook.office.com/mail/deleteditems"
                    "outlook.office.com/mail/id"
                    "outlook.office.com/mail/inbox"
                    "outlook.office.com/mail/sentitems"
                    "outlook.office.com/owa"
                    "play.google.com"
                    "prep.home.optimizely.com/callback"
                    "prep.login.optimizely.com"
                    "prep.turnstile.episerver.net"
                    "prod.outgoing.prod.webservices.mozgcp.net"
                    "profiler.firefox.com"
                    "ramdajs.com/docs/#"
                    "redirect.zalo.me"
                    "revanced.app/patches?"
                    "sanctuary.js.org/#"
                    "search.brave.com"
                    "searchfox.org/mozilla-central/rev/"
                    "shopee.vn/buyer/login"
                    "shopee.vn/find_similar_products?"
                    "shopee.vn/search"
                    "shopee.vn/verify"
                    "support.upwork.com"
                    "tc01.ep.se"
                    "teams.microsoft.com"
                    "translate.goog/"
                    "translate.google.com"
                    "ttgearlab.com/page/"
                    "upwork-usw2-prod-agora-file-storage.s3.us-west-2.amazonaws.com"
                    "url.de.m.mimecastprotect.com"
                    "us3.datadoghq.com/account/login"
                    "us3.datadoghq.com/apm"
                    "us3.datadoghq.com/dashboard"
                    "us3.datadoghq.com/logs?"
                    "us3.datadoghq.com/monitors"
                    "us3.datadoghq.com/services?"
                    "vi.m.wikipedia.org"
                    "vietstock.vn/tag"
                    "voz.vn/account/alerts"
                    "voz.vn/account/alert/"
                    "voz.vn/direct-messages/"
                    "voz.vn/goto/post?"
                    "voz.vn/p/"
                    "voz.vn/search/"
                    "voz.vn/u/"
                    "web.yammer.com"
                    "web-frameworks-benchmark.netlify.app/compare?"
                    "web-frameworks-benchmark.netlify.app/result?"
                    "world.optimizely.com/csclasslibraries"
                    "www.amazon.com/s/"
                    "www.amazon.com/s?"
                    "www.bahn.de/buchung/fahrplan/suche#"
                    "www.bing.com/search"
                    "www.booking.com"
                    "www.facebook.com/photo"
                    "www.facebook.com/reel"
                    "www.google.com/maps"
                    "www.guru.com/login.aspx?"
                    "www.ldoceonline.com/dictionary/"
                    "www.ldoceonline.com/spellcheck/"
                    "www.linkedin.com/404/"
                    "www.linkedin.com/feed/"
                    "www.linkedin.com/login"
                    "www.linkedin.com/mynetwork/"
                    "www.linkedin.com/mypreferences/"
                    "www.linkedin.com/verify"
                    "www.microsoft365.com/search/"
                    "www.openstreetmap.org/search"
                    "www.perplexity.ai/search/"
                    "www.quora.com/?"
                    "www.rockmods.net/?"
                    "www.rockmods.net/search/"
                    "www.rockmods.net/search?"
                    "www.virustotal.com"
                    "www.windy.com"
                    "www.xing.com/jobs/search?"
                    "www.youtube.com/results"
                    "xacthuc.dichvucong.gov.vn"
                    "you.com/search"
                    "youtu.be"
                    "zdusercontent.com" ]

              testTheoryAsync "Given garbage domain" garbageDomainTheoryData (fun domain ->
                  async {
                      let! removed = domain |> Db.deletePlaces querySeqAsync deletePlaceIds
                      removed |> map printPlace |> ignore
                  })

              // theory data
              let domainWithGarbageFragmentTheoryData: string list =
                  [ "analysiscenter.veracode.com"
                    "andrewlock.net"
                    "bongban.org"
                    "cheatsheetseries.owasp.org"
                    "community.chocolatey.org"
                    "connect.mozilla.org"
                    "csdiy.wiki"
                    "datatracker.ietf.org"
                    "devblogs.microsoft.com"
                    "docs.developers.optimizely.com"
                    "duckduckgo.com"
                    "episerver99.sharepoint.com"
                    "github.com"
                    "github.io"
                    "hoachau.vn"
                    "jira.sso.episerver.net"
                    "learn.microsoft.com"
                    "learnyouahaskell.com"
                    "lemon.io"
                    "localhost"
                    "logseq-db-demo.pages.dev"
                    "logseq-db-test.pages.dev"
                    "lucid.app"
                    "mullvad.net"
                    "nojaf.com"
                    "optimizely.brightfunds.org"
                    "pingsunday.com"
                    "portal.azure.com"
                    "privacyguides.org/en/"
                    "readthedocs.io"
                    "shopee.vn"
                    "ss64.com"
                    "support.mozilla.org"
                    "support.optimizely.com"
                    "tabletennis-reference.com"
                    "thinkpro.vn"
                    "tienphong.vn"
                    "tridactyl.xyz"
                    "ttgearlab.com"
                    "vorapis.pages.dev"
                    "voz.vn"
                    "wikipedia.org/wiki/"
                    "www.24h.com.vn"
                    "www.google.com"
                    "www.jetbrains.com"
                    "www.nuget.org"
                    "www.tabletennis11.com"
                    "www.tabletennisdaily.com"
                    "www.techempower.com"
                    "www.voidtools.com"
                    "xunit.net"
                    "zoom.earth" ]

              testTheoryAsync "Given domain with garbage fragment" domainWithGarbageFragmentTheoryData (fun domain ->
                  async {
                      let placeFilter = Place.withFragment
                      let! removed = (domain, placeFilter) ||> Db.deletePlacesWith querySeqAsync deletePlaceIds
                      removed |> map printPlace |> ignore
                  })

              // theory data
              let domainWithGarbageSubstringTheoryData: (string * string) list =
                  [ "bongbanduyhung.com", "/page/"
                    "connect.mozilla.org", "/page/"
                    "episerver99.sharepoint.com", "download.aspx?"
                    "episerver99.sharepoint.com", "spfxsinglesignon.aspx"
                    "episerveridentity.b2clogin.com", "/authorize?client_id="
                    "eur.delve.office.com", "/profileimage?"
                    "exercism.org", "/solutions"
                    "feedly.com", "/auth/"
                    "github.com", "/blob/"
                    "github.com", "/commits/"
                    "github.com", "/compare/"
                    "github.com", "/releases/"
                    "github.com", "/runs/"
                    "github.com", "/tree/"
                    "github.com/advisories/", "dependabot?query="
                    "login.optimizely.com", "/authorize?client_id="
                    "maybanbongban.vn", "/page/"
                    "optimizely.atlassian.net/servicedesk/", "/user/login?destination="
                    "ttgearlab.com", "/page/"
                    "voz.vn", "/page-"
                    "write.as", "/edit"
                    "www.reddit.com", "/comment/" ]

              testTheoryAsync
                  "Given domain with garbage substring"
                  domainWithGarbageSubstringTheoryData
                  (fun (domain, substring) ->
                      async {
                          let placeFilter = _.Url >> String.isSubString substring
                          let! removed = (domain, placeFilter) ||> Db.deletePlacesWith querySeqAsync deletePlaceIds
                          removed |> map printPlace |> ignore
                      })

              // theory data
              let domainWithAnyGarbageQueryParamTheoryData: (string * string list) list =
                  [ "addons.mozilla.org", [ "q"; "utm_source" ]
                    "apkdone.com", [ "s" ]
                    "apkpure.com", [ "q" ]
                    "app.optimizely.com/signin", [ "continue_to" ]
                    "asp-blogs.azurewebsites.net", [ "page" ]
                    "batdongsan.com.vn", [ "disIds"; "dtln"; "dtnn"; "gcn"; "gtn" ]
                    "bongban.org", [ "page" ]
                    "bongbanduyhung.com", [ "s" ]
                    "butterflyaustralia.com", [ "variant" ]
                    "chatgpt.com", [ "q" ]
                    "community.chocolatey.org", [ "q" ]
                    "confluence.sso.episerver.net", [ "preview"; "src" ]
                    "dailongsport.vn", [ "page" ]
                    "developer.mozilla.org", [ "q" ]
                    "docs.google.com", [ "usp" ]
                    "drive.google.com", [ "usp" ]
                    "duckduckgo.com", [ "q" ]
                    "dungcubongban.vn", [ "page" ]
                    "en.wikipedia.org", [ "search" ]
                    "exercism.org", [ "status" ]
                    "fastly.picsum.photos", [ "hmac" ]
                    "feedly.com", [ "gate" ]
                    "github.com", [ "after"; "check_run_id"; "from"; "page"; "q"; "query"; "return_to"; "tab" ]
                    "hanoian.com", [ "start" ]
                    "hanoinew.vn", [ "filter" ]
                    "hika.fyi", [ "question"; "topic_id" ]
                    "hoachau.vn", [ "brand"; "page" ]
                    "hoangchopbongban.com", [ "q" ]
                    "itviec.com", [ "click_source"; "job_selected"; "lab_feature"; "query" ]
                    "jira.sso.episerver.net", [ "atlOrigin"; "devStatusDetailDialog"; "jql"; "selectedItem" ]
                    "jira.sso.episerver.net/browse/", [ "page" ]
                    "learn.microsoft.com", [ "search"; "tabs"; "terms"; "viewFallbackFrom" ]
                    "localhost", [ "code" ]
                    "login.taobao.com", [ "redirectURL" ]
                    "luatvietnam.vn", [ "page" ]
                    "lucid.app", [ "invitationId"; "product"; "redirect_url" ]
                    "masothue.com", [ "q" ]
                    "media4.giphy.com", [ "ep" ]
                    "modyolo.com", [ "s" ]
                    "mycroftproject.com/install.html", [ "id" ]
                    "mycroftproject.com/search-engines.html", [ "name" ]
                    "nguoiquansat.vn", [ "gidzl" ]
                    "nhattao.com", [ "q" ]
                    "nodeflair.com", [ "page" ]
                    "nuget.optimizely.com", [ "q" ]
                    "ooakforum.com", [ "sid"; "start" ]
                    "opti-dxp.datadoghq.com", [ "query" ]
                    "optimizely.atlassian.net/servicedesk/", [ "page"; "reporter"; "src"; "statuses"; "token" ]
                    "phobongban.vn", [ "filter_thuong-hieu" ]
                    "pico.vn", [ "property" ]
                    "piped.video", [ "search_query" ]
                    "rethinkdns.com", [ "q" ]
                    "s.taobao.com", [ "q" ]
                    "searchfox.org", [ "q" ]
                    "shopee.vn", [ "cmtid"; "entryPoint"; "page"; "searchKeyword"; "sp_atk" ]
                    "support.mozilla.org", [ "as" ]
                    "support.optimizely.com", [ "return_to" ]
                    "thinkpro.vn", [ "skuId"; "tinh-trang" ]
                    "tiemanhnhabap.gump.gg", [ "sid" ]
                    "tiki.vn", [ "q"; "spid" ]
                    "topdev.vn", [ "src" ]
                    "ttsport.vn", [ "page" ]
                    "visa.vfsglobal.com", [ "q" ]
                    "vneconomy.vn", [ "trang" ]
                    "voz.party", [ "page" ]
                    "voz.vn", [ "page"; "prefix_id"; "show_only" ]
                    "web.analysiscenter.veracode.com", [ "code" ]
                    "world.optimizely.com", [ "releaseNoteId" ]
                    "www.adidas.com", [ "q" ]
                    "www.amazon.com", [ "keywords"; "rh" ]
                    "www.amazon.fr", [ "field-keywords" ]
                    "www.apkmirror.com", [ "post_type" ]
                    "www.contra.de", [ "search" ]
                    "www.cpubenchmark.net", [ "id" ]
                    "www.dict.cc", [ "s" ]
                    "www.donic.com", [ "order"; "p" ]
                    "www.facebook.com", [ "rdid" ]
                    "www.freelancer.com", [ "search_keyword" ]
                    "www.google.com", [ "q" ]
                    "www.guru.com", [ "SearchUrl" ]
                    "www.informatik.uni-leipzig.de", [ "word" ]
                    "www.nhaccuatui.com", [ "st" ]
                    "www.npmjs.com", [ "activeTab"; "q" ]
                    "www.nuget.org", [ "q" ]
                    "www.reddit.com", [ "chainedPosts" ]
                    "www.ruten.com.tw", [ "q"; "sort" ]
                    "www.tabletennis11.com", [ "q" ]
                    "www.tabletennisdaily.com", [ "page"; "q" ]
                    "www.upwork.com", [ "q" ]
                    "www.vinmec.com", [ "link_type" ]
                    "www.xing.com", [ "ijt"; "keywords"; "sc_o" ]
                    "www.xxl.se", [ "query" ]
                    "www.youtube.com", [ "index"; "search_query"; "t" ]
                    "yasakatabletennis.com", [ "filter" ] ]

              testTheoryAsync
                  "Given domain with any garbage query param"
                  domainWithAnyGarbageQueryParamTheoryData
                  (fun (domain, queryParams) ->
                      async {
                          let placeFilter = Place.hasAnyQueryParam queryParams
                          let! removed = (domain, placeFilter) ||> Db.deletePlacesWith querySeqAsync deletePlaceIds
                          removed |> map printPlace |> ignore
                      })

              // theory data
              let domainWithGarbageFragmentParamTheoryData: (string * string) list =
                  [ "vietnamnet.vn", "vnn_source"; "vnexpress.net", "vn_source" ]

              testTheoryAsync
                  "Given domain with garbage fragment param"
                  domainWithGarbageFragmentParamTheoryData
                  (fun (domain, fragmentParam) ->
                      async {
                          let placeFilter = Place.hasFragmentParam fragmentParam
                          let! removed = (domain, placeFilter) ||> Db.deletePlacesWith querySeqAsync deletePlaceIds
                          removed |> map printPlace |> ignore
                      })

              // theory data
              let domainWithGarbageRegexTheoryData: (string * string) list =
                  [ "apkpure.com", "/[\\w-]+/"
                    "bongban.org", "/forums/.+?\\d+/page-\\d+"
                    "bongban.org", "/threads/.+?/page-\\d+"
                    "community.windy.com", "/\\d+/.+?/\\d+"
                    "diendan.footballvn.net", "/threads/\\d+-[^/]+/page\\d+\\.html"
                    "dungbongban.com", "-page\\d+\\.html"
                    "github.com", "/commit/\\w{40}"
                    "github.com", "/pull/\\d+/commits"
                    "github.com", "/pull/\\d+/files"
                    "mytabletennis.net", "_page\\d+\\.html"
                    "tabletennis-reference.com", "detail/\\d+/"
                    "thanglongkydao.com", "/threads/.+?/page\\d+"
                    "voz.party", "/d/\\d+-.+?/\\d+$"
                    "voz.vn", "\\.\\d+/reply"
                    "voz.vn", "/unread$"
                    "www.apkmirror.com", "/apk/[\\w-]+/[\\w-]+/"
                    "www.nuget.org", "/packages/[\\w\\.]+/\\d+\\.\\d+\\.\\d+"
                    "www.otofun.net", "\\.\\d+/page-\\d+"
                    "www.tabletennisdaily.com", "/forum/topics/.+?\\d+/page-\\d+" ]

              testTheoryAsync
                  "Given domain with garbage regex"
                  domainWithGarbageRegexTheoryData
                  (fun (domain, pattern) ->
                      async {
                          let placeFilter = _.Url >> Regex.isMatch pattern
                          let! removed = (domain, placeFilter) ||> Db.deletePlacesWith querySeqAsync deletePlaceIds
                          removed |> map printPlace |> ignore
                      })

              // theory data
              let domainWithGarbageNotFirstThreadPostTheoryData: string list =
                  [ "community.e.foundation"
                    "discuss.logseq.com"
                    "discuss.privacyguides.net"
                    "f247.com"
                    "forum.f-droid.org"
                    "forum.uipath.com"
                    "forum.rescript-lang.org"
                    "forums.fsharp.org" ]

              testTheoryAsync
                  "Given domain with garbage not first thread post"
                  domainWithGarbageNotFirstThreadPostTheoryData
                  (fun domain ->
                      async {
                          let placeFilter = Place.isNotFirstThreadPost
                          let! removed = (domain, placeFilter) ||> Db.deletePlacesWith querySeqAsync deletePlaceIds
                          removed |> map printPlace |> ignore
                      })

              // theory data
              let domainWithComplexGarbagePlaceFilterTheoryData: (string * (Place -> bool)) list =
                  [ "local",
                    andF
                        [ _.Url >> Regex.isMatch ":\\d+/"
                          orF [ Place.withQueryParam; Place.withFragment ] ]
                    "localhost/", Place.withQueryParam
                    "nuget.optimizely.com", andF [ Place.hasQueryParam "id"; Place.hasQueryParam "v" ]
                    "world.taobao.com", andF [ Place.hasQueryParam "a"; Place.hasQueryParam "b" ] ]

              testTheoryAsync
                  "Given domain with complex garbage place filter to delete"
                  domainWithComplexGarbagePlaceFilterTheoryData
                  (fun (domain, placeFilter) ->
                      async {
                          let! removed = (domain, placeFilter) ||> Db.deletePlacesWith querySeqAsync deletePlaceIds
                          removed |> map (_.Url >> sprintf "%A" >> writeln) |> ignore
                      })

              // history entries, excluding bookmarks
              let urlParts () =
                  querySeqAsync<string> {
                      script
                          """select P.url
                             from moz_places P
                                      left join main.moz_bookmarks B on P.id = B.fk
                             where B.id is null"""
                  }
                  |> Async.RunSynchronously
                  |> toList
                  |> map (Uri >> UrlPart.create)

              test "Count by authority, count >= 5" {
                  urlParts ()
                  |> List.countBy _.Authority
                  |> filter (fun (_, count) -> count >= 5)
                  |> sortByDescending snd
                  |> map fst
                  |> (sprintf "%A" >> writeln)
              }

              test "Count by authority and first path's segment, count >= 5" {
                  urlParts ()
                  |> List.countBy UrlPart.toString
                  |> filter (fun (_, count) -> count >= 5)
                  |> sortByDescending snd
                  |> map fst
                  |> (sprintf "%A" >> writeln)
              } ]
