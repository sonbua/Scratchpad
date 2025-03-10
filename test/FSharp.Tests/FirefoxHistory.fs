module Firefox.History

// #r "nuget: FSharp.Data.Dapper"
// #r "nuget: FSharpPlus"
// #r "nuget: Microsoft.Data.SQLite"

[<AutoOpen>]
module Foldable =
    open FSharpPlus

    let andF (fs: ('a -> bool) list) arg : bool = fs |> forall (fun f -> f arg)

    let inline orF (fs: ('a -> bool) list) arg : bool = fs |> exists (fun f -> f arg)

module Regex =
    open System.Text.RegularExpressions

    /// <summary>
    /// Indicates whether the specified regular expression finds a match in the specified input string.
    /// </summary>
    /// <param name="pattern">The regular expression pattern to match.</param>
    /// <param name="text">The string to search for a match.</param>
    let isMatch pattern (text: string) = Regex.IsMatch(text, pattern)

module List =
    let intersect list1 list2 =
        let rec aux list1 list2 acc =
            match list1 with
            | head1 :: tail1 ->
                if List.contains head1 list2 then
                    aux tail1 list2 (head1 :: acc)
                else
                    aux tail1 list2 acc
            | [] -> List.rev acc

        aux list1 list2 []

module Uri =
    open System
    open FSharpPlus

    let private extractKeys (uri: Uri) =
        uri.Query
        |> String.trimStart [ '?' ]
        |> _.Split('&', StringSplitOptions.RemoveEmptyEntries)
        |> toList
        |> map (String.split [ "=" ] >> head)

    let hasQueryParams (qs: string list) (uri: Uri) : bool =
        uri |> extractKeys |> List.intersect qs |> (=) qs

    let hasAnyQueryParam qs (uri: Uri) : bool =
        uri |> extractKeys |> List.intersect qs |> List.isEmpty |> not

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

    let hasQueryParams qs place : bool =
        place.Url |> Uri |> Uri.hasQueryParams qs

    let hasAnyQueryParam qs place : bool =
        place.Url |> Uri |> Uri.hasAnyQueryParam qs

    let withQueryParam: Place -> bool = _.Url >> Uri >> Uri.withQueryParam

    let hasFragmentParam f place : bool =
        place.Url |> Uri |> Uri.hasFragmentParam f

    let withFragment: Place -> bool = _.Url >> String.isSubString "#"

    let isNotFirstThreadPost: Place -> bool = _.Url >> Regex.isMatch "/t/.+?/\\d+/\\d+"

open FSharp.Data.Dapper
open FSharpPlus

type Db(connectionString) =
    let alwaysTrue _ = true

    member this.querySeqAsync<'R>() =
        connectionString |> ConnectionFactory.create |> querySeqAsync<'R>

    /// <summary>
    /// Deletes place records by their IDs and related records in other tables,
    /// including `moz_annos`, `moz_bookmarks`, `moz_historyvisits`, `moz_inputhistory`, `moz_keywords`,
    /// `moz_places_metadata`, `moz_places`.
    /// </summary>
    /// <param name="ids">The place IDs to be deleted.</param>
    member private this.deletePlaceIds ids =
        this.querySeqAsync<int> () {
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
    /// <param name="urlPart">Part of the URL to match, which will be queried against the database.</param>
    /// <param name="placeFilter">The place filter, which will be applied in-memory, and not on the database.</param>
    member this.deletePlacesWith urlPart placeFilter =
        async {
            let! places =
                this.querySeqAsync<Place> () {
                    script
                        """select distinct P.id, P.url
                           from moz_places P
                             left join moz_bookmarks B on P.id = B.fk
                           where (url like '%' || @UrlPart || '%')
                             and B.id is null"""

                    parameters (dict [ "UrlPart", box urlPart ])
                }

            let places = places |> toList |> filter placeFilter
            let! _ = places |> map _.Id |> this.deletePlaceIds
            return places
        }

    /// <summary>
    /// Deletes place records, given domain or part of the URL to match.
    /// This does not delete place records, which are bookmarks.
    /// </summary>
    /// <param name="urlPart">Part of the URL to match.</param>
    member this.deletePlaces urlPart =
        this.deletePlacesWith urlPart alwaysTrue

    /// <summary>
    /// Deletes untitled places.
    /// This does not delete place records, which are bookmarks.
    /// </summary>
    member this.deleteUntitled =
        async {
            let! places =
                this.querySeqAsync<Place> () {
                    script
                        """select distinct P.id, P.url
                           from moz_places P
                             left join main.moz_bookmarks B on P.id = B.fk
                           where P.title is null
                             and B.id is null"""
                }

            let places = places |> toList
            let! _ = places |> map _.Id |> this.deletePlaceIds
            return places
        }

module Tests =
    open System
    open Expecto
    open Expecto.Logging

    let db =
        @"C:\Users\song\AppData\Roaming\Mozilla\Firefox\Profiles\4kcatvle.default-release\places.sqlite"
        |> sprintf "Data Source=%s;"
        |> Db

    [<Tests>]
    let specs =
        testList
            "FirefoxHistory"
            [ let logger = Log.createHiera [| "Firefox"; "History" |]
              let writeln = Message.eventX >> logger.info
              let printPlace = _.Url >> sprintf "%A" >> writeln

              testAsync "Delete untitled places" {
                  let! removed = db.deleteUntitled
                  removed |> map printPlace |> ignore
              }

              // theory data
              let garbageDomainTheoryData: string list =
                  [ "file:///"
                    "?__cf_chl_tk="
                    "?__cf_chl_rt_tk="
                    ".blob.core.windows.net"
                    ".cmstest.optimizely.com"
                    ".fbcdn.net"
                    ".giphy.com/media/"
                    ".googleusercontent.com"
                    ".logseq-db-demo.pages.dev"
                    ".logseq-db-test.pages.dev"
                    ".s3.amazonaws.com"
                    ".startpage.com/av/proxy?"
                    ".susercontent.com"
                    ".zdusercontent.com/attachment/"
                    "127.0.0.1"
                    "1tudien.com"
                    "5.vndic.net"
                    "account.proton.me/authorize?"
                    "accounts.fireant.vn"
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
                    "episerverex.qualtrics.com"
                    "eu.docusign.net"
                    "eu.onetimesecret.com/secret/"
                    "euc-powerpoint.officeapps.live.com"
                    "eur.delve.office.com/?"
                    "external-content.duckduckgo.com"
                    "fbtag.net"
                    "feedly.com/i/login?"
                    "forms.office.com"
                    "github.com/login"
                    "github.com/orgs"
                    "go.microsoft.com"
                    "hcm55.sapsf.eu"
                    "hoachau.vn/search"
                    "hvdic.thivien.net"
                    "i.sstatic.net"
                    "id.atlassian.com"
                    "id.zalo.me/account"
                    "info.hdbank.com.vn/subcriber"
                    "ipfs.io"
                    "jr.chat.zalo.me"
                    "jr.nhatkyzalo.vn"
                    "jr.zalo.cloud"
                    "jr.zaloapp.com"
                    "jr.zingmp3.vn"
                    "khachhang.prudential.com.vn"
                    "libgen.is"
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
                    "maps.app.goo.gl"
                    "mimecast.com"
                    "modyolo.com/download/"
                    "my.vnexpress.net/users/feed/"
                    "optimizely.litmos.com"
                    "optimizely.okta.com"
                    "paasportal.epimore.com/Error/"
                    "play.google.com"
                    "portal.azure.com/Error/"
                    "prep.home.optimizely.com/callback"
                    "prep.login.optimizely.com"
                    "prep.turnstile.episerver.net"
                    "prod.outgoing.prod.webservices.mozgcp.net"
                    "profiler.firefox.com"
                    "rambda.vercel.app/?"
                    "redirect.zalo.me"
                    "revanced.app/patches?"
                    "search.brave.com"
                    "searchfox.org/mozilla-central/rev/"
                    "shopee.vn/buyer/login"
                    "shopee.vn/find_similar_products?"
                    "shopee.vn/search"
                    "shopee.vn/verify"
                    "support.upwork.com"
                    "tc01.ep.se"
                    "teams.microsoft.com"
                    "tracking.email.azfin.vn"
                    "tracking.linkleads.com.vn"
                    "translate.goog/"
                    "translate.google.com"
                    "ttgearlab.com/page/"
                    "tudien.dolenglish.vn/"
                    "upwork-usw2-prod-agora-file-storage.s3.us-west-2.amazonaws.com"
                    "url.de.m.mimecastprotect.com"
                    "vi.m.wikipedia.org"
                    "vietstock.vn/tag"
                    "web.yammer.com"
                    "web-frameworks-benchmark.netlify.app/compare?"
                    "web-frameworks-benchmark.netlify.app/result?"
                    "winmart.vn/search/"
                    "world.optimizely.com/csclasslibraries"
                    "www.amazon.com/s/"
                    "www.amazon.com/s?"
                    "www.bahn.de/buchung/fahrplan/suche#"
                    "www.bing.com/search"
                    "www.booking.com"
                    "www.compositional-it.com/news-blog/page/"
                    "www.dailymotion.com/search/"
                    "www.dictionary.com/browse/"
                    "www.facebook.com/photo"
                    "www.facebook.com/reel"
                    "www.google.com/maps"
                    "www.guru.com/login.aspx?"
                    "www.ldoceonline.com/dictionary/"
                    "www.ldoceonline.com/spellcheck/"
                    "www.linkedin.com/404/"
                    "www.linkedin.com/authwall"
                    "www.linkedin.com/feed/"
                    "www.linkedin.com/login"
                    "www.linkedin.com/mynetwork/"
                    "www.linkedin.com/mypreferences/"
                    "www.linkedin.com/signup"
                    "www.linkedin.com/verify"
                    "www.merriam-webster.com/dictionary/"
                    "www.microsoft365.com/search/"
                    "www.openstreetmap.org/search"
                    "www.perplexity.ai/search/"
                    "www.quora.com/?"
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
                      let! removed = domain |> db.deletePlaces
                      removed |> map printPlace |> ignore
                  })

              // theory data
              let domainWithGarbageFragmentTheoryData: string list =
                  [ ".sh-daily.ep.se"
                    "analysiscenter.veracode.com"
                    "andrewlock.net"
                    "bongban.org"
                    "cheatsheetseries.owasp.org"
                    "chimviet.free.fr"
                    "community.chocolatey.org"
                    "confluence.sso.episerver.net"
                    "connect.mozilla.org"
                    "crontab.guru"
                    "csdiy.wiki"
                    "datatracker.ietf.org"
                    "devblogs.microsoft.com"
                    "developer.mozilla.org"
                    "docs.controld.com"
                    "docs.developers.optimizely.com"
                    "docs.google.com"
                    "docs.logseq.com"
                    "docs.onionshare.org"
                    "docs.syncthing.net"
                    "dontkillmyapp.com"
                    "duckduckgo.com"
                    "efsa.onlinelibrary.wiley.com"
                    "enlio.vn"
                    "episerver99.sharepoint.com"
                    "episerver99-my.sharepoint.com"
                    "eur-lex.europa.eu"
                    "examine.com"
                    "fdc.nal.usda.gov"
                    "fsharpforfunandprofit.com"
                    "github.com"
                    "github.io"
                    "hoachau.vn"
                    "hoanghamobile.com"
                    "jira.sso.episerver.net"
                    "jmeter.apache.org"
                    "kagi.com"
                    "keepass.info"
                    "kylinsaigon.com"
                    "learn.microsoft.com"
                    "learnyouahaskell.com"
                    "lemon.io"
                    "lifestyle.znews.vn"
                    "logseq-db-demo.pages.dev"
                    "logseq-db-test.pages.dev"
                    "lucid.app"
                    "mermaid.live"
                    "mullvad.net"
                    "my.nextdns.io"
                    "myapps.microsoft.com"
                    "mysignins.microsoft.com"
                    "nap.nationalacademies.org"
                    "naturesway.com"
                    "ncbi.nlm.nih.gov"
                    "nhandan.vn"
                    "nhathuocvietnhat.vn"
                    "nojaf.com"
                    "ods.od.nih.gov"
                    "optimizely.brightfunds.org"
                    "paasportal.episerver.net"
                    "pingsunday.com"
                    "portal.azure.com"
                    "privacyguides.org/en/"
                    "ramdajs.com/docs/"
                    "readthedocs.io"
                    "revancedmicrog.com"
                    "sanctuary.js.org"
                    "selfrefactor.github.io"
                    "shopee.vn"
                    "ss64.com"
                    "suckhoe123.vn"
                    "support.google.com"
                    "support.mozilla.org"
                    "support.optimizely.com"
                    "tabletennis-reference.com"
                    "thinkpro.vn"
                    "tienphong.vn"
                    "tridactyl.xyz"
                    "tsf.telegram.org"
                    "ttgearlab.com"
                    "tuoitre.vn"
                    "typesense.org"
                    "viendinhduong.vn"
                    "viethealthy.com"
                    "vietnamnet.vn"
                    "vnexpress.net"
                    "vorapis.pages.dev"
                    "voz.vn"
                    "webhook.site/"
                    "wikipedia.org/wiki/"
                    "www.24h.com.vn"
                    "www.dragoncapital.com.vn"
                    "www.google.com"
                    "www.health.com"
                    "www.healthline.com"
                    "www.jetbrains.com"
                    "www.medicalnewstoday.com"
                    "www.mermaidchart.com"
                    "www.msdmanuals.com"
                    "www.mybib.com"
                    "www.ncb-bank.vn"
                    "www.nuget.org"
                    "www.nutritionvalue.org"
                    "www.quad9.net"
                    "www.rfc-editor.org"
                    "www.sqlite.org"
                    "www.tabletennis11.com"
                    "www.tabletennisdaily.com"
                    "www.techempower.com"
                    "www.vcbf.com"
                    "www.voidtools.com"
                    "xunit.net"
                    "zen-browser.app"
                    "zoom.earth" ]

              testTheoryAsync "Given domain with garbage fragment" domainWithGarbageFragmentTheoryData (fun domain ->
                  async {
                      let placeFilter = Place.withFragment
                      let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                      removed |> map printPlace |> ignore
                  })

              // theory data
              let domainWithGarbagePathTheoryData: (string * string list) list =
                  [ "aas.com.vn", [ "/Content/Archive/FileAttact/" ]
                    "bongbanduyhung.com", [ "/page/" ]
                    "connect.mozilla.org", [ "/page/" ]
                    "episerver99-my.sharepoint.com", [ "AccessDenied.aspx?" ]
                    "episerver99.sharepoint.com", [ "download.aspx?"; "spfxsinglesignon.aspx" ]
                    "eur.delve.office.com", [ "/profileimage?" ]
                    "exercism.org", [ "/solutions" ]
                    "feedly.com", [ "/auth/"; "/i/discover/sources/search/" ]
                    "fireant.vn", [ "/charts/content/notifications/"; "/dashboard/content/notifications/" ]
                    "github.com", [ "/blob/"; "/commits/"; "/compare/"; "/releases/"; "/runs/"; "/tree/" ]
                    "github.com/advisories/", [ "dependabot?query=" ]
                    "jira.sso.episerver.net",
                    [ "/issue/CloneIssueProgress.jspa"
                      "/login.jsp"
                      "/secure/attachment/"
                      "/secure/ConvertIssue.jspa?"
                      "/secure/ConvertSubTask.jspa?" ]
                    "maybanbongban.vn", [ "/page/" ]
                    "opti-dxp.datadoghq.com", [ "/account/login"; "/apm" ]
                    "optimizely.atlassian.net/servicedesk/", [ "/user/login?destination=" ]
                    "outlook.office.com", [ "/groups/"; "/mail/"; "/owa" ]
                    "www.rockmods.net", [ "/?"; "/search/"; "/search?" ]
                    "ttgearlab.com", [ "/page/" ]
                    "us3.datadoghq.com", [ "/account/login"; "/apm"; "/dashboard"; "/logs?"; "/monitors"; "/services?" ]
                    "voz.vn",
                    [ "/account/alerts"
                      "/direct-messages/"
                      "/goto/post?"
                      "/p/"
                      "/page-"
                      "/search/"
                      "/u/" ]
                    "write.as", [ "/edit" ]
                    "www.reddit.com", [ "/comment/" ] ]

              testTheoryAsync
                  "Given domain with garbage path"
                  domainWithGarbagePathTheoryData
                  (fun (domain, substrings) ->
                      async {
                          let placeFilter = substrings |> map String.isSubString |> orF |> (>>) _.Url
                          let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                          removed |> map printPlace |> ignore
                      })

              // theory data
              let domainWithAnyGarbageQueryParamTheoryData: (string * string list) list =
                  [ ".giphy.com", [ "ep" ]
                    "accounts.fireant.vn", [ "client_id"; "redirect_uri"; "signin" ]
                    "accounts.spotify.com", [ "continue" ]
                    "accounts.x.ai", [ "redirect" ]
                    "addons.mozilla.org", [ "q"; "utm_source" ]
                    "apkdone.com", [ "s" ]
                    "apkpure.com", [ "q" ]
                    "app.optimizely.com/signin", [ "continue_to" ]
                    "asp-blogs.azurewebsites.net", [ "page" ]
                    "batdongsan.com.vn", [ "disIds"; "dtln"; "dtnn"; "gcn"; "gtn"; "tpl" ]
                    "benxehanoi.vn", [ "diem-den"; "diem-di" ]
                    "bongban.org", [ "page" ]
                    "bongbanduyhung.com", [ "s" ]
                    "books-search.typesense.org", [ "b%5Bquery%5D" ]
                    "butterflyaustralia.com", [ "variant" ]
                    "challenge.spotify.com", [ "flow_ctx" ]
                    "chatgpt.com", [ "callbackUrl"; "error"; "prompt"; "q" ]
                    "community.chocolatey.org", [ "q" ]
                    "confluence.sso.episerver.net", [ "draftId"; "preview"; "src"; "title" ]
                    "congtytui.info", [ "page" ]
                    "controld.com", [ "freeResolverType" ]
                    "cve.mitre.org", [ "keyword" ]
                    "dailongsport.vn", [ "page" ]
                    "dash.readme.com", [ "redirect" ]
                    "dev.to/", [ "state" ]
                    "developer.mozilla.org", [ "q" ]
                    "dichvucong.bocongan.gov.vn", [ "tukhoa" ]
                    "dichvucong.gov.vn", [ "group"; "pkeyWord" ]
                    "dict.laban.vn", [ "query" ]
                    "dictionary.zim.vn", [ "/anh-viet/" ]
                    "dienquang.com", [ "q" ]
                    "discord.com", [ "key"; "redirect_to" ]
                    "docs.controld.com", [ "ref" ]
                    "docs.google.com", [ "fbzx"; "ldf_id"; "usp" ]
                    "docs.syncthing.net", [ "version" ]
                    "dragonx.com.vn", [ "id"; "moe_app_id" ]
                    "drive.google.com", [ "ldf_id"; "q"; "resourcekey"; "url"; "usp" ]
                    "drive.usercontent.google.com", [ "export" ]
                    "duckduckgo.com", [ "q" ]
                    "dungcubongban.vn", [ "page" ]
                    "ecosystem.atlassian.net", [ "src" ]
                    "efsa.onlinelibrary.wiley.com", [ "file" ]
                    "en.wikipedia.org", [ "search"; "title" ]
                    "episerver99-my.sharepoint.com", [ "id" ]
                    "episerveridentity.b2clogin.com", [ "client_id" ]
                    "eur-lex.europa.eu", [ "uri" ]
                    "examine.com", [ "show_conditions" ]
                    "exercism.org", [ "status" ]
                    "fado.vn", [ "keywords" ]
                    "fastly.picsum.photos", [ "hmac" ]
                    "fdc.nal.usda.gov", [ "component"; "query"; "type" ]
                    "feedly.com", [ "code"; "gate" ]
                    "finance.yahoo.com", [ "period1" ]
                    "fireant.vn", [ "jskey" ]
                    "ftcshop.vn", [ "key" ]
                    "galaxystore.samsung.com", [ "langCd" ]
                    "gearshop.vn", [ "page" ]
                    "github.com",
                    [ "after"
                      "author"
                      "check_run_id"
                      "closed"
                      "filename"
                      "from"
                      "notice"
                      "page"
                      "q"
                      "query"
                      "return_to"
                      "tab" ]
                    "glycemic-index.net", [ "s" ]
                    "glycemicindex.com", [ "food_name" ]
                    "google.com", [ "authuser" ]
                    "grok.com", [ "tab" ]
                    "hanoian.com", [ "start" ]
                    "hanoinew.vn", [ "brand"; "filter" ]
                    "hanoiprofile.ep.se", [ "returnUrl" ]
                    "hika.fyi", [ "question"; "topic_id" ]
                    "hoachau.vn", [ "brand"; "page" ]
                    "hoangchopbongban.com", [ "q" ]
                    "hoanghamobile.com", [ "product_id"; "sku" ]
                    "hungmobile.vn", [ "search" ]
                    "identity.getpostman.com", [ "auth_challenge"; "redirect_uri" ]
                    "itviec.com", [ "click_source"; "job_selected"; "lab_feature"; "query" ]
                    "jira.sso.episerver.net",
                    [ "atlOrigin"
                      "devStatusDetailDialog"
                      "filter"
                      "jql"
                      "name"
                      "page"
                      "rapidView"
                      "returnTo"
                      "schema"
                      "searchString"
                      "selectedItem"
                      "selectPageId" ]
                    "kinhmatviettin.vn", [ "wpf_filter_thuong-hieu" ]
                    "learn.microsoft.com", [ "pivots"; "search"; "tabs"; "term"; "viewFallbackFrom" ]
                    "login.optimizely.com", [ "client_id"; "stateToken"; "stateTokenExternalId" ]
                    "login.taobao.com", [ "redirectURL" ]
                    "login.yahoo.com", [ ".done"; "done" ]
                    "luatvietnam.vn", [ "page" ]
                    "lucid.app",
                    [ "invitationId"
                      "product"
                      "redirect_url"
                      "referredProduct"
                      "returnUrlOverride" ]
                    "m.dict.laban.vn", [ "keyword" ]
                    "masothue.com", [ "q" ]
                    "mayanhcuhanoi.com", [ "query" ]
                    "modyolo.com", [ "s" ]
                    "monica.im", [ "id" ]
                    "mrotherguy.github.io", [ "file" ]
                    "music.youtube.com", [ "list"; "q" ]
                    "myaccount.google.com", [ "backUrl"; "continue"; "gar"; "rapt" ]
                    "myaccount.microsoft.com", [ "tenant" ]
                    "myapps.microsoft.com", [ "tenantId" ]
                    "mycroftproject.com/install.html", [ "id" ]
                    "mycroftproject.com/search-engines.html", [ "name" ]
                    "mysignins.microsoft.com", [ "tenant" ]
                    "na2.docusign.net", [ "a"; "ti" ]
                    "naturesway.com", [ "_pos"; "page" ]
                    "nguoiquansat.vn", [ "gidzl" ]
                    "nhathuoclongchau.com.vn", [ "s" ]
                    "nhathuocphuongchinh.com", [ "keyword" ]
                    "nhattao.com", [ "q" ]
                    "nodeflair.com", [ "page" ]
                    "nongsandungha.com", [ "attribute_pa_khu-vuc" ]
                    "nuget.optimizely.com", [ "q" ]
                    "old.reddit.com", [ "context" ]
                    "one.google.com", [ "g1_landing_page" ]
                    "ooakforum.com", [ "sid"; "start" ]
                    "open.spotify.com", [ "flow_ctx" ]
                    "opti-dxp.datadoghq.com", [ "compare_query_A"; "fromUser"; "p"; "query"; "search" ]
                    "opti-graph.datadoghq.com", [ "next"; "query" ]
                    "optimizely.atlassian.net/servicedesk/", [ "page"; "reporter"; "src"; "statuses"; "token" ]
                    "optimizely.brightfunds.org", [ "keywords"; "page"; "start_date" ]
                    "paasportal.epimore.com", [ "message"; "ReturnUrl" ]
                    "phobongban.vn", [ "filter_thuong-hieu" ]
                    "pico.vn", [ "property" ]
                    "piped.video", [ "search_query" ]
                    "policies.account.samsung.com", [ "appKey" ]
                    "portal.azure.com", [ "bundlingKind" ]
                    "prep.admincenter.optimizely.com", [ "code"; "login_hint"; "orgId"; "state" ]
                    "prep.home.optimizely.com", [ "idpId"; "login_hint"; "orgId" ]
                    "rethinkdns.com", [ "q" ]
                    "s.taobao.com", [ "q" ]
                    "searchfox.org", [ "q" ]
                    "securitylabs.veracode.com", [ "module" ]
                    "shopee.vn",
                    [ "cmtid"
                      "d_id"
                      "entryPoint"
                      "flow_source"
                      "is_from_login"
                      "keyword"
                      "orderIndex"
                      "page"
                      "searchKeyword"
                      "sp_atk"
                      "type"
                      "uls_trackid"
                      "xptdk" ]
                    "sso.tuoitre.vn", [ "redirectUrl" ]
                    "stackoverflow.com", [ "rq" ]
                    "subscene.com", [ "q" ]
                    "support.mozilla.org", [ "as" ]
                    "support.office.com", [ "authtype"; "context" ]
                    "support.optimizely.com", [ "return_to" ]
                    "tabletennis.guide", [ "query" ]
                    "tdeecalculator.net", [ "age" ]
                    "thinkpro.vn", [ "skuId"; "tinh-trang" ]
                    "tichsancophieu.azfin.vn", [ "ldf_id" ]
                    "tiemanhnhabap.gump.gg", [ "sid" ]
                    "tiki.vn", [ "q"; "spid" ]
                    "topdev.vn", [ "src" ]
                    "travel.b-europe.com", [ "traveltype" ]
                    "trello.com", [ "returnUrl" ]
                    "ttsport.vn", [ "page" ]
                    "tudien.dolenglish.vn", [ "q" ]
                    "usazu1sv-01pw.ep.se", [ "Error" ]
                    "vi.wikipedia.org", [ "search" ]
                    "viendinhduong.vn", [ "page"; "sort"; "txtSearch" ]
                    "viethealthy.com", [ "masp" ]
                    "vietmart.co", [ "t"; "trang" ]
                    "visa.vfsglobal.com", [ "q" ]
                    "vneconomy.vn", [ "trang" ]
                    "voz.party", [ "page" ]
                    "voz.vn", [ "page"; "prefix_id"; "show_only" ]
                    "web.analysiscenter.veracode.com", [ "code" ]
                    "webextension.org", [ "type"; "version" ]
                    "world.optimizely.com", [ "releaseNoteId" ]
                    "www.accuweather.com", [ "city"; "year" ]
                    "www.adidas.com", [ "q" ]
                    "www.airbnb.fr", [ "source_impression_id" ]
                    "www.amazon.com", [ "keywords"; "rh"; "th" ]
                    "www.amazon.fr", [ "field-keywords" ]
                    "www.apkmirror.com", [ "post_type" ]
                    "www.asics.com", [ "pid" ]
                    "www.bachhoaxanh.com", [ "key" ]
                    "www.bandovn.vn", [ "AspxAutoDetectCookieSupport" ]
                    "www.bing.com", [ "q" ]
                    "www.calculator.net", [ "cage" ]
                    "www.cfl.lu", [ "SearchDepartureExtId" ]
                    "www.cisa.gov", [ "search_api_fulltext" ]
                    "www.contra.de", [ "search" ]
                    "www.cpubenchmark.net", [ "id" ]
                    "www.csgt.vn", [ "LoaiXe" ]
                    "www.dict.cc", [ "s" ]
                    "www.dm.de", [ "brandName0"; "query" ]
                    "www.docosan.com", [ "keyword" ]
                    "www.donic.com", [ "order"; "p" ]
                    "www.dragoncapital.com.vn", [ "contentId"; "moe_app_id" ]
                    "www.facebook.com",
                    [ "__eep__"
                      "attachment_id"
                      "comment_id"
                      "ldf_id"
                      "locale"
                      "lsrc"
                      "message_id"
                      "notif_id"
                      "profile_id"
                      "q"
                      "rdid"
                      "sorting_setting"
                      "thread_id" ]
                    "www.freelancer.com", [ "search_keyword" ]
                    "www.google.com", [ "gsessionid"; "lsessionid"; "q"; "vsrid" ]
                    "www.google.de", [ "q" ]
                    "www.guru.com", [ "SearchUrl" ]
                    "www.idealo.de", [ "q" ]
                    "www.informatik.uni-leipzig.de", [ "word" ]
                    "www.jetbrains.com", [ "billing"; "keymap"; "top" ]
                    "www.logixpathchef.com", [ "fdcid" ]
                    "www.kaufland.de", [ "search_value" ]
                    "www.mermaidchart.com", [ "redirect" ]
                    "www.msdmanuals.com", [ "query"; "ruleredirectid" ]
                    "www.naturesway.com.au", [ "q" ]
                    "www.nhaccuatui.com", [ "st" ]
                    "www.npmjs.com", [ "activeTab"; "q" ]
                    "www.nuget.org", [ "q" ]
                    "www.nutritionvalue.org", [ "action"; "food_query"; "foods"; "register"; "size"; "tag" ]
                    "www.perplexity.ai/search/", [ "0"; "2"; "4"; "q" ]
                    "www.pharmacity.vn", [ "keyword" ]
                    "www.reddit.com", [ "chainedPosts"; "context" ]
                    "www.rockmods.net", [ "m" ]
                    "www.rossmann.de", [ "text"; "width" ]
                    "www.ruten.com.tw", [ "q"; "sort" ]
                    "www.sciencedirect.com", [ "via" ]
                    "www.sqlite.org", [ "q" ]
                    "www.startpage.com", [ "query" ]
                    "www.tabletennis11.com", [ "q" ]
                    "www.tabletennisdaily.com", [ "page"; "q" ]
                    "www.truecaller.com", [ "noei" ]
                    "www.upwork.com", [ "q" ]
                    "www.urbandictionary.com", [ "term" ]
                    "www.vietnamworks.com", [ "fromPage" ]
                    "www.viettablet.com", [ "product_old" ]
                    "www.vinmec.com", [ "link_type" ]
                    "www.vitaminhouse.vn", [ "variant" ]
                    "www.xing.com", [ "ijt"; "keywords"; "sc_o" ]
                    "www.xxl.se", [ "query" ]
                    "www.yazio.com", [ "q" ]
                    "www.youtube.com", [ "feature"; "index"; "pp"; "search_query"; "si"; "t"; "time_continue" ]
                    "x.com", [ "redirect_after_login" ]
                    "yasakatabletennis.com", [ "filter" ]
                    "zen-browser.app", [ "v" ] ]

              testTheoryAsync
                  "Given domain with any garbage query param"
                  domainWithAnyGarbageQueryParamTheoryData
                  (fun (domain, queryParams) ->
                      async {
                          let placeFilter = Place.hasAnyQueryParam queryParams
                          let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                          removed |> map printPlace |> ignore
                      })

              // theory data
              let domainWithGarbageFragmentParamTheoryData: (string * string) list =
                  [ "vietnamnet.vn", "vnn_source" ]

              testTheoryAsync
                  "Given domain with garbage fragment param"
                  domainWithGarbageFragmentParamTheoryData
                  (fun (domain, fragmentParam) ->
                      async {
                          let placeFilter = Place.hasFragmentParam fragmentParam
                          let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                          removed |> map printPlace |> ignore
                      })

              // theory data
              let domainWithGarbageRegexTheoryData: (string * string list) list =
                  [ "apkpure.com", [ "/[\\w-]+/" ]
                    "bacsytranvanphuc.com", [ "/page/\\d+/" ]
                    "batdongsan.com.vn", [ "/p\\d+$" ]
                    "bongban.fanciko.com", [ "/page/\\d+/" ]
                    "bongban.org", [ "/forums/.+?\\d+/page-\\d+"; "/threads/.+?/page-\\d+" ]
                    "community.windy.com", [ "/\\d+/.+?/\\d+" ]
                    "diendan.footballvn.net", [ "/threads/\\d+-[^/]+/page\\d+\\.html" ]
                    "dungbongban.com", [ "-page\\d+\\.html" ]
                    "github.com",
                    [ "/commit/\\w{40}"
                      "/milestone/\\d+"
                      "/pkgs/nuget/[\\w\\.]+/versions"
                      "/pkgs/nuget/[\\w\\.]+/\\d+"
                      "/pull/\\d+/$"
                      "/pull/\\d+/commits"
                      "/pull/\\d+/files" ]
                    "hanoiskyteam.vn", [ "/page/\\d+" ]
                    "hanotour.com.vn", [ "/page-\\d+" ]
                    "kinhmatviettin.vn", [ "/page/\\d+/" ]
                    "myphamxachtayduc.vn", [ "/page/\\d+/" ]
                    "mytabletennis.net", [ "_page\\d+\\.html" ]
                    "tabletennis-reference.com", [ "detail/\\d+/" ]
                    "thanglongkydao.com", [ "/threads/.+?/page\\d+" ]
                    "ttgearlab.com", [ "/comment-page-\\d+" ]
                    "vietnamnet.vn", [ "-page\\d+\\.html$" ]
                    "vnexpress.net", [ "-\\d+-p\\d+$" ]
                    "voz.party", [ "/d/\\d+-.+?/\\d+$" ]
                    "voz.vn", [ "\\.\\d+/reply"; "/unread$" ]
                    "www.apkmirror.com", [ "/apk/[\\w-]+/[\\w-]+/" ]
                    "www.compositional-it.com/news-blog/author/isaac/page/2/", [ "/page/\\d+/" ]
                    "www.nguyenkim.com", [ "/page-\\d+/" ]
                    "www.nuget.org", [ "/packages/[\\w\\.]+/\\d+\\.\\d+\\.\\d+" ]
                    "www.nutritionvalue.org", [ "_content_page_\\d+" ]
                    "www.otofun.net", [ "\\.\\d+/page-\\d+" ]
                    "www.tabletennisdaily.com", [ "/forum/topics/.+?\\d+/page-\\d+" ] ]

              testTheoryAsync
                  "Given domain with garbage regex"
                  domainWithGarbageRegexTheoryData
                  (fun (domain, patterns) ->
                      async {
                          let placeFilter = patterns |> map Regex.isMatch |> orF |> (>>) _.Url
                          let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                          removed |> map printPlace |> ignore
                      })

              // theory data
              let domainWithGarbageNotFirstThreadPostTheoryData: string list =
                  [ "community.e.foundation"
                    "community.spiceworks.com"
                    "discuss.logseq.com"
                    "discuss.privacyguides.net"
                    "discuss.techlore.tech"
                    "f247.com"
                    "forum.f-droid.org"
                    "forum.graphviz.org"
                    "forum.rescript-lang.org"
                    "forum.sublimetext.com"
                    "forum.uipath.com"
                    "forum.xojo.com"
                    "forums.fsharp.org" ]

              testTheoryAsync
                  "Given domain with garbage not first thread post"
                  domainWithGarbageNotFirstThreadPostTheoryData
                  (fun domain ->
                      async {
                          let placeFilter = Place.isNotFirstThreadPost
                          let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                          removed |> map printPlace |> ignore
                      })

              // theory data
              let domainWithComplexGarbagePlaceFilterTheoryData: (string * (Place -> bool)) list =
                  [ "local",
                    andF
                        [ _.Url >> Regex.isMatch ":\\d+/"
                          orF [ Place.withQueryParam; Place.withFragment ] ]
                    "localhost/", Place.withQueryParam
                    "nuget.optimizely.com", Place.hasQueryParams [ "id"; "v" ]
                    "opti-dxp.datadoghq.com/logs", Place.withQueryParam
                    "opti-dxp.datadoghq.com/monitors/", Place.withQueryParam
                    "world.taobao.com", Place.hasQueryParams [ "a"; "b" ]
                    "www.jetbrains.com/help/", Place.withQueryParam ]

              testTheoryAsync
                  "Given domain with complex garbage place filter to delete"
                  domainWithComplexGarbagePlaceFilterTheoryData
                  (fun (domain, placeFilter) ->
                      async {
                          let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                          removed |> map printPlace |> ignore
                      })

              // history entries, excluding bookmarks
              let urlParts =
                  db.querySeqAsync<string> () {
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
                  urlParts
                  |> List.countBy _.Authority
                  |> filter (fun (_, count) -> count >= 5)
                  |> sortByDescending snd
                  |> map fst
                  |> (sprintf "%A" >> writeln)
              }

              test "Count by authority and first path's segment, count >= 5" {
                  urlParts
                  |> List.countBy UrlPart.toString
                  |> filter (fun (_, count) -> count >= 5)
                  |> sortByDescending snd
                  |> map fst
                  |> (sprintf "%A" >> writeln)
              } ]
