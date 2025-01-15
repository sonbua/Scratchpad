module Firefox.History

// #r "nuget: FSharp.Data.Dapper"
// #r "nuget: FSharpPlus"
// #r "nuget: Microsoft.Data.SQLite"

[<AutoOpen>]
module Foldable =
    open FSharpPlus

    let forallF fs arg =
        fs |> forall (fun f -> f arg)

module Regex =
    open System.Text.RegularExpressions

    /// <summary>
    /// Indicates whether the specified regular expression finds a match in the specified input string.
    /// </summary>
    /// <param name="pattern">The regular expression pattern to match.</param>
    /// <param name="text">The string to search for a match.</param>
    let isMatch pattern (text: string) = Regex.IsMatch(text, pattern)

module Uri =
    open System
    open FSharpPlus

    let hasQueryParam q (uri: Uri) =
        uri.Query
        |> String.trimStart [ '?' ]
        |> _.Split('&', StringSplitOptions.RemoveEmptyEntries)
        |> map (String.split [ "=" ] >> head)
        |> Array.contains q

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
type Place =
    { Id: int
      Url: string }

module Place =
    open System
    open FSharpPlus

    let hasQueryParam q place = place.Url |> Uri |> Uri.hasQueryParam q

    let hasFragmentParam f place = place.Url |> Uri |> Uri.hasFragmentParam f

    let withFragment = _.Url >> String.isSubString "#"

    let isNotFirstThreadPost = _.Url >> Regex.isMatch "/t/.+?/\\d+/\\d+"

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
        @"C:\Users\song\AppData\Roaming\Mozilla\Firefox\Profiles\4kcatvle.default-release\places.sqlite"

    let connectionString = $"Data Source={db};"
    let connectionF = ConnectionFactory.create connectionString

    let querySeqAsync<'R> = querySeqAsync<'R> connectionF
    let querySingleAsync<'R> = querySingleAsync<'R> connectionF
    let querySingleOptionAsync<'R> = querySingleOptionAsync<'R> connectionF

    [<Tests>]
    let specs =
        testList "FirefoxHistory"
            [ let logger = Log.createHiera [| "Firefox"; "History" |]
              let writeln = Message.eventX >> logger.info

              let deletePlaceIds = Db.deletePlaceIds querySeqAsync

              testAsync "Delete untitled places" {
                  let! removed = Db.deleteUntitled querySeqAsync deletePlaceIds
                  removed |> map (_.Url >> sprintf "%A" >> writeln) |> ignore
              }

              // theory data
              let garbageDomainTheoryData =
                  [ "file:///"
                    "?__cf_chl_tk="
                    "?__cf_chl_rt_tk="
                    ".cmstest.optimizely.com"
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
                    "developer.mozilla.org/en-US/search?q="
                    "dichvucong.dancuquocgia.gov.vn/portal"
                    "dictionary.cambridge.org"
                    "docs.google.com/accounts"
                    "download.library.lol"
                    "engage.cloud.microsoft/main"
                    "episerver99.sharepoint.com/sites/"
                    "eu.docusign.net"
                    "euc-powerpoint.officeapps.live.com"
                    "eur.delve.office.com/?"
                    "fbtag.net"
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
                    "jira.sso.episerver.net/projects"
                    "jira.sso.episerver.net/secure"
                    "jr.chat.zalo.me"
                    "jr.nhatkyzalo.vn"
                    "jr.zalo.cloud"
                    "jr.zaloapp.com"
                    "jr.zingmp3.vn"
                    "khachhang.prudential.com.vn"
                    "libgen.is"
                    "www.linkedin.com/authwall"
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
                    "modyolo.com/download/"
                    "mysignins.microsoft.com/#"
                    "nodeflair.com/salaries?page="
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
                    "rethinkdns.com/search?q="
                    "revanced.app/patches?"
                    "sanctuary.js.org/#"
                    "search.brave.com"
                    "searchfox.org/mozilla-central/rev/"
                    "searchfox.org/mozilla-central/search?q="
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
                    "voz.vn/goto/post?"
                    "voz.vn/p/"
                    "voz.vn/search/"
                    "voz.vn/u/"
                    "web.yammer.com"
                    "web-frameworks-benchmark.netlify.app/compare?"
                    "web-frameworks-benchmark.netlify.app/result?"
                    "world.optimizely.com/csclasslibraries"
                    "www.apkmirror.com/?post_type="
                    "www.amazon.com/s/"
                    "www.amazon.com/s?"
                    "www.bahn.de/buchung/fahrplan/suche#"
                    "www.bing.com/search"
                    "www.booking.com"
                    "www.dict.cc/?s="
                    "www.facebook.com/photo"
                    "www.facebook.com/reel"
                    "www.google.com/maps"
                    "www.ldoceonline.com/dictionary/"
                    "www.ldoceonline.com/spellcheck/"
                    "www.linkedin.com/404/"
                    "www.linkedin.com/feed/"
                    "www.linkedin.com/login"
                    "www.linkedin.com/mynetwork/"
                    "www.linkedin.com/mypreferences/"
                    "www.linkedin.com/verify"
                    "www.microsoft365.com/search/"
                    "www.nuget.org/packages?q="
                    "www.openstreetmap.org/search"
                    "www.quora.com/?"
                    "www.rockmods.net/?"
                    "www.rockmods.net/search/"
                    "www.rockmods.net/search?"
                    "www.virustotal.com"
                    "www.windy.com"
                    "www.youtube.com/results"
                    "xacthuc.dichvucong.gov.vn"
                    "you.com/search"
                    "youtu.be"
                    "zdusercontent.com" ]

              testTheoryAsync "Given garbage domain" garbageDomainTheoryData (fun domain ->
                  async {
                      let! removed = domain |> Db.deletePlaces querySeqAsync deletePlaceIds
                      removed |> map (_.Url >> sprintf "%A" >> writeln) |> ignore
                  })

              // theory data
              let domainWithGarbagePlaceFilterTheoryData: (string * (Place -> bool)) list =
                  [ "addons.mozilla.org", Place.hasQueryParam "q"
                    "addons.mozilla.org", Place.hasQueryParam "utm_source"
                    "analysiscenter.veracode.com", Place.withFragment
                    "andrewlock.net", Place.withFragment
                    "apkdone.com", Place.hasQueryParam "s"
                    "apkpure.com", Place.hasQueryParam "q"
                    "apkpure.com", _.Url >> Regex.isMatch "/[\\w-]+/"
                    "app.optimizely.com/signin", Place.hasQueryParam "continue_to"
                    "asp-blogs.azurewebsites.net", Place.hasQueryParam "page"
                    "batdongsan.com.vn", Place.hasQueryParam "disIds"
                    "batdongsan.com.vn", Place.hasQueryParam "dtln"
                    "batdongsan.com.vn", Place.hasQueryParam "dtnn"
                    "batdongsan.com.vn", Place.hasQueryParam "gcn"
                    "batdongsan.com.vn", Place.hasQueryParam "gtn"
                    "bongban.org", Place.withFragment
                    "bongban.org", Place.hasQueryParam "page"
                    "bongban.org", _.Url >> Regex.isMatch "/forums/.+?\\d+/page-\\d+"
                    "bongban.org", _.Url >> Regex.isMatch "/threads/.+?/page-\\d+"
                    "cheatsheetseries.owasp.org", Place.withFragment
                    "community.chocolatey.org", Place.withFragment
                    "community.chocolatey.org", Place.hasQueryParam "q"
                    "community.e.foundation", Place.isNotFirstThreadPost
                    "community.windy.com", _.Url >> Regex.isMatch "/\\d+/.+?/\\d+"
                    "confluence.sso.episerver.net", Place.hasQueryParam "src"
                    "confluence.sso.episerver.net", Place.hasQueryParam "preview"
                    "csdiy.wiki", Place.withFragment
                    "dailongsport.vn", Place.hasQueryParam "page"
                    "datatracker.ietf.org", Place.withFragment
                    "devblogs.microsoft.com", Place.withFragment
                    "diendan.footballvn.net", _.Url >> Regex.isMatch "/threads/\\d+-[^/]+/page\\d+\\.html"
                    "discuss.logseq.com", Place.isNotFirstThreadPost
                    "discuss.privacyguides.net", Place.isNotFirstThreadPost
                    "drive.google.com", Place.hasQueryParam "usp"
                    "duckduckgo.com", Place.withFragment
                    "duckduckgo.com", Place.hasQueryParam "q"
                    "dungbongban.com", _.Url >> Regex.isMatch "-page\\d+\\.html"
                    "dungcubongban.vn", Place.hasQueryParam "page"
                    "en.wikipedia.org", Place.hasQueryParam "search"
                    "episerver99.sharepoint.com", Place.withFragment
                    "episerver99.sharepoint.com", _.Url >> String.isSubString "download.aspx?"
                    "episerver99.sharepoint.com", _.Url >> String.isSubString "spfxsinglesignon.aspx"
                    "episerveridentity.b2clogin.com", _.Url >> String.isSubString "/authorize?client_id="
                    "eur.delve.office.com", _.Url >> String.isSubString "/profileimage?"
                    "exercism.org", _.Url >> String.isSubString "/solutions"
                    "f247.com", Place.isNotFirstThreadPost
                    "forum.uipath.com", Place.isNotFirstThreadPost
                    "forum.rescript-lang.org", Place.isNotFirstThreadPost
                    "forums.fsharp.org", Place.isNotFirstThreadPost
                    "github.com", Place.withFragment
                    "github.com", Place.hasQueryParam "check_run_id"
                    "github.com", Place.hasQueryParam "from"
                    "github.com", Place.hasQueryParam "page"
                    "github.com", Place.hasQueryParam "q"
                    "github.com", Place.hasQueryParam "query"
                    "github.com", Place.hasQueryParam "tab"
                    "github.com", _.Url >> String.isSubString "/blob/"
                    "github.com", _.Url >> String.isSubString "/commits/"
                    "github.com", _.Url >> String.isSubString "/compare/"
                    "github.com", _.Url >> String.isSubString "/releases/"
                    "github.com", _.Url >> String.isSubString "/runs/"
                    "github.com", _.Url >> String.isSubString "/tree/"
                    "github.com", _.Url >> String.isSubString "#issue"
                    "github.com", _.Url >> Regex.isMatch "/commit/\\w{40}"
                    "github.com", _.Url >> Regex.isMatch "/issues/\\d+#"
                    "github.com", _.Url >> Regex.isMatch "/pull/\\d+#"
                    "github.com", _.Url >> Regex.isMatch "/pull/\\d+/commits"
                    "github.com", _.Url >> Regex.isMatch "/pull/\\d+/files"
                    "github.com", forallF [ _.Url >> String.isSubString "/tags"; Place.hasQueryParam "after" ]
                    "github.com/advisories/", _.Url >> String.isSubString "dependabot?query="
                    "github.io", Place.withFragment
                    "hanoian.com", Place.hasQueryParam "start"
                    "hanoinew.vn", Place.hasQueryParam "filter"
                    "hika.fyi", Place.hasQueryParam "question"
                    "hika.fyi", Place.hasQueryParam "topic_id"
                    "hoachau.vn", Place.withFragment
                    "hoachau.vn", Place.hasQueryParam "brand"
                    "hoachau.vn", Place.hasQueryParam "page"
                    "hoangchopbongban.com", Place.hasQueryParam "q"
                    "jira.sso.episerver.net", Place.withFragment
                    "jira.sso.episerver.net", Place.hasQueryParam "atlOrigin"
                    "jira.sso.episerver.net", Place.hasQueryParam "devStatusDetailDialog"
                    "jira.sso.episerver.net", Place.hasQueryParam "jql"
                    "jira.sso.episerver.net/browse/", Place.hasQueryParam "page"
                    "learn.microsoft.com", Place.withFragment
                    "learn.microsoft.com", Place.hasQueryParam "search"
                    "learn.microsoft.com", Place.hasQueryParam "tabs"
                    "learn.microsoft.com", Place.hasQueryParam "terms"
                    "learnyouahaskell.com", Place.withFragment
                    "localhost", Place.withFragment
                    "localhost", Place.hasQueryParam "code"
                    "login.optimizely.com", _.Url >> String.isSubString "/authorize?client_id="
                    "login.taobao.com", Place.hasQueryParam "redirectURL"
                    "logseq-db-demo.pages.dev", _.Url >> String.isSubString "/#/"
                    "lucid.app", Place.withFragment
                    "lucid.app", Place.hasQueryParam "invitationId"
                    "lucid.app", Place.hasQueryParam "product"
                    "lucid.app", Place.hasQueryParam "redirect_url"
                    "masothue.com", Place.hasQueryParam "q"
                    "media4.giphy.com", Place.hasQueryParam "ep"
                    "modyolo.com", Place.hasQueryParam "s"
                    "mullvad.net", Place.withFragment
                    "mycroftproject.com/install.html", Place.hasQueryParam "id"
                    "mycroftproject.com/search-engines.html", Place.hasQueryParam "name"
                    "mytabletennis.net", _.Url >> Regex.isMatch "_page\\d+\\.html"
                    "nhattao.com", Place.hasQueryParam "q"
                    "nojaf.com", Place.withFragment
                    "nuget.optimizely.com", Place.hasQueryParam "q"
                    "nuget.optimizely.com", forallF [ Place.hasQueryParam "id"; Place.hasQueryParam "v" ]
                    "optimizely.atlassian.net/servicedesk/", Place.hasQueryParam "token"
                    "optimizely.atlassian.net/servicedesk/", _.Url >> String.isSubString "/user/login?destination="
                    "phobongban.vn", Place.hasQueryParam "filter_thuong-hieu"
                    "pingsunday.com", Place.withFragment
                    "piped.video", Place.hasQueryParam "search_query"
                    "portal.azure.com", Place.withFragment
                    "privacyguides.org/en/", Place.withFragment
                    "readthedocs.io", Place.withFragment
                    "s.taobao.com", Place.hasQueryParam "q"
                    "shopee.vn", Place.withFragment
                    "shopee.vn", Place.hasQueryParam "cmtid"
                    "shopee.vn", Place.hasQueryParam "entryPoint"
                    "shopee.vn", Place.hasQueryParam "page"
                    "shopee.vn", Place.hasQueryParam "searchKeyword"
                    "shopee.vn", Place.hasQueryParam "sp_atk"
                    "ss64.com", Place.withFragment
                    "support.optimizely.com", Place.withFragment
                    "support.optimizely.com", Place.hasQueryParam "return_to"
                    "thanglongkydao.com", _.Url >> Regex.isMatch "/threads/.+?/page\\d+"
                    "thinkpro.vn", Place.withFragment
                    "thinkpro.vn", Place.hasQueryParam "skuId"
                    "thinkpro.vn", Place.hasQueryParam "tinh-trang"
                    "tiemanhnhabap.gump.gg", Place.hasQueryParam "sid"
                    "tienphong.vn", Place.withFragment
                    "tiki.vn", Place.hasQueryParam "q"
                    "tridactyl.xyz", Place.withFragment
                    "ttgearlab.com", Place.withFragment
                    "ttsport.vn", Place.hasQueryParam "page"
                    "vietnamnet.vn", Place.hasFragmentParam "vnn_source"
                    "vneconomy.vn", Place.hasQueryParam "trang"
                    "vnexpress.net", Place.hasFragmentParam "vn_source"
                    "voz.vn", Place.withFragment
                    "voz.vn", Place.hasQueryParam "page"
                    "voz.vn", Place.hasQueryParam "prefix_id"
                    "voz.vn", Place.hasQueryParam "show_only"
                    "voz.vn", _.Url >> String.isSubString "/page-"
                    "voz.vn", _.Url >> String.isSubString "#post-"
                    "voz.vn", _.Url >> Regex.isMatch "\\.\\d+/reply"
                    "voz.vn", _.Url >> Regex.isMatch "/unread$"
                    "wikipedia.org/wiki/", Place.withFragment
                    "world.taobao.com", forallF [ Place.hasQueryParam "a"; Place.hasQueryParam "b" ]
                    "write.as", _.Url >> String.isSubString "/edit"
                    "www.adidas.com", Place.hasQueryParam "q"
                    "www.amazon.com", Place.hasQueryParam "keywords"
                    "www.amazon.com", Place.hasQueryParam "rh"
                    "www.amazon.fr", Place.hasQueryParam "field-keywords"
                    "www.apkmirror.com", _.Url >> Regex.isMatch "/apk/[\\w-]+/[\\w-]+/"
                    "www.contra.de", Place.hasQueryParam "search"
                    "www.cpubenchmark.net", Place.hasQueryParam "id"
                    "www.donic.com", Place.hasQueryParam "order"
                    "www.donic.com", Place.hasQueryParam "p"
                    "www.facebook.com", Place.hasQueryParam "rdid"
                    "www.freelancer.com", Place.hasQueryParam "search_keyword"
                    "www.google.com", Place.withFragment
                    "www.google.com", Place.hasQueryParam "q"
                    "www.nhaccuatui.com", Place.hasQueryParam "st"
                    "www.npmjs.com", Place.hasQueryParam "activeTab"
                    "www.npmjs.com", Place.hasQueryParam "q"
                    "www.nuget.org", Place.withFragment
                    "www.nuget.org", _.Url >> Regex.isMatch "/packages/[\\w\\.]+/\\d+\\.\\d+\\.\\d+"
                    "www.otofun.net", _.Url >> Regex.isMatch "\\.\\d+/page-\\d+"
                    "www.reddit.com", Place.hasQueryParam "chainedPosts"
                    "www.reddit.com", _.Url >> String.isSubString "/comment/"
                    "www.ruten.com.tw", Place.hasQueryParam "q"
                    "www.ruten.com.tw", Place.hasQueryParam "sort"
                    "www.tabletennis11.com", Place.withFragment
                    "www.tabletennis11.com", Place.hasQueryParam "q"
                    "www.tabletennisdaily.com", _.Url >> Regex.isMatch "/forum/topics/.+?\\d+/page-\\d+"
                    "www.techempower.com", Place.withFragment
                    "www.voidtools.com", Place.withFragment
                    "www.xxl.se", Place.hasQueryParam "query"
                    "www.youtube.com", Place.hasQueryParam "index"
                    "www.youtube.com", Place.hasQueryParam "t"
                    "yasakatabletennis.com", Place.hasQueryParam "filter" ]

              testTheoryAsync
                  "Given domain with garbage place filter to delete"
                  domainWithGarbagePlaceFilterTheoryData
                  (fun (domain, placeFilter) ->
                      async {
                          let! removed = (domain, placeFilter) ||> Db.deletePlacesWith querySeqAsync deletePlaceIds
                          removed |> map (_.Url >> sprintf "%A" >> writeln) |> ignore
                      })

              let urlParts =
                  querySeqAsync<string> { script """select url from moz_places""" }
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
