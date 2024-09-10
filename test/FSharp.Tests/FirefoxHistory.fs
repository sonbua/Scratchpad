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

    let hasQuery q (uri: Uri) =
        uri.Query
        |> String.trimStart [ '?' ]
        |> _.Split('&', StringSplitOptions.RemoveEmptyEntries)
        |> map (String.split [ "=" ] >> head)
        |> Array.contains q

    let hasFragment f (uri: Uri) =
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
      Url: string
      Title: string option }

module Place =
    open System
    open FSharpPlus

    let hasQuery q place = place.Url |> Uri |> Uri.hasQuery q

    let hasFragment f place = place.Url |> Uri |> Uri.hasFragment f

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
    /// The place filter will be applied in-memory, and not on the database.
    /// This does not delete place records, which are bookmarks.
    /// </summary>
    /// <param name="querySeqAsync">QuerySeqAsyncBuilder.</param>
    /// <param name="deletePlaceIds">Query to delete places IDs.</param>
    /// <param name="urlPart">Part of the URL to match.</param>
    /// <param name="placeFilter">The place filter, which will be applied in-memory, and not on the database.</param>
    let deletePlacesWith (querySeqAsync: QuerySeqAsyncBuilder<Place>) deletePlaceIds urlPart placeFilter =
        async {
            let! places =
                querySeqAsync {
                    script
                        """select distinct P.id, P.url, P.title
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
                    "127.0.0.1"
                    "1tudien.com"
                    "accounts.firefox.com"
                    "accounts.google.com"
                    "apkdone.com/?s="
                    "app.datadoghq.com/account/login"
                    "app.datadoghq.com/apm"
                    "app.datadoghq.com/dashboard"
                    "app.datadoghq.com/logs?"
                    "app.navan.com"
                    "app.opsgenie.com"
                    "bonus.ly"
                    "calendar.google.com"
                    "claude.ai/chat"
                    "claude.ai/login"
                    "claude.ai/magic-link"
                    "community.upwork.com"
                    "contacts.google.com/label"
                    "contacts.google.com/person"
                    "contacts.google.com/search"
                    "developer.mozilla.org/en-US/search?q="
                    "dichvucong.dancuquocgia.gov.vn/portal"
                    "docs.google.com/accounts"
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
                    "id.atlassian.com"
                    "id.zalo.me/account"
                    "info.hdbank.com.vn/subcriber"
                    "ipfs.io"
                    "jira.sso.episerver.net/issues/?jql="
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
                    "localhost:50592"
                    "login.microsoftonline.com"
                    "lucid.app/users/registerOrLogin/"
                    "ludwig.guru/s/"
                    "m.cafebiz.vn"
                    "m.cafef.vn"
                    "m.genk.vn"
                    "m.youtube.com"
                    "mail.google.com"
                    "mail.proton.me/login"
                    "mail.proton.me/u"
                    "modyolo.com/?s="
                    "mysignins.microsoft.com/#"
                    "nodeflair.com/salaries?page="
                    "optimizely.litmos.com"
                    "optimizely.okta.com"
                    "outlook.office.com/mail/?"
                    "outlook.office.com/mail/id"
                    "outlook.office.com/mail/inbox"
                    "outlook.office.com/owa"
                    "play.google.com"
                    "prod.outgoing.prod.webservices.mozgcp.net"
                    "profiler.firefox.com"
                    "redirect.zalo.me"
                    "rethinkdns.com/search?q="
                    "revanced.app/patches?"
                    "sanctuary.js.org/#"
                    "search.brave.com"
                    "searchfox.org/mozilla-central/rev/"
                    "searchfox.org/mozilla-central/search?q="
                    "shopee.vn/buyer/login"
                    "shopee.vn/search"
                    "shopee.vn/verify"
                    "support.upwork.com"
                    "tc01.ep.se"
                    "teams.microsoft.com"
                    "translate.google.com"
                    "upwork-usw2-prod-agora-file-storage.s3.us-west-2.amazonaws.com"
                    "url.de.m.mimecastprotect.com"
                    "vi.m.wikipedia.org"
                    "vietstock.vn/tag"
                    "voz.vn/p"
                    "voz.vn/u"
                    "web.yammer.com"
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
                  [ "addons.mozilla.org", Place.hasQuery "q"
                    "addons.mozilla.org", Place.hasQuery "utm_source"
                    "asp-blogs.azurewebsites.net", Place.hasQuery "page"
                    "cheatsheetseries.owasp.org", Place.withFragment
                    "community.chocolatey.org", Place.withFragment
                    "community.e.foundation", Place.isNotFirstThreadPost
                    "community.windy.com", _.Url >> Regex.isMatch "/\\d+/.+?/\\d+"
                    "discuss.logseq.com", Place.isNotFirstThreadPost
                    "episerver99.sharepoint.com", Place.withFragment
                    "episerver99.sharepoint.com", _.Url >> String.isSubString "download.aspx?"
                    "episerver99.sharepoint.com", _.Url >> String.isSubString "spfxsinglesignon.aspx"
                    "episerveridentity.b2clogin.com", _.Url >> String.isSubString "/authorize?client_id="
                    "f247.com", Place.isNotFirstThreadPost
                    "forum.uipath.com", Place.isNotFirstThreadPost
                    "forums.fsharp.org", Place.isNotFirstThreadPost
                    "github.com", Place.withFragment
                    "github.com", Place.hasQuery "q"
                    "github.com", Place.hasQuery "query"
                    "github.com", Place.hasQuery "tab"
                    "github.com", _.Url >> String.isSubString "/blob/"
                    "github.com", _.Url >> String.isSubString "/compare/"
                    "github.com", _.Url >> String.isSubString "/releases/"
                    "github.com", _.Url >> String.isSubString "/runs/"
                    "github.com", _.Url >> String.isSubString "/tree/"
                    "github.com", _.Url >> String.isSubString "#issue"
                    "github.com", _.Url >> Regex.isMatch "/issues/\\d+#"
                    "github.com", _.Url >> Regex.isMatch "/pull/\\d+#"
                    "github.com", _.Url >> Regex.isMatch "/pull/\\d+/commits/"
                    "github.com", _.Url >> Regex.isMatch "/pull/\\d+/files$"
                    "github.com/advisories/", _.Url >> String.isSubString "dependabot?query="
                    "github.io", Place.withFragment
                    "learn.microsoft.com", Place.withFragment
                    "login.optimizely.com", _.Url >> String.isSubString "/authorize?client_id="
                    "masothue.com", Place.hasQuery "q"
                    "nojaf.com", Place.withFragment
                    "nuget.optimizely.com", Place.hasQuery "q"
                    "nuget.optimizely.com", forallF [ Place.hasQuery "id"; Place.hasQuery "v" ]
                    "optimizely.atlassian.net/servicedesk/", Place.hasQuery "token"
                    "optimizely.atlassian.net/servicedesk/", _.Url >> String.isSubString "/user/login?destination="
                    "privacyguides.org/en/", Place.withFragment
                    "readthedocs.io", Place.withFragment
                    "ss64.com", Place.withFragment
                    "support.optimizely.com", Place.hasQuery "return_to"
                    "tienphong.vn", Place.withFragment
                    "tiki.vn", Place.hasQuery "q"
                    "vi.wikipedia.org/wiki/", Place.withFragment
                    "vietnamnet.vn", Place.hasFragment "vnn_source"
                    "vnexpress.net", Place.hasFragment "vn_source"
                    "voz.vn", _.Url >> String.isSubString "/page-"
                    "voz.vn", _.Url >> String.isSubString "#post-"
                    "world.optimizely.com/csclasslibraries", Place.withFragment
                    "write.as", _.Url >> String.isSubString "/edit"
                    "www.freelancer.com", Place.hasQuery "search_keyword"
                    "www.google.com", Place.hasQuery "q"
                    "www.npmjs.com", Place.hasQuery "activeTab"
                    "www.npmjs.com", Place.hasQuery "q"
                    "www.otofun.net", _.Url >> Regex.isMatch "\\.\\d+/page-\\d+"
                    "www.voidtools.com", Place.withFragment ]

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
