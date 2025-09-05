module Firefox.History

open FSharpPlus

type UrlPart =
    { Authority: string
      FirstSegment: string option }

module UrlPart =
    let private firstSegment path =
        match path with
        | null
        | ""
        | "/" -> None
        | path -> path |> String.splitCharWithRemovingEmptyEntries [| '/' |] |> tryHead

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
    let hasQueryParams qs place : bool =
        place.Url |> Uri |> Uri.hasQueryParams qs

    let hasAnyQueryParam qs place : bool =
        place.Url |> Uri |> Uri.hasAnyQueryParam qs

    let withQueryParam: Place -> bool = _.Url >> Uri >> Uri.withQueryParam

    let hasAnyFragmentParam fs place : bool =
        place.Url |> Uri |> Uri.hasAnyFragmentParam fs

    let withFragment: Place -> bool = _.Url >> String.isSubString "#"

    let isNotFirstThreadPost: Place -> bool = _.Url >> Regex.isMatch "/t/.+?/\\d+/\\d+"

open FSharp.Data.Dapper

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
    member this.deletePlacesWith (urlPart: string) placeFilter =
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
    member this.deletePlaces(urlPart: string) =
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
