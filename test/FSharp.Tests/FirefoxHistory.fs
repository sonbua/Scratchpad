module Firefox.History

open System
open FSharp.Data
open FSharpPlus

type UrlPart =
    { Authority: string
      PathSegments: string list }

module UrlPart =
    let private toSegments path =
        match path with
        | null
        | ""
        | "/" -> []
        | path ->
            path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
            |> toList

    let create (uri: Uri): UrlPart =
        { Authority = uri.Authority
          PathSegments = uri.LocalPath |> toSegments }

    let cutoffToFirstPathSegment urlPart =
        match urlPart.PathSegments with
        | [] -> urlPart.Authority
        | [ x ]
        | x :: _ -> $"{urlPart.Authority}/{x}"

[<Literal>]
let source = "TestData\\moz_places.json"

type Places = JsonProvider<source>


open Expecto
open Expecto.Logging

[<Tests>]
let specs =
    testList "FirefoxHistory" [
        let logger = Log.createHiera [| "Firefox"; "History" |]
        let writeln = Message.eventX >> logger.info

        let urlParts =
            Places.GetSamples()
            |> map (fun x -> x.Url |> Uri |> UrlPart.create)
            |> toList

        test "Count by authority, count >= 5" {
            urlParts
            |> List.countBy _.Authority
            |> filter (fun (_, count) -> count >= 5)
            |> sortByDescending snd
            |> map fst

            // Side effect
            |> (sprintf "%A" >> writeln)
        }

        test "Count by authority and first path's segment, count >= 5" {
            urlParts
            |> List.countBy UrlPart.cutoffToFirstPathSegment
            |> filter (fun (_, count) -> count >= 5)
            |> sortByDescending snd
            |> map fst

            // Side effect
            |> (sprintf "%A" >> writeln)
        } ]
