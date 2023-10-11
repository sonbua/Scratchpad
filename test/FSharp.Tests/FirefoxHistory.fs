namespace Firefox

open System
open FsUnit
open FSharpPlus
open FSharp.Data
open Xunit
open Xunit.Abstractions

module History =
    type UrlPart =
        { Authority: string
          PathSegments: string list }

    module UrlPart =
        type private Create = Uri -> UrlPart

        let create: Create =
            fun uri ->
                let toSegments path =
                    match path with
                    | null
                    | ""
                    | "/" -> []
                    | path -> path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> toList

                { Authority = uri.Authority
                  PathSegments = uri.LocalPath |> toSegments }

    [<Literal>]
    let source = "TestData\\moz_places.json"

    type Places = JsonProvider<source>

    let urlParts =
        Places.GetSamples()
        |> sortByDescending (fun x -> x.LastVisitDate |> Option.defaultValue 0L)
        |> map (fun x -> x.Url |> Uri |> UrlPart.create)
        |> toList

    type Reports(console: ITestOutputHelper) =
        let writeln = console.WriteLine

        [<Fact>]
        member _.``Count by authority, count >= 5``() =
            urlParts
            |> List.countBy (fun x -> x.Authority)
            |> filter (fun (_, count) -> count >= 5)
            |> map fst

            // side-effect
            |> iter writeln

        [<Fact>]
        member _.``Count by authority and first path's segment, count >= 5``() =
            let firstPathSegment (segments: string list) =
                match segments with
                | [] -> ""
                | [ x ]
                | x :: _ -> $"/{x}"

            let authorityAndFirstPathSegment urlPart =
                $"{urlPart.Authority}{urlPart.PathSegments |> firstPathSegment}"

            urlParts
            |> List.countBy authorityAndFirstPathSegment
            |> filter (fun (_, count) -> count >= 5)
            |> map fst

            // side-effect
            |> iter writeln
