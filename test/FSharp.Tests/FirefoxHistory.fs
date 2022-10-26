namespace Firefox

open System
open FsUnit
open FSharp.Data
open Xunit
open Xunit.Abstractions

module History =
    type UrlPart =
        { Authority: string
          PathSegments: string list }

    [<Literal>]
    let source = "TestData\\moz_places.json"

    type Places = JsonProvider<source>

    let urlPart (uri: Uri) =
        let toSegments (path: string) : string list =
            match path with
            | null
            | ""
            | "/" -> []
            | path ->
                path.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.toList

        { Authority = uri.Authority
          PathSegments = uri.LocalPath |> toSegments }

    let urlParts =
        Places.GetSamples()
        |> Array.toList
        |> List.sortByDescending (fun x -> Option.defaultValue 0L x.LastVisitDate)
        |> List.map (fun x -> x.Url |> Uri |> urlPart)

    type Reports(console: ITestOutputHelper) =
        let writeln = console.WriteLine

        [<Fact>]
        member _.``Count by authority, count >= 5``() =
            urlParts
            |> List.countBy (fun x -> x.Authority)
            |> List.where (fun (_, count) -> count >= 5)
            |> List.map fst

            // side-effect
            |> List.iter writeln

        [<Fact>]
        member _.``Count by authority and first path's segment, count >= 5``() =
            let firstPathSegment (segments: string list) =
                match segments with
                | [] -> ""
                | [ x ]
                | x :: _ -> $"/{x}"

            let authorityAndFirstPathSegment urlPart =
                $"{urlPart.Authority}{firstPathSegment urlPart.PathSegments}"

            urlParts
            |> List.countBy authorityAndFirstPathSegment
            |> List.where (fun (_, count) -> count >= 5)
            |> List.map fst

            // side-effect
            |> List.iter writeln
