module VnEconomyRss

open System
open System.Text.RegularExpressions
open FSharp.Data
open Xunit
open Xunit.Abstractions

[<Literal>]
let RssFeedUrl = "https://vneconomy.vn/chung-khoan.rss"

type RssFeed = XmlProvider<RssFeedUrl>

type FeedItem =
    { Title: string
      Published: string
      Abstract: string
      Link: string }

type Test(helper: ITestOutputHelper) =
    let charDecode input =
        let charCodeToString = fun (m: Match) -> m.Groups[1].Value |> int |> char |> string
        Regex.Replace(input, "\#(\d+);", charCodeToString)

    [<Fact>]
    let ``Blog chứng khoán - last 7 entries`` () =
        RssFeed.GetSample()
        |> (fun x -> x.Channel.Items)
        |> Array.toList
        |> Seq.where (fun x -> x.Category = "Chứng khoán")
        |> Seq.where (fun x -> x.Title.StartsWith "Blog chứng khoán")
        |> Seq.sortByDescending (fun x -> x.PubDate)
        |> Seq.chunkBySize 7
        |> Seq.head
        |> Seq.map (fun x ->
            { Title = x.Title
              Published = x.PubDate.UtcDateTime |> DateOnly.FromDateTime |> stringf "dd/MM/yyyy"
              Abstract = x.Description |> charDecode
              Link = x.Link })
        |> Seq.map (sprintf "%A" >> helper.WriteLine)
        |> Seq.toList
