module VnEconomyRss

open System.Text.RegularExpressions
open FSharpPlus
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
        let charCodeToString (m: Match) =
            m.Groups[1].Value |> int |> char |> string

        Regex.Replace(input, "\#(\d+);", charCodeToString)

    [<Fact>]
    member _.``Blog chứng khoán - last 7 entries``() =
        RssFeed.GetSample()
        |> _.Channel.Items
        |> filter (fun x -> x.Category = "Chứng khoán")
        |> filter (fun x -> x.Title.StartsWith "Blog chứng khoán")
        |> sortByDescending _.PubDate
        |> map (fun x ->
            { Title = x.Title
              Published = x.PubDate.UtcDateTime |> stringf "dd/MM/yyyy"
              Abstract = x.Description |> charDecode
              Link = x.Link })
        |> iter (sprintf "%A" >> helper.WriteLine)
