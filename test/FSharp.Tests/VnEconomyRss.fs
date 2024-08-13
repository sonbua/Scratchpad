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
      Link: string }

module RssFeedItem =
    let toFeedItem (item: RssFeed.Item) =
        { Title = item.Title |> String.trimWhiteSpaces
          Published = item.PubDate.UtcDateTime |> stringf "dd/MM/yyyy"
          Link = item.Link }

type Test(helper: ITestOutputHelper) =
    let charDecode input =
        let charCodeToString (m: Match) =
            m.Groups[1].Value |> int |> char |> string

        Regex.Replace(input, "\#(\d+);", charCodeToString)

    [<Fact>]
    member _.``Blog chứng khoán``() =
        RssFeed.GetSample()
        |> _.Channel.Items
        |> filter (fun x -> x.Title.Contains "Blog chứng khoán")
        |> sortByDescending _.PubDate
        |> map RssFeedItem.toFeedItem
        |> iter (sprintf "%A" >> helper.WriteLine)
