module VnEconomyRss

open FSharpPlus
open FSharp.Data

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
