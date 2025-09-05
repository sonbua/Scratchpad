module VnEconomy

open FSharpPlus
open FSharp.Data

module Html =
    let private decimalCharToString (m: Match) =
        m.Groups[1].Value |> int |> char |> string

    let decodeDecimal input =
        Regex.Replace(input, @"&#(\d+);", decimalCharToString)

    let private hexadecimalCharToString (m: Match) =
        Convert.ToInt32(m.Groups[1].Value, 16) |> char |> string

    let decodeHexadecimal input =
        Regex.Replace(input, @"&#x([0-9a-f]+);", hexadecimalCharToString, RegexOptions.IgnoreCase)

    let decode = decodeDecimal >> decodeHexadecimal

let extractBlogChungKhoanLinks url =
    async {
        let! doc = url |> HtmlDocument.AsyncLoad

        return
            doc
            |> HtmlDocument.html
            |> HtmlNode.cssSelectR "a[href]"
            |> map (HtmlNode.attributeValue "href")
            |> filter (String.isSubString "blog-chung-khoan-")
            |> map (sprintf "https://vneconomy.vn%s")
    }

type Article =
    { Title: string
      Url: string
      Published: DateTime option }

let loadArticleMetadata url =
    async {
        let! doc = HtmlDocument.AsyncLoad url

        let title =
            doc
            |> HtmlDocument.descendantsNamed true [ "h2" ]
            |> Seq.map HtmlNode.innerText
            |> Seq.map Html.decode
            |> Seq.filter (String.isSubString "Blog")
            |> Seq.map String.trimWhiteSpaces
            |> Seq.tryHead
            |> Option.defaultValue "No title found"

        // TODO: another way to get published date
        let published =
            doc
            |> HtmlDocument.html
            |> HtmlNode.cssSelectR "meta[property='article:published_time']"
            |> List.tryHead
            |> Option.map (HtmlNode.attributeValue "content")
            |> Option.map DateTime.Parse

        return
            { Title = title
              Url = url
              Published = published }
    }
