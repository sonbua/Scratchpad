module TrangTrang

open System.Text.RegularExpressions
open FSharp.Data
open FsUnit
open Xunit
open Xunit.Abstractions

// Reporter
type Reporter =
    private
    | Anonymous
    | Name of string

module Reporter =
    let map (name: string option) =
        match name with
        | None -> Reporter.Anonymous
        | Some "Ẩn Danh" -> Reporter.Anonymous
        | Some n -> Reporter.Name n

// Report entries
type ReportEntry =
    { Category: string
      Description: string
      Reporter: Reporter }

[<Literal>]
let SampleReportUrl = "https://www.trangtrang.com/0904637134.html"

type ReportProvider = HtmlProvider<SampleReportUrl>

type Comment =
    XmlProvider<"""
<div class="r">
  <div class="avt">
    <div class="img a23"></div>
  </div>
  <div class="ctn">
    <h3>Lừa Đảo</h3><div class="rating s1"></div><p>Lừa đảo.</p>
    <div class="au">
      bởi <span>Ẩn Danh</span>, Việt Nam
<time datetime="2023-09-18 15:44:35">cách đây khoảng 1 giờ</time>
    </div>
  </div>
</div>
""">

module Comment =
    type CommentProblems = private | InvalidCommentFormat

    type private ToReportEntry = Comment.Div -> Result<ReportEntry, CommentProblems>

    let private div2ToReportEntry (comment: Comment.Div2) =
        { Category = comment.H3 |> Option.defaultValue "No category"
          Description = comment.P.Value
          Reporter = comment.Divs[1].Span |> Reporter.map }

    let toReportEntry: ToReportEntry =
        fun commentDiv ->
            if commentDiv.Divs.Length <> 2 then
                Error InvalidCommentFormat
            else
                commentDiv.Divs[1] |> div2ToReportEntry |> Ok

type ReportEntries = ReportEntry list

module ReportEntries =
    type private Create = ReportEntry list -> ReportEntries

    let create: Create =
        function
        | [ e ] when e.Category = "Chưa có nhận xét" -> []
        | entries -> entries

// Carrier
type Carrier =
    private
    | MobiFone
    | VinaPhone
    | Viettel
    | Unknown of string

module Carrier =
    type private Create = string -> Carrier

    let create: Create =
        function
        | nameof Viettel -> Viettel
        | nameof MobiFone -> MobiFone
        | "Vinaphone" -> VinaPhone
        | number -> Unknown number

// Report
type Report =
    { Number: string
      Carrier: Carrier
      Entries: ReportEntries }

module Report =
    type private Get = string -> Report

    let get: Get =
        let regexMatches pattern text = Regex.Matches(text, pattern)

        fun number ->
            let url = $"https://www.trangtrang.com/{number}.html"
            let bodyElement = url |> ReportProvider.Load |> (fun x -> x.Lists.Html.Body())

            let normalizedNumber =
                bodyElement.CssSelect("header.p > h2").Head.InnerText().ToString()
                |> regexMatches "Số (\d+)"
                |> Seq.head
                |> fun x -> x.Groups[1].Value

            let numberDetailsSection =
                bodyElement |> fun x -> x.CssSelect("div.bc > nav > ul > li")

            let carrier =
                match numberDetailsSection.Length with
                | 4 -> numberDetailsSection[2].CssSelect("a").Head.InnerText()
                | 2 -> numberDetailsSection[1].InnerText()
                | _ -> "Unknown"
                |> Carrier.create

            let commentSection = bodyElement |> fun x -> x.CssSelect("#nhan-xet > .r")

            let entries =
                commentSection
                |> List.map string
                |> List.map Comment.Parse
                |> List.map Comment.toReportEntry
                |> List.filter Result.isOk
                |> List.map Result.get
                |> ReportEntries.create

            { Number = normalizedNumber
              Carrier = carrier
              Entries = entries }

type Tests(helper: ITestOutputHelper) =
    let toReportDto (report: Report) =
        {| Carrier = report.Carrier
           Comments =
            report.Entries
            |> List.map (fun entry ->
                {| Category = entry.Category
                   Description = entry.Description |}) |}

    [<Theory>]
    [<InlineData("0342486872")>]
    [<InlineData("0904637134")>]
    [<InlineData("0942750429")>] // Clean
    let ``Get report`` number =
        number |> Report.get |> toReportDto |> sprintf "%A" |> helper.WriteLine
