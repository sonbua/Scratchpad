module TrangTrang

open FSharp.Data
open FsUnit
open Reusables
open Xunit
open Xunit.Abstractions

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

type ReportEntry =
    { Category: string
      Description: string
      Reporter: Reporter }

[<Literal>]
let ReportSampleUrl = "https://www.trangtrang.com/0904637134.html"

type ReportSample = HtmlProvider<ReportSampleUrl>

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

    let div2ToReportEntry (comment: Comment.Div2) =
        { Category = comment.H3 |> Option.defaultValue "No comment"
          Description = comment.P.Value
          Reporter = comment.Divs[1].Span |> Reporter.map }

    let toReportEntry: ToReportEntry =
        fun commentDiv ->
            if commentDiv.Divs.Length <> 2 then
                Error InvalidCommentFormat
            else
                commentDiv.Divs[1] |> div2ToReportEntry |> Ok

type Report =
    private
    | Clean
    | Reported of ReportEntry list

type GetReport = string -> Report

let getReport: GetReport =
    fun number ->
        let url = $"https://www.trangtrang.com/{number}.html"

        let commentSection =
            url
            |> ReportSample.Load
            |> fun x -> x.Lists.Html.Body().CssSelect("#nhan-xet > .r")

        let entries =
            commentSection
            |> List.map string
            |> List.map Comment.Parse
            |> List.map Comment.toReportEntry
            |> List.filter Result.isOk
            |> List.map Result.okValue

        match entries with
        | [] -> Clean
        | [ e ] when e.Category = "Chưa có nhận xét" -> Clean
        | _ -> Reported entries

type Tests(helper: ITestOutputHelper) =
    let quickReport (entry: ReportEntry) =
        {| Category = entry.Category
           Description = entry.Description |}

    [<Theory>]
    [<InlineData("0342486872")>]
    [<InlineData("0904637134")>]
    [<InlineData("0942750429")>] // Clean
    let ``Get report`` number =
        number
        |> getReport
        |> function
            | Clean -> []
            | Reported entries -> entries |> List.map (quickReport >> string)
        |> List.map helper.WriteLine
