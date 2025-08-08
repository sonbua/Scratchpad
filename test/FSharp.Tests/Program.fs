module Program

open FSharp.SystemCommandLine
open FSharpPlus

let fromClipboardInput =
    Input.option "--fromClipboard"
    |> Input.alias "--from-clipboard"
    |> Input.desc "Read input from clipboard. When not specified, input is read from the command line."
    |> Input.defaultValue false

let toClipboardInput =
    Input.option "--toClipboard"
    |> Input.alias "--to-clipboard"
    |> Input.desc "Copy output to clipboard. When not specified, output is printed to the console."
    |> Input.defaultValue false

let withClipboardChannel f (maybeText: string option, fromClipboard: bool, toClipboard: bool) : unit =
    let text =
        if fromClipboard then
            Clipboard.get
        else
            maybeText |> Option.defaultValue ""

    let output = f text

    if toClipboard then
        output |> Clipboard.set
    else
        output |> printfn "%s"

module Cleanup =
    module RiderLog =
        open System
        open RiderLog

        let private riderLogAction () =
            let options =
                { LogRootDir = "R:\\idea-log\\"
                  CutOffTimestamp = DateTime.Now.Date }

            options |> cleanup |> map (printfn "%s") |> ignore

        let command =
            command "rider-log" {
                description "Cleanup Rider logs, except today's"
                setAction riderLogAction
            }

    module FirefoxHistory =
        open Firefox.History

        let private deleteUntitledPlacesInput =
            Input.option "--deleteUntitledPlaces"
            |> Input.alias "--delete-untitled-places"
            |> Input.desc "Delete untitled places"
            |> Input.defaultValue true

        let private db =
            @"C:\Users\song\AppData\Roaming\Mozilla\Firefox\Profiles\4kcatvle.default-release\places.sqlite"
            |> sprintf "Data Source=%s;"
            |> Db

        let private printPlace = _.Url >> printfn "%A"

        let private firefoxHistoryAction (deleteUntitledPlaces: bool) : unit =
            async {
                if deleteUntitledPlaces then
                    let! removed = db.deleteUntitled
                    removed |> map printPlace |> ignore
            }
            |> Async.RunSynchronously

        let command =
            command "firefox-history" {
                description "Cleanup Firefox history"
                inputs deleteUntitledPlacesInput
                setAction firefoxHistoryAction
            }

    let command =
        command "cleanup" {
            description "Cleanup garbage"
            noAction
            addCommand FirefoxHistory.command
            addCommand RiderLog.command
        }

module Convert =
    let private maybeTextInput =
        Input.argumentMaybe "text" |> Input.desc "Text to convert"

    let private convertAction = TableConverter.tabularToMarkdown |> withClipboardChannel

    let command =
        command "convert" {
            description "Convert tab-delimited to markdown"
            inputs (maybeTextInput, fromClipboardInput |> Input.def true, toClipboardInput |> Input.def true)
            setAction convertAction
        }

module Extract =
    let private extractAction =
        let f = Uri.extractUriStrings >> String.concat System.Environment.NewLine
        f |> withClipboardChannel

    let private maybeTextInput =
        Input.argumentMaybe "text" |> Input.desc "Text to extract URLs from"

    let command =
        command "extract" {
            description "Extract URLs from text"
            inputs (maybeTextInput, fromClipboardInput, toClipboardInput)
            setAction extractAction
        }

module Fetch =
    module BlogChungKhoanRss =
        open System.Text.RegularExpressions

        /// Was used to decode the description
        let private charDecode input =
            let charCodeToString (m: Match) =
                m.Groups[1].Value |> int |> char |> string

            Regex.Replace(input, "\#(\d+);", charCodeToString)

        let private isBlogChungKhoan (x: VnEconomyRss.RssFeed.Item) =
            x.Title.Contains "Blog chứng khoán"
            || String.isSubString "vneconomy.vn/blog-chung-khoan-" x.Link

        let private blogChungKhoanAction () =
            VnEconomyRss.RssFeed.GetSample()
            |> _.Channel.Items
            |> filter isBlogChungKhoan
            |> sortByDescending _.PubDate
            |> map VnEconomyRss.RssFeedItem.toFeedItem
            |> map (sprintf "%A")
            |> String.concat System.Environment.NewLine
            |> printfn "%s"

        let command =
            command "blog-chung-khoan" {
                description "Blog chứng khoán RSS feed"
                setAction blogChungKhoanAction
            }

    let command =
        command "fetch" {
            description "Fetch RSS feed"
            noAction
            addCommand BlogChungKhoanRss.command
        }

module Outdated =
    open Chocolatey.Outdated

    let private outdatedAction () =
        run () |> Result.either id id |> printfn "%s"

    let command =
        command "outdated" {
            description "Check for outdated Chocolatey packages"
            setAction outdatedAction
        }

/// <summary>
/// Sample usages:
/// <code>
/// cleanup rider-log
/// cleanup firefox-history
/// cleanup firefox-history --deleteUntitledPlaces false
/// convert
/// convert &lt;text&gt;
/// outdated
/// </code>
/// </summary>
/// TODO: convert tab2md
[<EntryPoint>]
let main argv =
    rootCommand argv {
        description "Utility tools"
        noAction
        addCommand Cleanup.command
        addCommand Convert.command
        addCommand Extract.command
        addCommand Fetch.command
        addCommand Outdated.command
    }
