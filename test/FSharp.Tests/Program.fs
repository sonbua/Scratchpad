module Program

open FSharp.SystemCommandLine
open FSharpPlus

let fromClipboardInput =
    Input.option "--fromClipboard"
    |> Input.alias "--from-clipboard"
    |> Input.desc "Read input from clipboard."
    |> Input.defaultValue false

let toClipboardInput =
    Input.option "--toClipboard"
    |> Input.alias "--to-clipboard"
    |> Input.desc "Copy output to clipboard. When set to False, output is printed to the console."
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
        open RiderLog

        let private riderLogAction () =
            let options =
                { LogRootDir = "R:\\idea-log\\"
                  CutOffTimestamp = DateTime.Now.Date }

            options |> cleanup |> map (printfn "%s") |> ignore

        let command =
            command "rider-log" {
                description "Cleanup Rider logs, except for today's"
                setAction riderLogAction
            }

    module FirefoxHistory =
        open Firefox.History
        open YamlDotNet.Serialization
        open YamlDotNet.Serialization.NamingConventions

        [<CLIMutable>]
        type Config =
            { Domains: List<string>
              DomainsHavingFragments: List<string>
              DomainsHavingPaths: List<DomainWithPaths>
              QueryParams: List<string>
              DomainsHavingAnyQueryParam: List<DomainWithQueryParams>
              DomainsHavingAnyFragmentParam: List<DomainWithFragmentParam>
              DomainsHavingPathPatterns: List<DomainWithPathPatterns>
              DomainsHavingNotFirstThreadPost: List<string> }

        and [<CLIMutable>] DomainWithPaths = { Domain: string; Paths: List<string> }

        and [<CLIMutable>] DomainWithQueryParams =
            { Domain: string
              QueryParams: List<string> }

        and [<CLIMutable>] DomainWithFragmentParam =
            { Domain: string
              FragmentParams: List<string> }

        and [<CLIMutable>] DomainWithPathPatterns =
            { Domain: string
              Patterns: List<string> }

        let deserializer =
            DeserializerBuilder().WithNamingConvention(HyphenatedNamingConvention.Instance).Build()

        let config =
            lazy ("firefox-history.yml" |> File.ReadAllText |> deserializer.Deserialize<Config>)

        let private untitledEntriesInput =
            Input.option "--untitledEntries"
            |> Input.alias "--untitled-entries"
            |> Input.desc "Delete untitled history entries"
            |> Input.defaultValue true

        let private garbageDomainsInput =
            Input.option "--garbageDomains"
            |> Input.alias "--garbage-domains"
            |> Input.desc "Delete history entries in the domain list"
            |> Input.defaultValue true

        let private garbageDomainsHavingFragmentsInput =
            Input.option "--garbageDomainsHavingFragmentsInput"
            |> Input.alias "--garbage-domains-having-fragments"
            |> Input.desc "Delete history entries that have fragment(s) in the domain list"
            |> Input.defaultValue true

        let private domainsHavingGarbagePathsInput =
            Input.option "--domainsHavingGarbagePathsInput"
            |> Input.alias "--domains-having-garbage-paths"
            |> Input.desc "Delete history entries that have matching domain and path(s) in the list"
            |> Input.defaultValue true

        let private garbageQueryParamsInput =
            Input.option "--garbageQueryParams"
            |> Input.alias "--garbage-query-params"
            |> Input.desc "Delete history entries that have any of the query params in the list"
            |> Input.defaultValue true

        let private domainsHavingAnyGarbageQueryParamInput =
            Input.option "--domainsHavingAnyGarbageQueryParam"
            |> Input.alias "--domains-having-any-garbage-query-param"
            |> Input.desc "Delete history entries that have matching domain and any of the query params in the list"
            |> Input.defaultValue true

        let private domainsHavingAnyGarbageFragmentParamInput =
            Input.option "--domainsHavingAnyGarbageFragmentParam"
            |> Input.alias "--domains-having-any-garbage-fragment-param"
            |> Input.desc "Delete history entries that have matching domain and any of the fragment param in the list"
            |> Input.defaultValue true

        let private domainsHavingPathPatternsInput =
            Input.option "--domainsHavingPathPatterns"
            |> Input.alias "--domains-having-path-patterns"
            |> Input.desc "Delete history entries that have matching domain and path pattern(s) in the list"
            |> Input.defaultValue true

        let private domainsHavingNotFirstThreadPostInput =
            Input.option "--domainsHavingNotFirstThreadPost"
            |> Input.alias "--domains-having-not-first-thread-post"
            |> Input.desc "Delete history entries that have matching domain and not first thread post in the list"
            |> Input.defaultValue true

        let private domainsHavingComplexPatternsInput =
            Input.option "--domainsHavingComplexPatterns"
            |> Input.alias "--domains-having-complex-patterns"
            |> Input.desc "Delete history entries that having predefined complex patterns"
            |> Input.defaultValue true

        let private db =
            @"C:\Users\song\AppData\Roaming\Mozilla\Firefox\Profiles\4kcatvle.default-release\places.sqlite"
            |> sprintf "Data Source=%s;"
            |> Db

        let private printPlace = _.Url >> printfn "%s"

        let private firefoxHistoryAction (ctx: ActionContext) : unit =
            async {
                let parseResult = ctx.ParseResult

                let deleteUntitledEntries = untitledEntriesInput.GetValue parseResult

                if deleteUntitledEntries then
                    let! removed = db.deleteUntitled
                    removed |> map printPlace |> ignore

                let config = config.Value

                let deleteGarbageDomains = garbageDomainsInput.GetValue parseResult

                if deleteGarbageDomains then
                    let domains = config.Domains

                    // TODO: Use AsyncSeq? https://fsprojects.github.io/FSharp.Control.AsyncSeq/AsyncSeq.html
                    for domain in domains do
                        let! removed = db.deletePlaces domain
                        removed |> map printPlace |> ignore

                let deleteGarbageDomainsHavingFragments =
                    garbageDomainsHavingFragmentsInput.GetValue parseResult

                if deleteGarbageDomainsHavingFragments then
                    let domains = config.DomainsHavingFragments

                    for domain in domains do
                        let placeFilter = Place.withFragment
                        let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                        removed |> map printPlace |> ignore

                let deleteDomainsHavingGarbagePaths =
                    domainsHavingGarbagePathsInput.GetValue parseResult

                if deleteDomainsHavingGarbagePaths then
                    let domains = config.DomainsHavingPaths

                    for { Domain = domain; Paths = paths } in domains do
                        let placeFilter = paths |> map String.isSubString |> orF |> (>>) _.Url
                        let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                        removed |> map printPlace |> ignore

                let deleteGarbageQueryParams = garbageQueryParamsInput.GetValue parseResult

                if deleteGarbageQueryParams then
                    let queryParams = config.QueryParams

                    for queryParam in queryParams do
                        let placeFilter = queryParams |> List.ofSeq |> Place.hasAnyQueryParam
                        let! removed = (queryParam, placeFilter) ||> db.deletePlacesWith
                        removed |> map printPlace |> ignore

                let deleteDomainsHavingAnyGarbageQueryParam =
                    domainsHavingAnyGarbageQueryParamInput.GetValue parseResult

                if deleteDomainsHavingAnyGarbageQueryParam then
                    let domains = config.DomainsHavingAnyQueryParam

                    for { Domain = domain
                          QueryParams = queryParams } in domains do
                        let placeFilter = queryParams |> List.ofSeq |> Place.hasAnyQueryParam
                        let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                        removed |> map printPlace |> ignore

                let deleteDomainsHavingAnyGarbageFragmentParam =
                    domainsHavingAnyGarbageFragmentParamInput.GetValue parseResult

                if deleteDomainsHavingAnyGarbageFragmentParam then
                    let domains = config.DomainsHavingAnyFragmentParam

                    for { Domain = domain
                          FragmentParams = fragmentParams } in domains do
                        let placeFilter = fragmentParams |> List.ofSeq |> Place.hasAnyFragmentParam
                        let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                        removed |> map printPlace |> ignore

                let deleteDomainsHavingPathPatterns =
                    domainsHavingPathPatternsInput.GetValue parseResult

                if deleteDomainsHavingPathPatterns then
                    let domains = config.DomainsHavingPathPatterns

                    for { Domain = domain; Patterns = patterns } in domains do
                        let placeFilter = patterns |> map Regex.isMatch |> orF |> (>>) _.Url
                        let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                        removed |> map printPlace |> ignore

                let deleteDomainsHavingNotFirstThreadPost =
                    domainsHavingNotFirstThreadPostInput.GetValue parseResult

                if deleteDomainsHavingNotFirstThreadPost then
                    let domains = config.DomainsHavingNotFirstThreadPost

                    for domain in domains do
                        let placeFilter = Place.isNotFirstThreadPost
                        let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                        removed |> map printPlace |> ignore

                let deleteDomainsHavingComplexPatterns =
                    domainsHavingComplexPatternsInput.GetValue parseResult

                if deleteDomainsHavingComplexPatterns then
                    let domainsHavingComplexPatterns: (string * (Place -> bool)) list =
                        [ "local",
                          andF
                              [ _.Url >> Regex.isMatch ":\\d+/"
                                orF [ Place.withQueryParam; Place.withFragment ] ]
                          "localhost/", Place.withQueryParam
                          "nuget.optimizely.com", Place.hasQueryParams [ "id"; "v" ]
                          "opti-dxp.datadoghq.com/logs", Place.withQueryParam
                          "opti-dxp.datadoghq.com/monitors/", Place.withQueryParam
                          "world.taobao.com", Place.hasQueryParams [ "a"; "b" ]
                          "www.jetbrains.com/help/", Place.withQueryParam ]

                    for domain, placeFilter in domainsHavingComplexPatterns do
                        let! removed = (domain, placeFilter) ||> db.deletePlacesWith
                        removed |> map printPlace |> ignore
            }
            |> Async.RunSynchronously

        let command =
            command "firefox-history" {
                description "Cleanup Firefox history"
                inputs Input.context
                setAction firefoxHistoryAction

                addInputs
                    [ untitledEntriesInput
                      garbageDomainsInput
                      garbageDomainsHavingFragmentsInput
                      domainsHavingGarbagePathsInput
                      garbageQueryParamsInput
                      domainsHavingAnyGarbageQueryParamInput
                      domainsHavingAnyGarbageFragmentParamInput
                      domainsHavingPathPatternsInput
                      domainsHavingNotFirstThreadPostInput
                      domainsHavingComplexPatternsInput ]
            }

    module Logseq =
        open Logseq

        let private noopInput =
            Input.option "--noop"
            |> Input.desc "List removable backup directories without deleting them."
            |> Input.defaultValue false

        let private backupDirectories =
            [ { Path = @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\pages\"
                Pattern = "*.md" }
              { Path = @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\journals\"
                Pattern = "*.md" }
              { Path = @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\logseq\"
                Pattern = "*.edn" }
              { Path = @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\logseq\"
                Pattern = "*.css" } ]

        let private options = { ItemsToKeep = 1 }

        let private logseqAction (noop: bool) =
            let backupDirectoryF options dir =
                if noop then
                    (options, dir)
                    ||> RootBackupDirectory.pendingCleanupDirectories
                    |> map (printfn "%A")
                else
                    (options, dir) ||> RootBackupDirectory.cleanup |> map (printfn "%s")

            backupDirectories |> map (backupDirectoryF options) |> ignore

        let command =
            command "logseq" {
                description "Cleanup Logseq backup files"
                inputs noopInput
                setAction logseqAction
            }

    module NuGetCache =
        open NuGetCache

        module Removable =
            let format (r: Removable) =
                let versionsFormatted =
                    r.RemovingVersions |> map _.Name |> String.concat ", " |> sprintf "[ %s ]"

                $"{r.PackageDir.Name} -> {versionsFormatted}"

        let private noopInput =
            Input.option "--noop"
            |> Input.desc "List removable package directories without deleting them."
            |> Input.defaultValue false

        let private nugetCacheAction (noop: bool) =
            let cleanupOptions = { CacheRootDir = @"C:\Users\song\.nuget\packages" }

            cleanupOptions
            |> match noop with
               | true -> listRemovables
               | false -> cleanup
            |> map Removable.format
            |> map (printfn "%s")
            |> ignore

        let command =
            command "nuget-cache" {
                description "Cleanup NuGet cache"
                inputs noopInput
                setAction nugetCacheAction
            }

    let command =
        command "cleanup" {
            description "Cleanup garbage"
            noAction
            addCommands [ FirefoxHistory.command; RiderLog.command; Logseq.command; NuGetCache.command ]
        }

module Convert =
    let private maybeTextInput =
        Input.argumentMaybe "text" |> Input.desc "Text to convert"

    module Tab2Md =
        let private tab2MdAction = TableConverter.tabularToMarkdown |> withClipboardChannel

        let command =
            command "tab2md" {
                description "Convert tab-delimited text to markdown format"
                inputs (maybeTextInput, fromClipboardInput |> Input.def true, toClipboardInput |> Input.def true)
                setAction tab2MdAction
            }

    module Md2Tab =
        let private md2TabAction = TableConverter.markdownToTabular |> withClipboardChannel

        let command =
            command "md2tab" {
                description "Convert markdown table to tab-delimited format"
                inputs (maybeTextInput, fromClipboardInput |> Input.def true, toClipboardInput |> Input.def true)
                setAction md2TabAction
            }

    let command =
        command "convert" {
            description "Convert between tab-delimited and markdown formats"
            noAction
            addCommands [ Tab2Md.command; Md2Tab.command ]
        }

module Extract =
    let private extractAction =
        let f = Uri.extractUriStrings >> String.concat Environment.NewLine
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
    module BlogChungKhoan =
        let landingPages =
            [ "https://vneconomy.vn/"
              "https://vneconomy.vn/chung-khoan.htm"
              "https://vneconomy.vn/thi-truong-chung-khoan.htm" ]

        let ckPages =
            [ 2..10 ] |> List.map (sprintf "https://vneconomy.vn/chung-khoan.htm?page=%d")

        let ttckPages =
            [ 2..10 ]
            |> List.map (sprintf "https://vneconomy.vn/thi-truong-chung-khoan.htm?page=%d")

        let urls = landingPages @ ckPages @ ttckPages

        let private blogChungKhoanAction () =
            async {
                let! links = urls |> map VnEconomy.extractBlogChungKhoanLinks |> Async.Parallel

                let! articles =
                    links
                    |> List.concat
                    |> distinct
                    |> map VnEconomy.loadArticleMetadata
                    |> Async.Parallel

                articles
                |> sortByDescending _.Published
                |> map (fun a ->
                    {| Date = a.Published.Value.Date.ToString("dd/MM/yyyy")
                       Url = a.Url |})
                |> map (fun a -> printfn $"{a.Date} {a.Url}")
                |> ignore
            }
            |> Async.RunSynchronously

        let command =
            command "blog-chung-khoan" {
                description "Blog chứng khoán RSS feed"
                setAction blogChungKhoanAction
            }

    let command =
        command "fetch" {
            description "Fetch RSS feed"
            noAction
            addCommand BlogChungKhoan.command
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

module WhatDayOfWeek =
    open WhatDayOfWeek

    let private dayInput =
        Input.argument "day"
        |> Input.acceptOnlyFromAmong ([ 1..31 ] |> List.map string)
        |> Input.desc "Day (1-31)"

    let private monthInput =
        Input.argumentMaybe "month"
        |> Input.acceptOnlyFromAmong ([ 1..12 ] |> List.map string)
        |> Input.desc "Month (1-12)"

    let private yearInput = Input.argumentMaybe "year" |> Input.desc "Year (e.g., 2024)"

    let private whatDayOfWeekAction (day: int, month: int option, year: int option) =
        let month = month |> Option.defaultValue DateTime.Now.Month
        let year = year |> Option.defaultValue DateTime.Now.Year

        whatDayOfWeek (year, month, day) |> printfn "%A"

    let command =
        command "what-day-of-week" {
            addAlias "whatdayofweek"
            description "Determine the day of the week for a given date"
            inputs (dayInput, monthInput, yearInput)
            setAction whatDayOfWeekAction
        }

/// <summary>
/// Sample usages:
/// <code>
/// cleanup rider-log
/// cleanup firefox-history
/// cleanup firefox-history
///     --untitled-entries=false
///     --garbage-domains=false
///     --garbage-domains-having-fragments=false
///     --domains-having-garbage-paths=false
///     --garbage-query-params=false
///     --domains-having-any-garbage-query-param=false
///     --domains-having-any-garbage-fragment-param=false
///     --domains-having-path-patterns=false
///     --domains-having-not-first-thread-post=false
///     --domains-having-complex-patterns=false
/// cleanup logseq --noop
/// cleanup nuget-cache --noop
/// convert tab2md
/// convert md2tab
/// extract "text with url https://example.com"
/// fetch blog-chung-khoan
/// outdated
/// what-day-of-week 25
/// what-day-of-week 25 12
/// what-day-of-week 25 12 2024
/// </code>
/// </summary>
[<EntryPoint>]
let main argv =
    rootCommand argv {
        description "Utility tools"
        noAction

        addCommands
            [ Cleanup.command
              Convert.command
              Extract.command
              Fetch.command
              Outdated.command
              WhatDayOfWeek.command ]
    }
