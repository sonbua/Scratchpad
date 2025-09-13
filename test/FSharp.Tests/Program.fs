module Program

open FSharp.SystemCommandLine
open FSharpPlus
open FsToolkit.ErrorHandling

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

module Azure =
    open Azure
    open YamlDotNet.Serialization
    open YamlDotNet.Serialization.NamingConventions

    let private azureConfigInput =
        Input.option "--azure-config"
        |> Input.desc "The path to the Azure config file"
        // TODO: use environment variable to expand path
        |> Input.defaultValue (@"C:\Users\song\Downloads\azure.yml" |> FileInfo)
        |> Input.validateFileExists

    let private deserializer =
        DeserializerBuilder().WithNamingConvention(HyphenatedNamingConvention.Instance).Build()

    module EventGrid =
        module Topic =
            module Delete =
                open Azure.EventGrid
                open Fake.IO

                let private topicNamesInput =
                    Input.argument "topic-names"
                    |> Input.desc "The names of the topics to delete"
                    |> Input.allowMultipleArgumentsPerToken

                let private deleteAction (topicNames: string array, azureConfig: FileInfo) =
                    asyncResult {
                        let azureOptions =
                            azureConfig.FullName
                            |> File.readAsString
                            |> deserializer.Deserialize<AzureOptions>

                        for topicName in topicNames do
                            do! topicName |> deleteTopic azureOptions
                            printfn $"Deleted topic '%s{topicName}'."
                    }
                    |> fun ar ->
                        match ar |> Async.RunSynchronously with
                        | Error e -> e |> printfn "%A"
                        | _ -> ()

                let command =
                    command "delete" {
                        description "Delete multiple topics, separated by space"
                        inputs (topicNamesInput, azureConfigInput)
                        setAction deleteAction
                    }

            let command =
                command "topic" {
                    description "Event Grid topic tools"
                    inputs Input.context
                    helpAction
                    addCommand Delete.command
                }

        let command =
            command "event-grid" {
                addAlias "eg"
                description "Azure Event Grid tools"
                inputs Input.context
                helpAction
                addCommand Topic.command
            }

    module StorageAccount =
        module Delete =
            open Fake.IO

            let private accountNamesInput =
                Input.argument "account-names"
                |> Input.desc "The names of the storage accounts to delete"
                |> Input.allowMultipleArgumentsPerToken

            let private deleteAction (accountNames: string array, azureConfig: FileInfo) =
                asyncResult {
                    let azureOptions =
                        azureConfig.FullName
                        |> File.readAsString
                        |> deserializer.Deserialize<AzureOptions>

                    for accountName in accountNames do
                        do! accountName |> StorageAccount.delete azureOptions
                        printfn $"Deleted storage account '%s{accountName}'."
                }
                |> fun ar ->
                    match ar |> Async.RunSynchronously with
                    | Error e -> e |> printfn "%A"
                    | _ -> ()

            let command =
                command "delete" {
                    description "Delete multiple storage accounts, separated by space"
                    inputs (accountNamesInput, azureConfigInput)
                    setAction deleteAction
                }

        let command =
            command "storage-account" {
                addAlias "sa"
                description "Azure Storage Account tools"
                inputs Input.context
                helpAction
                addCommand Delete.command
            }

    let command =
        command "azure" {
            addAlias "az"
            description "Azure tools"
            inputs Input.context
            helpAction
            addCommands [ EventGrid.command; StorageAccount.command ]
        }

module Calendar =
    open Calendar

    let private isLeapStr isLeap = if isLeap then " (leap month)" else ""

    let private getTimeZone () =
        TimeZoneInfo.Local.GetUtcOffset(DateTime.Now).TotalHours

    module Solar2Lunar =
        let private dayInput =
            Input.argumentMaybe "day"
            |> Input.desc "Day (1-31). Default to today if not provided."
            |> Input.validate (fun d ->
                match d with
                | Some d when d < 1 || d > 31 -> Error "Day must be between 1 and 31"
                | _ -> Ok())

        let private monthInput =
            Input.argumentMaybe "month"
            |> Input.desc "Month (1-12). Default to current month if not provided."
            |> Input.validate (fun m ->
                match m with
                | Some m when m < 1 || m > 12 -> Error "Month must be between 1 and 12"
                | _ -> Ok())

        let private yearInput =
            Input.argumentMaybe "year"
            |> Input.desc "Year (e.g., 2024). Default to current year if not provided."

        let private solar2LunarAction (day: int option, month: int option, year: int option) =
            let now = DateTime.Now

            let day = day |> Option.defaultValue now.Day
            let month = month |> Option.defaultValue now.Month
            let year = year |> Option.defaultValue now.Year
            let timeZone = getTimeZone ()

            let lunarDay, lunarMonth, lunarYear, isLeap = solar2Lunar day month year timeZone
            let dayInWeek = WhatDayOfWeek.whatDayOfWeek (year, month, day)

            printfn
                $"{day}/{month}/{year} ({dayInWeek}) --> {lunarDay}/{lunarMonth}/{lunarYear}{isLeap |> isLeapStr} [AL]"

        let command =
            command "solar2lunar" {
                addAliases [ "al"; "dl2al" ]
                description "Convert a solar date to a lunar date (Vietnamese calendar)"
                inputs (dayInput, monthInput, yearInput)
                setAction solar2LunarAction
            }

    module Lunar2Solar =
        let private lunarDayInput =
            Input.argumentMaybe "lunar-day"
            |> Input.desc "Lunar day (1-30). Default to today if not provided."
            |> Input.validate (fun d ->
                match d with
                | Some d when d < 1 || d > 30 -> Error "Lunar day must be between 1 and 30"
                | _ -> Ok())

        let private lunarMonthInput =
            Input.argumentMaybe "lunar-month"
            |> Input.desc "Lunar month (1-12). Default to current lunar month if not provided."
            |> Input.validate (fun m ->
                match m with
                | Some m when m < 1 || m > 12 -> Error "Lunar month must be between 1 and 12"
                | _ -> Ok())

        let private lunarYearInput =
            Input.argumentMaybe "lunar-year"
            |> Input.desc "Lunar year (e.g., 2024). Default to current lunar year if not provided."

        let private isLeapInput =
            Input.option "-l"
            |> Input.alias "--is-leap"
            |> Input.defaultValue false
            |> Input.desc "Whether the lunar month is a leap month. Default to false if not provided."

        let lunar2SolarAction (lunarDay, lunarMonth, lunarYear, isLeap) =
            let now = DateTime.Now
            let timeZone = getTimeZone ()

            let d, m, y, _ = solar2Lunar now.Day now.Month now.Year timeZone

            let lunarDay = lunarDay |> Option.defaultValue d
            let lunarMonth = lunarMonth |> Option.defaultValue m
            let lunarYear = lunarYear |> Option.defaultValue y

            let day, month, year = lunar2Solar lunarDay lunarMonth lunarYear isLeap timeZone
            let dayInWeek = WhatDayOfWeek.whatDayOfWeek (year, month, day)

            printfn
                $"{day}/{month}/{year} ({dayInWeek}) <-- {lunarDay}/{lunarMonth}/{lunarYear}{isLeap |> isLeapStr} [AL]"

        let command =
            command "lunar2solar" {
                addAliases [ "dl"; "al2dl" ]
                description "Convert a lunar date to a solar date (Vietnamese calendar)"
                inputs (lunarDayInput, lunarMonthInput, lunarYearInput, isLeapInput)

                setAction lunar2SolarAction
            }

    let command =
        command "calendar" {
            addAlias "cal"
            description "Calendar tools"
            inputs Input.context
            helpAction
            addCommands [ Solar2Lunar.command; Lunar2Solar.command ]
        }

module Cleanup =
    module RiderLog =
        open RiderLog

        let rootDirectoryInput =
            Input.argument "root-directory"
            |> Input.desc "Path to the root directory of Rider logs"
            |> Input.defaultValue (@"R:\idea-log\" |> DirectoryInfo)
            |> Input.validateDirectoryExists

        let private riderLogAction (rootDirectoryInput: DirectoryInfo) =
            let options =
                { LogRootDir = rootDirectoryInput.FullName
                  CutOffTimestamp = DateTime.Now.Date }

            options |> cleanup |> map (printfn "%s") |> ignore

        let command =
            command "rider-log" {
                description "Cleanup Rider logs, except for today's"
                inputs rootDirectoryInput
                setAction riderLogAction
            }

    module FirefoxHistory =
        open Firefox.History
        open YamlDotNet.Serialization
        open YamlDotNet.Serialization.NamingConventions

        [<CLIMutable>]
        type Config =
            { Domains: List<string>
              DomainsWithFragment: List<string>
              DomainsHavingPaths: List<DomainWithPaths>
              QueryParams: List<string>
              DomainsHavingAnyQueryParam: List<DomainWithQueryParams>
              DomainsHavingAllQueryParams: List<DomainWithQueryParams>
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

        let private deserializer =
            DeserializerBuilder().WithNamingConvention(HyphenatedNamingConvention.Instance).Build()

        let private config =
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

        let private garbageDomainsWithFragmentInput =
            Input.option "--garbageDomainsWithFragmentInput"
            |> Input.alias "--garbage-domains-with-fragment"
            |> Input.desc "Delete history entries that contain fragment(s) in the domain list"
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

        let private domainsHavingAllGarbageQueryParamsInput =
            Input.option "--domainsHavingAllGarbageQueryParams"
            |> Input.alias "--domains-having-all-garbage-query-params"
            |> Input.desc "Delete history entries that have matching domain and all of the query params in the list"
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

                let deleteGarbageDomainsWithFragment =
                    garbageDomainsWithFragmentInput.GetValue parseResult

                if deleteGarbageDomainsWithFragment then
                    let domains = config.DomainsWithFragment

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

                let deleteDomainsHavingAllGarbageQueryParams =
                    domainsHavingAllGarbageQueryParamsInput.GetValue parseResult

                if deleteDomainsHavingAllGarbageQueryParams then
                    let domains = config.DomainsHavingAllQueryParams

                    for { Domain = domain
                          QueryParams = queryParams } in domains do
                        let placeFilter = queryParams |> List.ofSeq |> Place.hasQueryParams
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
                          "opti-dxp.datadoghq.com/logs", Place.withQueryParam
                          "opti-dxp.datadoghq.com/monitors/", Place.withQueryParam
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
                      garbageDomainsWithFragmentInput
                      domainsHavingGarbagePathsInput
                      garbageQueryParamsInput
                      domainsHavingAnyGarbageQueryParamInput
                      domainsHavingAllGarbageQueryParamsInput
                      domainsHavingAnyGarbageFragmentParamInput
                      domainsHavingPathPatternsInput
                      domainsHavingNotFirstThreadPostInput
                      domainsHavingComplexPatternsInput ]
            }

    module Logseq =
        open Fake.IO.FileSystemOperators
        open Logseq

        let private rootDirectoryInput =
            Input.argument "root-directory"
            |> Input.desc "Path to the root directory of Logseq notes"
            |> Input.defaultValue (@"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\" |> DirectoryInfo)
            |> Input.validateDirectoryExists

        let private noopInput =
            Input.option "--noop"
            |> Input.desc "List removable backup directories without deleting them."
            |> Input.defaultValue false

        let private backupDirectoriesRelativeToRoot =
            [ { Path = @"bak\pages\"
                Pattern = "*.md" }
              { Path = @"bak\journals\"
                Pattern = "*.md" }
              { Path = @"bak\logseq\"
                Pattern = "*.edn" }
              { Path = @"bak\logseq\"
                Pattern = "*.css" } ]

        let private options = { ItemsToKeep = 1 }

        let private logseqAction (rootDirectory: DirectoryInfo, noop: bool) =
            let backupDirectoryF options dir =
                if noop then
                    (options, dir)
                    ||> RootBackupDirectory.pendingCleanupDirectories
                    |> map (function
                        | Empty d -> printfn $"%s{d.FullName}"
                        | HasPendingItems { Directory = d } -> printfn $"%s{d.FullName}")
                else
                    (options, dir) ||> RootBackupDirectory.cleanup |> map (printfn "%s")

            backupDirectoriesRelativeToRoot
            |> map (fun d ->
                { d with
                    Path = rootDirectory.FullName </> d.Path })
            |> map (backupDirectoryF options)
            |> ignore

        let command =
            command "logseq" {
                description "Cleanup Logseq backup files"
                inputs (rootDirectoryInput, noopInput)
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
            inputs Input.context
            helpAction
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
            inputs Input.context
            helpAction
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
        let private landingPages =
            [ "https://vneconomy.vn/"
              "https://vneconomy.vn/chung-khoan.htm"
              "https://vneconomy.vn/thi-truong-chung-khoan.htm" ]

        let private ckPages =
            [ 2..10 ] |> List.map (sprintf "https://vneconomy.vn/chung-khoan.htm?page=%d")

        let private ttckPages =
            [ 2..10 ]
            |> List.map (sprintf "https://vneconomy.vn/thi-truong-chung-khoan.htm?page=%d")

        let private urls = landingPages @ ckPages @ ttckPages

        let private blogChungKhoanAction () =
            async {
                let! links = urls |> traverse VnEconomy.extractBlogChungKhoanLinks
                let! articles = links |> List.concat |> distinct |> traverse VnEconomy.loadArticleMetadata

                articles
                |> sortByDescending _.Published
                |> map (fun a ->
                    {| Date = a.Published.Value.Date |> stringf "dd/MM/yyyy"
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
            inputs Input.context
            helpAction
            addCommand BlogChungKhoan.command
        }

module Git =
    module BatchPull =
        open GitBatchPull

        let private rootDirInput =
            Input.argument "rootDir"
            |> Input.desc "Path to the root directory of Git repositories to pull"
            |> Input.defaultValue (@"C:\repo" |> DirectoryInfo)
            |> Input.validateDirectoryExists

        let private exclusionsInput =
            Input.option "-e"
            |> Input.alias "--exclusions"
            |> Input.desc "List of directory names to exclude (space-separated)"
            |> Input.allowMultipleArgumentsPerToken
            |> Input.defaultValue [| "_"; "archived" |]

        let private gitBatchPullAction (rootDir: DirectoryInfo, exclusions: string array) =
            let exclusions = exclusions |> toList

            rootDir.FullName
            |> gitBatchPull exclusions
            |> Observable.subscribe (printfn "%A")
            |> ignore

        let command =
            command "batch-pull" {
                description "Batch pull multiple Git repositories"
                inputs (rootDirInput, exclusionsInput)
                setAction gitBatchPullAction
            }

    let command =
        command "git" {
            description "Git tools"
            noAction
            addCommand BatchPull.command
        }

module Longman =
    open Longman

    let private wordInput = Input.argument "word" |> Input.desc "Word to look up"
    let private longmanAction (word: string) = word |> lookup |> printfn "%A"

    let command =
        command "longman" {
            addAlias "ldoce"
            description "Lookup a word in Longman Dictionary of Contemporary English online"
            inputs wordInput
            setAction longmanAction
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

module Voz =
    module UserPosts =
        open VozUserPosts

        let private usernameInput =
            Input.argument "username" |> Input.desc "The username of the user"

        let private profileUrlInput =
            Input.argument "profileUrl" |> Input.desc "The URL to the user's profile page"

        let private userPostsAction (username: string, profileUrl: string) =
            userPosts username profileUrl |> printfn "%s"

        let command =
            command "user-posts" {
                description "Generate URL to page listing all posts of a given user"
                inputs (usernameInput, profileUrlInput)
                setAction userPostsAction
            }

    let command =
        command "voz" {
            description "VOZ.vn tools"
            inputs Input.context
            helpAction
            addCommand UserPosts.command
        }

module WhatDayOfWeek =
    open WhatDayOfWeek

    let private dayInput =
        Input.argument "day"
        |> Input.desc "Day (1-31)"
        |> Input.validate (fun d ->
            match d with
            | d when d < 1 || d > 31 -> Error "Day must be between 1 and 31"
            | _ -> Ok())

    let private monthInput =
        Input.argumentMaybe "month"
        |> Input.desc "Month (1-12). Default to current month if not provided."
        |> Input.validate (fun m ->
            match m with
            | Some m when m < 1 || m > 12 -> Error "Month must be between 1 and 12"
            | _ -> Ok())

    let private yearInput =
        Input.argumentMaybe "year"
        |> Input.desc "Year (e.g., 2024). Default to current year if not provided."

    let private whatDayOfWeekAction (day: int, month: int option, year: int option) =
        let now = DateTime.Now
        let month = month |> Option.defaultValue now.Month
        let year = year |> Option.defaultValue now.Year

        whatDayOfWeek (year, month, day)
        |> fun r -> printfn $"{day}/{month}/{year} -> %A{r}"

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
/// azure event-grid topic delete topic1 topic2
/// azure storage-account delete storageAccount1 storageAccount2
/// calendar al2dl
/// calendar al2dl 15
/// calendar al2dl 15 8
/// calendar al2dl 15 8 2024
/// calendar dl2al
/// calendar dl2al 31
/// calendar dl2al 31 12
/// calendar dl2al 31 12 2024
/// cleanup rider-log
/// cleanup firefox-history
/// cleanup firefox-history
///     --untitled-entries=false
///     --garbage-domains=false
///     --garbage-domains-with-fragment=false
///     --domains-having-garbage-paths=false
///     --garbage-query-params=false
///     --domains-having-any-garbage-query-param=false
///     --domains-having-all-garbage-query-params=false
///     --domains-having-any-garbage-fragment-param=false
///     --domains-having-path-patterns=false
///     --domains-having-not-first-thread-post=false
///     --domains-having-complex-patterns=false
/// cleanup logseq "C:\logseq-notes" --noop
/// cleanup nuget-cache --noop
/// convert tab2md
/// convert md2tab
/// extract "text with url https://example.com"
/// fetch blog-chung-khoan
/// git batch-pull "C:\repo" -e archived some-other-dirs
/// longman "vocabulary"
/// outdated
/// voz user-posts "Fire Of Heart" https://voz.vn/u/fire-of-heart.873787/
/// what-day-of-week 25
/// what-day-of-week 25 12
/// what-day-of-week 25 12 2024
/// </code>
/// </summary>
[<EntryPoint>]
let main argv =
    rootCommand argv {
        description "Utility tools"
        inputs Input.context
        helpAction

        addCommands
            [ Azure.command
              Calendar.command
              Cleanup.command
              Convert.command
              Extract.command
              Fetch.command
              Git.command
              Longman.command
              Outdated.command
              Voz.command
              WhatDayOfWeek.command ]
    }
