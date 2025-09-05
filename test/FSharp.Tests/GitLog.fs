module GitLog

open Fake.Core
open FSharpPlus

let private matches pattern text =
    let regexMatches pattern textSingleLine = Regex.Matches(textSingleLine, pattern)

    text |> regexMatches pattern |> Seq.map _.Value

let private idPattern =
    let projects = [ "CMS"; "HAPI" ]
    projects |> String.concat "|" |> (fun x -> $"({x})-\d+")

let private extractJiraIds' textSingleLine =
    textSingleLine |> matches idPattern |> distinct |> sort

let extractJiraIds repoName mainBranch featureBranch =
    let mutable ids = Seq.empty

    [ "log"; $"{mainBranch}...{featureBranch}" ]
    |> CreateProcess.fromRawCommand "git"
    |> CreateProcess.withWorkingDirectory (@"C:\repo\" + repoName)
    |> CreateProcess.redirectOutput
    |> CreateProcess.withOutputEventsNotNull
        (fun line -> line |> extractJiraIds' |> Seq.append ids |> (fun xs -> ids <- xs))
        ignore
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

    ids |> distinct |> sort |> toList


open Expecto
open Expecto.Logging

[<Tests>]
let specs =
    let logger = Log.create "GitLog"
    let writeln = Message.eventX >> logger.info

    // theory data
    let extractJiraIdsTheoryData =
        [ "content-rest-api", "origin/master", "develop"
          "content-platform", "origin/master", "origin/release/12.18.0" ]

    testTheory "GitLog.Extract Jira IDs" extractJiraIdsTheoryData (fun (repoName, mainBranch, featureBranch) ->
        (repoName, mainBranch, featureBranch)
        |||> extractJiraIds
        |> (sprintf "%A" >> writeln))
