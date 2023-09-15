module Scratchpad

open System.Text.RegularExpressions
open Fake.Core
open Reusables

let matches pattern text =
    let regexMatches pattern textSingleLine = Regex.Matches(textSingleLine, pattern)

    text |> regexMatches pattern |> Seq.map (fun m -> m.Value)

let extractJiraIds' textSingleLine =
    let idPattern =
        let projects = [ "CMS"; "HAPI" ]
        projects |> String.concat "|" |> (fun x -> $"({x})-\d+")

    textSingleLine |> matches idPattern |> Seq.distinct |> Seq.sort

let extractJiraIds repoName mainBranch featureBranch =
    let mutable ids = Seq.empty

    [ "log"; $"{mainBranch}...{featureBranch}" ]
    |> CreateProcess.fromRawCommand "git"
    |> CreateProcess.withWorkingDirectory (@"C:\repo\" + repoName)
    |> CreateProcess.redirectOutput
    |> CreateProcess.withOutputEvents
        (fun line ->
            line
            |> String.emptyIfNull
            |> extractJiraIds'
            |> Seq.append ids
            |> fun xs -> ids <- xs)
        ignore
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

    ids |> Seq.distinct |> Seq.sort |> Seq.toList

let headless = extractJiraIds "content-rest-api" "origin/master" "develop"
let core = extractJiraIds "content-platform" "origin/master" "release/12.18.0"
