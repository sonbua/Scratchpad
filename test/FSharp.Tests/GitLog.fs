module GitLog

open System.Text.RegularExpressions
open Fake.Core
open Reusables
open Xunit
open Xunit.Abstractions

let private matches pattern text =
    let regexMatches pattern textSingleLine = Regex.Matches(textSingleLine, pattern)

    text |> regexMatches pattern |> Seq.map (fun m -> m.Value)

let private extractJiraIds' textSingleLine =
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
    |> CreateProcess.withOutputEventsNotNull
        (fun line -> line |> extractJiraIds' |> Seq.append ids |> (fun xs -> ids <- xs))
        ignore
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

    ids |> Seq.distinct |> Seq.sort |> Seq.toList

type Tests(helper: ITestOutputHelper) =
    [<Theory>]
    [<InlineData("content-rest-api", "origin/master", "develop")>]
    [<InlineData("content-platform", "origin/master", "origin/release/12.18.0")>]
    let ``Extract Jira IDs`` repoName mainBranch featureBranch =
        extractJiraIds repoName mainBranch featureBranch
        |> sprintf "%A"
        |> helper.WriteLine
