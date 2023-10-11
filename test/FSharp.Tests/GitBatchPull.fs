module GitBatchPull

open System
open System.IO
open System.Reactive.Linq
open Fake.Core
open FSharpPlus
open Microsoft.FSharp.Collections
open Xunit
open Xunit.Abstractions

type Output = private Output of string

// Branch
type Branch = private Branch of string

module Branch =
    let name (Branch branch) = branch

    let isMajorBranch (Branch branch) =
        [ "master"; "main"; "develop" ] |> List.contains branch

// Repo
type Repo = private Repo of string

module Repo =
    let isGitRepo dir =
        [| dir; ".git" |] |> Path.Combine |> Directory.Exists

type private GitBatchPull = string -> IObservable<Repo * Branch * Output>

let gitBatchPull: GitBatchPull =
    let toOutput = String.trimWhiteSpaces >> Output

    let getCurrentBranch (Repo repo) =
        CreateProcess.fromRawCommandLine "git" "branch --show-current"
        |> CreateProcess.withWorkingDirectory repo
        |> CreateProcess.redirectOutput
        |> Proc.run
        |> fun result ->
            match result.ExitCode with
            | 0 -> result.Result.Output |> String.trimWhiteSpaces |> Branch |> Ok
            | _ -> result.Result.Error |> toOutput |> Error

    let gitPull (Repo repo) =
        CreateProcess.fromRawCommandLine "git" "pull --prune"
        |> CreateProcess.withWorkingDirectory repo
        |> CreateProcess.redirectOutput
        |> Proc.run
        |> fun result ->
            match result.ExitCode with
            | 0 -> result.Result.Output |> toOutput |> Ok
            | _ -> result.Result.Error |> toOutput |> Error

    let gitPullBranch repo branch =
        if branch |> Branch.isMajorBranch then
            repo |> gitPull
        else
            $"Current branch '{branch |> Branch.name}' is not one of the major branches (master, main, develop)."
            |> Output
            |> Error
        |> Result.map (tuple2 branch)
        |> Result.mapError (tuple2 branch)

    let gitPull' repo =
        repo
        |> getCurrentBranch
        |> Result.mapError (fun output -> (Branch "", output))
        |> Result.bind (gitPullBranch repo)

    fun parentDir ->
        parentDir
        |> Directory.EnumerateDirectories
        |> filter Repo.isGitRepo
        |> map Repo
        |> Observable.ToObservable
        |> map (fun repo -> (repo, repo |> gitPull' |> either id id))
        |> map (fun (repo, (branch, output)) -> (repo, branch, output))

type Tests(helper: ITestOutputHelper) =
    [<Theory>]
    [<InlineData(@"C:\repo\")>]
    [<InlineData(@"C:\repo\archived\")>]
    let ``Perform "git pull" against all subdirectories`` parentDir =
        parentDir
        |> gitBatchPull
        |> Observable.subscribe (sprintf "%A" >> helper.WriteLine)
