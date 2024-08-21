module GitBatchPull

open System
open System.IO
open System.Reactive.Linq
open Fake.Core
open FSharpPlus

// Repo
type Repo = private Repo of string

module Repo =
    let create dir =
        match Path.Combine [| dir; ".git" |] |> Directory.Exists with
        | true -> Ok(Repo dir)
        | false -> Error $"'{dir}' is not a Git repository."

    let value (Repo name) = name

// Branch
type Branch = { Name: string; Repo: Repo }

module Branch =
    let private majorBranchNames = [ "master"; "main"; "develop" ]

    let create repo branchName =
        { Name = branchName |> String.trimWhiteSpaces
          Repo = repo }

    let isMajorBranch branch =
        majorBranchNames |> List.contains branch.Name

type GitPullError =
    | NotAGitRepo of string
    | GetCurrentBranchError of (Repo * string)
    | NotAMajorBranch of (Branch * string)
    | CannotPull of (Branch * string)

let private toRepo: string -> Result<Repo, GitPullError> =
    Repo.create >> Result.mapError NotAGitRepo

let private getCurrentBranch repo : Result<Branch, GitPullError> =
    CreateProcess.fromRawCommandLine "git" "branch --show-current"
    |> CreateProcess.withWorkingDirectory (repo |> Repo.value)
    |> CreateProcess.redirectOutput
    |> Proc.run
    |> fun result ->
        match result.ExitCode with
        | 0 -> result.Result.Output |> Branch.create repo |> Ok
        | _ ->
            (repo, result.Result.Error)
            |> GetCurrentBranchError
            |> Error

let private rejectNonMajorBranch branch : Result<Branch, GitPullError> =
    if branch |> Branch.isMajorBranch then
        Ok branch
    else
        $"Current branch, '{branch.Name}', is not one of the major branches (master, main, develop)."
        |> tuple2 branch
        |> NotAMajorBranch
        |> Error

let private gitPullBranch branch : Result<Branch * string, GitPullError> =
    CreateProcess.fromRawCommandLine "git" "pull --prune"
    |> CreateProcess.withWorkingDirectory (branch.Repo |> Repo.value)
    |> CreateProcess.redirectOutput
    |> Proc.run
    |> fun result ->
        match result.ExitCode with
        | 0 -> (branch, result.Result.Output) |> Ok
        | _ -> CannotPull(branch, result.Result.Error) |> Error

let private gitPull: string -> Result<Branch * string, GitPullError> =
    toRepo
    >=> getCurrentBranch
    >=> rejectNonMajorBranch
    >=> gitPullBranch

let gitBatchPull: string -> IObservable<Result<Branch * string, GitPullError>> =
    Directory.EnumerateDirectories
    >> Observable.ToObservable
    >> map gitPull


open Expecto
open Expecto.Logging

[<Tests>]
let specs =
    let logger = Log.create "GitBatchPull"
    let writeln = Message.eventX >> logger.info
    // theory data
    let subdirsTheoryData = [
        @"C:\repo\"
        @"C:\repo\_\"
    ]
    testTheory "Perform 'git pull' against all subdirectories" subdirsTheoryData (fun parentDir ->
        parentDir |> gitBatchPull |> Observable.subscribe (sprintf "%A" >> writeln))
