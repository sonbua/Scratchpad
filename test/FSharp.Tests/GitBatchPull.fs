module GitBatchPull

open System.Reactive.Linq
open Fake.Core
open FSharpPlus
open Reusables.IO

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
        | _ -> (repo, result.Result.Error) |> GetCurrentBranchError |> Error

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
    toRepo >=> getCurrentBranch >=> rejectNonMajorBranch >=> gitPullBranch

let gitBatchPull: string list -> string -> IObservable<Result<Branch * string, GitPullError>> =
    fun exclusions parentDir ->
        parentDir
        |> DirectoryInfo
        |> DirectoryInfo.getSubDirectories
        |> filter (fun d -> exclusions |> notF (List.contains d.Name))
        |> map _.FullName
        |> Observable.ToObservable
        |> map gitPull
