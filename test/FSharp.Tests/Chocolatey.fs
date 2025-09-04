module Chocolatey

open FSharpPlus

module ProcessResult =
    open Fake.Core

    let toResult result : Result<string, string> =
        match result.ExitCode with
        | 0 -> Ok <| result.Result.Output
        | _ -> Error <| result.Result.Error

module Outdated =
    open System
    open Fake.Core

    let private noSpace: string -> bool = String.contains ' ' >> not

    let private containsOutdatedPackage split : bool =
        (split |> length) = 4 && split |> head |> noSpace

    /// <summary>Parses output line of `choco outdated` command and returns package name for valid output line.</summary>
    /// <param name="outdatedOutputLine">
    /// Valid output: <code>7-taskbar-tweaker|5.15.1|5.15.2|false</code>
    /// Returns: <code>7-taskbar-tweaker</code>
    /// </param>
    let private parsePackageName (outdatedOutputLine: string) : string option =
        outdatedOutputLine
        |> String.split '|'
        |> Some
        |> Option.filter containsOutdatedPackage
        |> Option.map head

    let toUpgradeCommand (outdatedOutput: string) : Result<string, string> =
        outdatedOutput
        |> String.splitStr Environment.NewLine
        |> choose parsePackageName
        |> function
            | [] -> Error "Chocolatey has determined no package is outdated."
            | ps -> ps |> String.concat " " |> sprintf "choco upgrade -y %s" |> Ok

    let run () : Result<string, string> =
        CreateProcess.fromRawCommandLine "choco" "outdated"
        |> CreateProcess.redirectOutput
        |> Proc.run
        |> ProcessResult.toResult
        |> Result.bind toUpgradeCommand


open Expecto
open Expecto.Flip

[<Tests>]
let specs =
    testList
        "Chocolatey"
        [ // theory data
          let outdatedOutputTheoryData =
              [ """Chocolatey v2.3.0
Outdated Packages
 Output is package name | current version | available version | pinned?

obs-studio.install|30.1.2|30.2.3|false
python3|3.12.5|3.12.6|false
python312|3.12.5|3.12.6|false

Chocolatey has determined 3 package(s) are outdated.""",
                Ok "choco upgrade -y obs-studio.install python3 python312"

                "Chocolatey has determined no package is outdated.",
                Result.Error "Chocolatey has determined no package is outdated."

                " Output is package name | current version | available version | pinned?",
                Result.Error "Chocolatey has determined no package is outdated." ]

          testTheory
              "Given output of `choco outdated` command"
              outdatedOutputTheoryData
              (fun (output, upgradeCommand) ->
                  output
                  |> Outdated.toUpgradeCommand
                  |> Expect.equal "Should return correct `choco upgrade` command" upgradeCommand) ]
