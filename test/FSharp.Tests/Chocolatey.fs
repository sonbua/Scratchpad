module Chocolatey

module Outdated =
    open System
    open Fake.Core
    open FSharpPlus

    let private noSpace = String.contains ' ' >> not

    let private containsOutdatedPackage split =
        (split |> length) = 4 && split |> head |> noSpace

    /// <summary>Parses output line of `choco outdated` command and returns package name for valid output line.</summary>
    /// <param name="outdatedOutputLine">
    /// Valid output: <code>7-taskbar-tweaker|5.15.1|5.15.2|false</code>
    /// Returns: <code>7-taskbar-tweaker</code>
    /// </param>
    let private parsePackageName (outdatedOutputLine: string) : string option =
        let split = outdatedOutputLine.Split('|')

        if split |> containsOutdatedPackage then
            split |> head |> Some
        else
            None

    let toUpgradeCommand (outdatedOutput: string) : string option =
        let installingPackages =
            outdatedOutput.Split(Environment.NewLine) |> choose parsePackageName

        if installingPackages |> Array.isEmpty then
            None
        else
            installingPackages |> String.concat " " |> sprintf "choco upgrade -y %s" |> Some

    let private toUpgradeCommandR =
        toUpgradeCommand
        >> Option.toResultWith "Chocolatey has determined no package is outdated."

    let run =
        fun () ->
            CreateProcess.fromRawCommandLine "choco" "outdated"
            |> CreateProcess.redirectOutput
            |> Proc.run
            |> fun result ->
                match result.ExitCode with
                | 0 -> Ok <| result.Result.Output
                | _ -> Error <| result.Result.Error
            |> Result.bind toUpgradeCommandR


module Tests =
    open Expecto
    open Expecto.Flip

    [<Tests>]
    let specs =
        testList "Chocolatey"
            [ // theory data
              let outdatedOutputTheoryData =
                  [ """Chocolatey v2.3.0
Outdated Packages
 Output is package name | current version | available version | pinned?

obs-studio.install|30.1.2|30.2.3|false
python3|3.12.5|3.12.6|false
python312|3.12.5|3.12.6|false

Chocolatey has determined 3 package(s) are outdated.""",
                    Some "choco upgrade -y obs-studio.install python3 python312"
                    "Chocolatey has determined no package is outdated.", None
                    " Output is package name | current version | available version | pinned?", None ]

              testTheory
                  "Given output of `choco outdated` command"
                  outdatedOutputTheoryData
                  (fun (output, upgradeCommand) ->
                      output
                      |> Outdated.toUpgradeCommand
                      |> Expect.equal "Should return correct `choco upgrade` command" upgradeCommand) ]
