module RiderLog

open FSharpPlus
open Reusables.IO

type CleanupOptions =
    { LogRootDir: string
      CutOffTimestamp: DateTime }

let private beforeTimestamp timestamp arg : bool = arg < timestamp

let private cleanupDirs beforeCutOffTimestamp rootDir : string list =
    let deletingThreadDumps rootDir =
        rootDir
        |> DirectoryInfo.getSubDirectories
        |> toList
        |> filter (_.Name >> String.startsWith "threadDumps-freeze-")

    let deletingDebuggerWorker rootDir =
        rootDir
        |> DirectoryInfo.tryFind "DebuggerWorker"
        |> Option.map (DirectoryInfo.getSubDirectories >> toList)
        |> Option.defaultValue []

    let deletingIndexingDiagnostic rootDir =
        rootDir
        |> DirectoryInfo.tryFind "indexing-diagnostic"
        |> Option.map (DirectoryInfo.getSubDirectories >> toList)
        |> Option.defaultValue []

    let deletingDirs =
        [ rootDir ]
        |> List.apply [ deletingThreadDumps; deletingDebuggerWorker; deletingIndexingDiagnostic ]
        |> List.concat
        |> filter (_.LastWriteTime >> beforeCutOffTimestamp)
        |> map _.FullName

    deletingDirs |> map Directory.delete |> ignore

    deletingDirs

let private cleanupFiles beforeCutOffTimestamp rootDir : string list =
    let dirs =
        [ rootDir ]
        |> List.apply
            [ Some
              DirectoryInfo.tryFind "BackendThreadDump"
              DirectoryInfo.tryFindPath [ "BackendThreadDump"; "WorkerLogs" ]
              DirectoryInfo.tryFind "indexing-diagnostic"
              DirectoryInfo.tryFind "JetBrains.DPA.Protocol.Backend"
              DirectoryInfo.tryFind "MsBuildTask"
              DirectoryInfo.tryFind "SolutionBuilder"
              DirectoryInfo.tryFindPath [ "UnitTestLogs"; "Sessions" ] ]

    let dirsWithSubDirs: DirectoryInfo option list =
        [ rootDir ]
        |> List.apply
            [ DirectoryInfo.tryFind "ProcessEnumerator.Worker"
              DirectoryInfo.tryFind "RoslynWorker"
              DirectoryInfo.tryFind "SqlProjectsWorker" ]
        |> List.collect (traverse (DirectoryInfo.getSubDirectories >> toList))

    let possibleDirectoriesToLookup = dirs @ dirsWithSubDirs

    possibleDirectoriesToLookup
    |> choose id
    |> List.collect (DirectoryInfo.getFiles >> toList)
    |> filter (_.LastWriteTime >> beforeCutOffTimestamp)
    |> map (_.FullName >> File.tryDelete)
    |> choose Result.toOption

let cleanup options : string list =
    let logRootDir =
        let dir = options.LogRootDir |> DirectoryInfo
        dir |> DirectoryInfo.ensure
        dir

    let beforeCutOffTimestamp = options.CutOffTimestamp |> beforeTimestamp
    let cleanupDirs' = cleanupDirs beforeCutOffTimestamp
    let cleanupFiles' = cleanupFiles beforeCutOffTimestamp

    [ logRootDir ] |> List.apply [ cleanupDirs'; cleanupFiles' ] |> List.concat
