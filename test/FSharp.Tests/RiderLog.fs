module RiderLog

// #r "nuget: Fake.Core.Target"
// #r "nuget: FSharpPlus"

open System
open System.IO
open FSharpPlus
open Fake.IO

type CleanupOptions =
    { LogRootDir: string
      CutOffTimestamp: DateTime }

let private beforeTimestamp timestamp arg : bool = arg < timestamp

let private tryFindSubDirByName dirName parentDir : DirectoryInfo option =
    parentDir
    |> DirectoryInfo.getSubDirectories
    |> tryFind (fun dir -> dir.Name = dirName)

let private cleanupDirs beforeCutOffTimestamp rootDir : string list =
    let deletingThreadDumps rootDir =
        rootDir
        |> DirectoryInfo.getSubDirectories
        |> filter (_.Name >> String.startsWith "threadDumps-freeze-")
        |> toList

    let deletingDebuggerWorker rootDir =
        rootDir
        |> tryFindSubDirByName "DebuggerWorker"
        |> Option.map DirectoryInfo.getSubDirectories
        |> Option.defaultValue [||]
        |> toList

    let deletingDirs =
        [ rootDir ]
        |> List.apply [ deletingThreadDumps; deletingDebuggerWorker ]
        |> List.concat
        |> filter (_.LastWriteTime >> beforeCutOffTimestamp)
        |> map _.FullName

    deletingDirs |> map Directory.delete |> ignore

    deletingDirs

let private cleanupFiles beforeCutOffTimestamp rootDir : string list =
    let dirsToCleanup rootDir =
        [ rootDir ]
        |> List.apply
            [ Some
              tryFindSubDirByName "SolutionBuilder"
              tryFindSubDirByName "BackendThreadDump" ]
        |> choose id

    rootDir
    |> dirsToCleanup
    |> List.collect (DirectoryInfo.getFiles >> toList)
    |> filter (_.LastWriteTime >> beforeCutOffTimestamp)
    |> map (_.FullName >> IO.File.tryDelete)
    |> choose Result.toOption

let cleanup options : string list =
    let logRootDir =
        let dir = options.LogRootDir |> DirectoryInfo.ofPath
        dir |> DirectoryInfo.ensure
        dir

    let beforeCutOffTimestamp = options.CutOffTimestamp |> beforeTimestamp
    let cleanupDirs' = cleanupDirs beforeCutOffTimestamp
    let cleanupFiles' = cleanupFiles beforeCutOffTimestamp

    [ logRootDir ] |> List.apply [ cleanupDirs'; cleanupFiles' ] |> List.concat
