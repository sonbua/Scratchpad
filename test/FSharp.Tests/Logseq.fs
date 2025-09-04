module Logseq

open System.IO
open Fake.IO
open FSharpPlus

type Options = { ItemsToKeep: int }

type BackupDirectory =
    { Directory: DirectoryInfo
      FilePattern: string }

module BackupDirectory =
    let create filePattern dir : BackupDirectory =
        { Directory = dir
          FilePattern = filePattern }

    /// Lists all the backup items within a backup directory.
    let items dir : FileInfo list =
        dir.Directory |> DirectoryInfo.getMatchingFiles dir.FilePattern |> toList

    /// Deletes all backup items except the most recent ones.
    let cleanup itemsToKeep dir : string list =
        let deleting = dir |> items |> rev |> List.skipSafe itemsToKeep |> map _.FullName

        deleting |> map File.delete |> ignore
        deleting

type PendingCleanupDirectory =
    | HasPendingItems of BackupDirectory
    | Empty of DirectoryInfo

module PendingCleanupDirectory =
    let create options dir : PendingCleanupDirectory option =
        if dir.Directory |> DirectoryInfo.getFiles |> Array.isEmpty then
            dir.Directory |> Empty |> Some
        elif dir |> BackupDirectory.items |> length |> isGreaterThan options.ItemsToKeep then
            dir |> HasPendingItems |> Some
        else
            None

    let cleanup options dir : string list =
        match dir with
        | HasPendingItems d -> d |> BackupDirectory.cleanup options.ItemsToKeep
        | Empty d ->
            d.Delete()
            [ d.FullName ]

type RootBackupDirectory = { Path: string; Pattern: string }

module RootBackupDirectory =
    let pendingCleanupDirectories options rootDir : PendingCleanupDirectory list =
        rootDir.Path
        |> DirectoryInfo.ofPath
        |> DirectoryInfo.getSubDirectories
        |> toList
        |> map (BackupDirectory.create rootDir.Pattern)
        |> choose (PendingCleanupDirectory.create options)

    let cleanup options rootDir : string list =
        rootDir
        |> pendingCleanupDirectories options
        |> List.map (PendingCleanupDirectory.cleanup options)
        |> List.concat
