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
    | HasPendingItems of BackupDirectory * itemsToKeep: int
    | Empty of DirectoryInfo

module PendingCleanupDirectory =
    let inline private moreThan n = (<) n

    let create options dir : PendingCleanupDirectory option =
        if dir.Directory |> DirectoryInfo.getFiles |> Array.isEmpty then
            dir.Directory |> Empty |> Some
        elif dir |> BackupDirectory.items |> length |> moreThan options.ItemsToKeep then
            (dir, options.ItemsToKeep) |> HasPendingItems |> Some
        else
            None

    let cleanup dir : string list =
        match dir with
        | HasPendingItems(d, itemsToKeep) -> BackupDirectory.cleanup itemsToKeep d
        | Empty d ->
            d.Delete()
            [ d.FullName ]

type RootBackupDirectory =
    { RootDirectory: DirectoryInfo
      FilePattern: string }

module RootBackupDirectory =
    let pendingCleanupDirectories options rootDir : PendingCleanupDirectory list =
        rootDir.RootDirectory
        |> DirectoryInfo.getSubDirectories
        |> toList
        |> map (BackupDirectory.create rootDir.FilePattern)
        |> choose (PendingCleanupDirectory.create options)

    let cleanup options rootDir : string list list =
        rootDir
        |> pendingCleanupDirectories options
        |> map PendingCleanupDirectory.cleanup


open Expecto
open Expecto.Logging

[<Tests>]
let specs =
    test "Logseq" {
        let rootBackupDirectories =
            [ { RootDirectory = DirectoryInfo @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\pages\"; FilePattern = "*.md" }
              { RootDirectory = DirectoryInfo @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\journals\"; FilePattern = "*.md" }
              { RootDirectory = DirectoryInfo @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\logseq\"; FilePattern = "*.edn" }
              { RootDirectory = DirectoryInfo @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\logseq\"; FilePattern = "*.css" } ]

        let options = { ItemsToKeep = 1 }
        let logger = Log.create "Logseq"
        let writeln = Message.eventX >> logger.info

        rootBackupDirectories
        |> map (RootBackupDirectory.pendingCleanupDirectories options)
        // |> map (RootBackupDirectory.cleanup options)
        |> (sprintf "%A" >> writeln)
    }
