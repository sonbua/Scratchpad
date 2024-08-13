module Logseq

open System.IO
open Fake.IO
open FSharpPlus

type Options = { ItemsToKeep: int }

type BackupDirectory = {
    Directory: DirectoryInfo
    FilePattern: string
}

module BackupDirectory =
    /// Lists all the backup items within the backup directory.
    let items (dir: BackupDirectory): FileInfo list =
        dir.Directory |> DirectoryInfo.getMatchingFiles dir.FilePattern |> toList

    /// Deletes all backup items except the most recent ones.
    /// The number of skipped items is provided by <paramref name="options"/>.
    /// <param name="options"></param>
    let cleanup options (dir: BackupDirectory): string list =
        let deleting = dir |> items |> rev |> List.skipSafe options.ItemsToKeep |> map _.FullName

        deleting |> map File.delete |> ignore

        deleting

type RootBackupDirectory = {
    RootDirectory: DirectoryInfo
    FilePattern: string
}

module RootBackupDirectory =
    let private moreThan n = (<) n

    let pendingCleanupFolders options rootDir: BackupDirectory list =
        rootDir.RootDirectory
        |> DirectoryInfo.getSubDirectories
        |> toList
        |> map (fun x -> { Directory = x; FilePattern = rootDir.FilePattern })
        |> filter (BackupDirectory.items >> length >> moreThan options.ItemsToKeep)

    let cleanup options rootDir =
        rootDir |> pendingCleanupFolders options |> map (BackupDirectory.cleanup options)

let rootBackupDirectories =
    [
        { RootDirectory = DirectoryInfo @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\pages\"; FilePattern = "*.md" }
        { RootDirectory = DirectoryInfo @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\journals\"; FilePattern = "*.md" }
        { RootDirectory = DirectoryInfo @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\logseq\"; FilePattern = "*.edn" }
        { RootDirectory = DirectoryInfo @"C:\Users\song\OneDrive - Episerver\doc\Note\logseq\bak\logseq\"; FilePattern = "*.css" }
    ]
let options = { ItemsToKeep = 1 }

// Usage
// rootBackupDirectories |> map (RootBackupDirectory.pendingCleanupFolders options)
// rootBackupDirectories |> map (RootBackupDirectory.cleanup options)
