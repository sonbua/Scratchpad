namespace Firefox

open System
open System.IO
open System.Text.RegularExpressions
open FSharpPlus

module Domain =
    let private whitelist =
        [ "moz-extension"
          "accounts.google.com\^userContextId=1"
          "app.slack.com"
          "chat.zalo.me\^userContextId=1"
          "duckduckgo.com"
          "ep\.se"
          "github.com"
          "keybr.com\^userContextId=1"
          "localhost"
          "reddit.com\^userContextId=1"
          "stackblitz.com"
          "todoist.com"
          "write.as" ]
        |> map (fun x -> Regex(x, RegexOptions.Compiled))

    let isWhitelisted (domain: string) =
        whitelist |> exists (fun pattern -> domain |> pattern.IsMatch)

module Data =
    let tryDelete (dir: DirectoryInfo) =
        try
            dir.Delete(true)
        with _ ->
            ()

    let path =
        @"C:\Users\song\AppData\Roaming\librewolf\Profiles\5d5x89ro.default-default\storage\default\"

    let action =
        let shouldRemove = Domain.isWhitelisted >> not

        path
        |> Directory.EnumerateDirectories
        |> map DirectoryInfo
        |> filter ((fun (x: DirectoryInfo) -> x.Name) >> shouldRemove)
        |> iter (string >> Console.WriteLine)

// Side effect
// action |> List.iter tryDelete
