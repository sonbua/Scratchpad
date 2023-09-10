namespace Firefox

open System
open System.IO
open System.Text.RegularExpressions
open Reusables

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
        |> List.map (fun x -> Regex(x, RegexOptions.Compiled))

    let isWhitelisted (domain: string) =
        whitelist |> List.exists (fun pattern -> domain |> pattern.IsMatch)

module Data =
    let tryDelete (dir: DirectoryInfo) =
        try
            dir.Delete(true)
        with _ ->
            ()

    let path =
        "C:\\Users\\song\\AppData\\Roaming\\librewolf\\Profiles\\5d5x89ro.default-default\\storage\\default\\"

    let action =
        let shouldRemove = Domain.isWhitelisted >> not

        path
        |> Directory.EnumerateDirectories
        |> Seq.map DirectoryInfo
        |> Seq.where ((fun x -> x.Name) >> shouldRemove)
        |> Seq.map (tap Console.WriteLine)
        |> Seq.toList

// side-effect
// action |> List.iter tryDelete
