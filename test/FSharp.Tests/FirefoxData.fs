namespace Firefox

open System
open System.IO
open System.Text.RegularExpressions

module Data =
    let path =
        "C:\\Users\\song\\AppData\\Roaming\\librewolf\\Profiles\\5d5x89ro.default-default\\storage\\default\\"

    let whitelist =
        [ "moz-extension"
          "accounts.google.com\^userContextId=1"
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

    let isWhitelisted domain =
        whitelist
        |> List.exists (fun x -> domain |> x.IsMatch)

    let tryDelete (dir: DirectoryInfo) =
        try
            dir.Delete(true)
        with
        | _ -> ()

    let tap action value =
        action value |> ignore
        value

    let action =
        let notExcluded = isWhitelisted >> not

        path
        |> Directory.EnumerateDirectories
        |> Seq.toList
        |> List.map DirectoryInfo
        |> List.where (fun x -> x.Name |> notExcluded)
        |> List.map (tap Console.WriteLine)

// side-effect
// action |> Seq.iter tryDelete
