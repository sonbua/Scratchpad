module TableConverter

// #r "nuget: Fake.Core.Target"
// #r "nuget: FSharpPlus"

open System
open FSharpPlus

module internal Regex =
    open System.Text.RegularExpressions

    let replace pattern (replacement: string) text : string =
        Regex.Replace(text, pattern, replacement)

module internal Line =
    let tabToMarkdown line : string =
        line |> String.replace "\t" " | " |> sprintf "| %s |"

    let markdownToTab (line: string) : string =
        line.Split("|", StringSplitOptions.RemoveEmptyEntries)
        |> map String.trimWhiteSpaces
        |> String.concat "\t"

let private newLines = [| "\r\n"; "\n"; Environment.NewLine |]

let tabularToMarkdown (text: string) : string =
    text.Split(newLines, StringSplitOptions.RemoveEmptyEntries)
    |> map (String.trim [ ' '  ])
    |> map Line.tabToMarkdown
    |> fun lines -> String.Join(Environment.NewLine, lines)

let markdownToTabular (text: string) : string =
    text.Split(newLines, StringSplitOptions.RemoveEmptyEntries)
    |> map String.trimWhiteSpaces
    |> map Line.markdownToTab
    |> fun lines -> String.Join(Environment.NewLine, lines)
