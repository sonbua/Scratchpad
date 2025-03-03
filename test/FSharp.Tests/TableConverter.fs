module TableConverter

// #r "nuget: Fake.Core.Target"
// #r "nuget: FSharpPlus"

open System
open FSharpPlus

module Regex =
    open System.Text.RegularExpressions

    let replace pattern (replacement: string) text : string =
        Regex.Replace(text, pattern, replacement)

module internal Line =
    let tabToMarkdown line : string =
        line |> Regex.replace "\\t" " | " |> sprintf "| %s |"

let private newLines = [| "\r\n"; "\n" |]

let tabularToMarkdown (text: string) : string =
    text.Split(newLines, StringSplitOptions.RemoveEmptyEntries)
    |> map String.trimWhiteSpaces
    |> map Line.tabToMarkdown
    |> fun lines -> String.Join(Environment.NewLine, lines)
