[<AutoOpen>]
module Reusables

let inline stringf format (x: 'a) =
    (^a: (member ToString: string -> string) (x, format))

module Seq =
    let any xs = xs |> Seq.isEmpty |> not

module Map =
    let change2 (key: 'Key) (newValue: 'T) =
        Map.change key (Option.map (fun _ -> newValue))

module String =
    open System

    let toOption text =
        match text with
        | null -> None
        | _ -> Some text

    let defaultIfNull defaultValue text =
        match text with
        | null -> defaultValue
        | _ -> text

    let emptyIfNull = defaultIfNull ""

    let splitWith (separators: string array) (options: StringSplitOptions) (text: string) =
        text.Split(separators, options)

    let splitCharWith (separators: char array) (options: StringSplitOptions) (text: string) =
        text.Split(separators, options)

    let splitWithRemovingEmptyEntries (separators: string array) (text: string) =
        text |> splitWith separators StringSplitOptions.RemoveEmptyEntries

    let splitCharWithRemovingEmptyEntries (separators: char array) (text: string) =
        text |> splitCharWith separators StringSplitOptions.RemoveEmptyEntries

module List =
    /// Like List.skip but rather than throwing System.ArgumentException, it returns an empty list if the number of
    /// skipped items is greater than the length of the list.
    let skipSafe count list =
        if list |> List.length |> (<) count then
            list |> List.skip count
        else
            []

module Result =
    let isOk =
        function
        | Ok _ -> true
        | Error _ -> false

    let isError =
        function
        | Error _ -> true
        | Ok _ -> false

module Uri =
    open System
    open System.Text.RegularExpressions

    [<Literal>]
    let private patternString =
        "(?:http|ftp)s?:\\/\\/"
        + "(?:(?:[A-Z0-9](?:[A-Z0-9-]{0,61}[A-Z0-9])?\\.)+(?:[A-Z]{2,6}\\.?|[A-Z0-9-]{2,}\\.?)|" // domain
        + "localhost|"
        + "\\d{1,3}\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})" // IP
        + "(?::\\d{2,6})?" // port
        + "(\/[=&\\w\\-\\.\\:\\#\\?\\\\/\\~]*)?"

    [<Literal>]
    let private patternStringExact = "^" + patternString + "$"

    let private urlPattern =
        Regex(
            patternString,
            RegexOptions.IgnoreCase
            ||| RegexOptions.ExplicitCapture
            ||| RegexOptions.Compiled
        )

    let private urlPatternExact =
        Regex(
            patternStringExact,
            RegexOptions.IgnoreCase
            ||| RegexOptions.ExplicitCapture
            ||| RegexOptions.Compiled
        )

    let isValid (input: string) = urlPatternExact.IsMatch(input)

    let extractUriStrings = urlPattern.Matches >> Seq.map _.Value

    let extract = extractUriStrings >> Seq.map Uri

module HtmlNode =
    open FSharp.Data

    let cssSelectR selector node = HtmlNode.cssSelect node selector

module IO =
    module File =
        open Fake.IO

        let tryDelete path : Result<string, exn> =
            try
                File.delete path
                Ok path
            with exn ->
                Error exn


open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection

/// Credit: https://stackoverflow.com/a/11798829
let rec ofCase =
    function
    | Lambda(_, expr)
    | Let(_, _, expr) -> ofCase expr
    | NewTuple exprs -> fun value -> exprs |> Seq.map ofCase |> Seq.exists ((|>) value)
    | NewUnionCase(uci, _) ->
        let utr = FSharpValue.PreComputeUnionTagReader uci.DeclaringType
        box >> utr >> (=) uci.Tag
    | _ -> failwith "Expression is not union case."
