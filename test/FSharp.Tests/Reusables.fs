[<AutoOpen>]
module Reusables

let inline isGreaterThan n = (<) n

let inline stringf format (x: 'a) =
    (^a: (member ToString: string -> string) (x, format))

let inline notF f x = f x |> not

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
        if list |> List.length |> isGreaterThan count then
            list |> List.skip count
        else
            []

    let intersect list1 list2 =
        let rec aux list1 list2 acc =
            match list1 with
            | head1 :: tail1 ->
                if List.contains head1 list2 then
                    aux tail1 list2 (head1 :: acc)
                else
                    aux tail1 list2 acc
            | [] -> List.rev acc

        aux list1 list2 []

module Regex =
    open System.Text.RegularExpressions

    /// Indicates whether the specified regular expression finds a match in the specified input string.
    let isMatch pattern (input: string) : bool = Regex.IsMatch(input, pattern)

    /// Searches the specified input string for the first occurrence of the specified regular expression.
    let findMatch pattern (input: string) : Match = Regex.Match(input, pattern)

module Result =
    let isOk =
        function
        | Ok _ -> true
        | Error _ -> false

    let isError =
        function
        | Error _ -> true
        | Ok _ -> false

module IO =
    open System.IO

    module Path =
        /// Combines two strings into a path.
        let combine path1 path2 = Path.Combine(path1, path2)

        /// Combines a list of strings into a path.
        let combineN (paths: string list) : string = paths |> List.reduce combine

    module DirectoryInfo =
        /// Gets a value indicating whether the directory exists.
        let exists (dir: DirectoryInfo) = dir.Exists

        /// Returns an array of Directories in the current directory.
        let getSubDirectories (dir: DirectoryInfo) : DirectoryInfo[] = dir.GetDirectories()

        let ofPath2 path =
            let dir = DirectoryInfo path
            if dir |> exists then Some dir else None

        let tryFind dirName (parentDir: DirectoryInfo) : DirectoryInfo option =
            dirName |> Path.combine parentDir.FullName |> ofPath2

        let tryFindPath (dirPath: string list) (parentDir: DirectoryInfo) : DirectoryInfo option =
            parentDir.FullName :: dirPath |> Path.combineN |> ofPath2

        /// Returns a file list from the current directory.
        let getFiles (dir: DirectoryInfo) : FileInfo[] = dir.GetFiles()

        /// Returns a file list from the current directory matching the given search pattern.
        let getMatchingFiles (pattern: string) (dir: DirectoryInfo) : FileInfo[] =
            if dir |> exists then dir.GetFiles(pattern) else [||]

        /// Ensure that directory chain exists. Create necessary directories if necessary.
        let ensure (dir: DirectoryInfo) =
            if dir |> notF exists then
                dir.Create()

    module Directory =
        /// Determines whether the given path refers to an existing directory on disk.
        let exists (path: string) = Directory.Exists path

        /// Deletes the specified directory and any subdirectories and files in the directory.
        let delete (path: string) : unit =
            if path |> exists then
                Directory.Delete(path, true)

    module File =
        /// Determines whether the specified file exists.
        let exists (path: string) = File.Exists path

        /// Deletes a file if it exists.
        let delete (path: string) : unit =
            if path |> exists then
                File.Delete path

        let tryDelete path : Result<string, exn> =
            try
                delete path
                Ok path
            with exn ->
                Error exn

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

    open FSharpPlus

    let private extractKeys (uri: Uri) =
        uri.Query
        |> String.trimStart [ '?' ]
        |> String.splitCharWithRemovingEmptyEntries [| '&' |]
        |> toList
        |> map (String.split [ "=" ] >> head)

    let hasQueryParams (qs: string list) (uri: Uri) : bool =
        uri |> extractKeys |> List.intersect qs |> (=) qs

    let hasAnyQueryParam (qs: string list) (uri: Uri) : bool =
        uri |> extractKeys |> List.intersect qs |> Seq.any

    let withQueryParam (uri: Uri) : bool =
        uri.Query
        |> String.trimStart [ '?' ]
        |> String.splitCharWithRemovingEmptyEntries [| '&' |]
        |> Seq.any

    let hasAnyFragmentParam (fs: string list) (uri: Uri) =
        uri.Fragment
        |> String.trimStart [ '#' ]
        |> String.splitCharWithRemovingEmptyEntries [| '&' |]
        |> map (String.split [ "=" ] >> head)
        |> toList
        |> List.intersect fs
        |> Seq.any

[<AutoOpen>]
module Operators =
    open FSharpPlus

    let andF (fs: ('a -> bool) seq) (arg: 'a) : bool = fs |> forall (fun f -> f arg)

    let orF (fs: ('a -> bool) seq) (arg: 'a) : bool = fs |> exists (fun f -> f arg)

module HtmlNode =
    open FSharp.Data

    let cssSelectR selector node = HtmlNode.cssSelect node selector

[<AutoOpen>]
module Expr =
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
