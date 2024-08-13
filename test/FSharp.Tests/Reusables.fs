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
    let toOption text =
        match text with
        | null -> None
        | _ -> Some text

    let defaultIfNull defaultValue text =
        match text with
        | null -> defaultValue
        | _ -> text

    let emptyIfNull = defaultIfNull ""

module List =
    /// Like List.skip but rather than throwing System.ArgumentException, it returns an empty list if the number of
    /// skipped items is greater than the length of the list.
    let skipSafe count list =
        if list |> List.length |> (<) count then
            list |> List.skip count
        else
            []

module Option =
    let ifWith predicate onTrue inp =
        if inp |> predicate then Some(inp |> onTrue) else None

module Result =
    let isOk =
        function
        | Ok _ -> true
        | Error _ -> false

    let isError =
        function
        | Error _ -> true
        | Ok _ -> false
