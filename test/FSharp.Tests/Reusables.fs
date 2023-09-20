module Reusables

let tap action value =
    action value |> ignore
    value

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

module Result =
    let isOk =
        function
        | Ok _ -> true
        | Error _ -> false

    let isError =
        function
        | Error _ -> true
        | Ok _ -> false

    let get result =
        match result with
        | Error _ -> failwith "Is Error!"
        | Ok value -> value
