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
