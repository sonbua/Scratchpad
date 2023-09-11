module Reusables

let tap action value =
    action value |> ignore
    value

module Map =
    let change2 (key: 'Key) (newValue: 'T) =
        Map.change key (Option.map (fun _ -> newValue))
