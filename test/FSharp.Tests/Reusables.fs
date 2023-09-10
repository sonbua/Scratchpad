module Reusables

let tap action value =
    action value |> ignore
    value
