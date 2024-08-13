module UriUtils

open FSharpPlus

let f text =
    text |> Uri.extract |> map _.Authority |> distinct


open Xunit
open FsUnit.Xunit

[<Theory>]
[<InlineData("http://example.com")>]
[<InlineData("https://example.com")>]
[<InlineData("ftp://example.com")>]
[<InlineData("ftps://example.com")>]
let ``Given URL`` url = url |> Uri.isValid |> should be True

[<Theory>]
[<InlineData("13:45:43​fsprojects.github.io​3​https://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/js/bootstrap.min.js​script​--",
             "https://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/js/bootstrap.min.js")>]
let ``Given text containing single URL`` text expectedUrl =
    let matches = text |> Uri.extract

    matches |> Seq.length |> should equal 1
    matches |> Seq.exactlyOne |> should equal expectedUrl
