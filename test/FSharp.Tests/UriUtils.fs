module UriUtils.Tests

open System
open Expecto
open Expecto.Flip

[<Tests>]
let specs =
    testList
        "UriUtils"
        [ // theory data
          let validUris =
              [ "http://example.com"
                "https://example.com"
                "ftp://example.com"
                "ftps://example.com" ]

          testTheory "Given valid URI" validUris (fun uri ->
              uri
              |> Uri.isValid
              |> Expect.isTrue "When applying `Uri.isValid` should return valid")

          // theory data
          let singleUriTexts =
              [ "13:45:43​fsprojects.github.io​3​https://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/js/bootstrap.min.js​script​--",
                Uri "https://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/js/bootstrap.min.js" ]

          testTheory "Given text containing single URI" singleUriTexts (fun (text, expectedUri) ->
              let matches = text |> Uri.extract

              matches
              |> Expect.hasLength "Should have single item" 1

              matches
              |> Seq.exactlyOne
              |> Expect.equal "Should extract correct URI" expectedUri) ]
