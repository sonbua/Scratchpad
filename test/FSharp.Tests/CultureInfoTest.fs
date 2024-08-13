module CultureInfo.Tests

open System.Globalization
open Expecto
open Expecto.Flip

[<Tests>]
let specs =
    testList
        "CultureInfo.CreateSpecificCulture()"
        [ // theory data
          let defaultSpecificCultureTheoryData =
              [ "fr", "fr-FR"
                "en", "en-US"
                "vi", "vi-VN"
                "es", "es-ES" ]

          testTheory
              "Given neutral culture name When creating specific culture"
              defaultSpecificCultureTheoryData
              (fun (cultureName, defaultSpecificCultureName) ->
                  let specificCulture = CultureInfo.CreateSpecificCulture(cultureName)

                  specificCulture.Name
                  |> Expect.equal "Should return default specific culture name" defaultSpecificCultureName) ]
