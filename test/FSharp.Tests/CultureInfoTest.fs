module CultureInfoTest

open System.Globalization
open Xunit

[<Theory>]
[<InlineData("fr", "fr-FR")>]
[<InlineData("en", "en-US")>]
[<InlineData("vi", "vi-VN")>]
[<InlineData("es", "es-ES")>]
let ``Given neutral culture name When creating specific culture Then returns default specific culture name``
    (
        cultureName,
        defaultSpecificCultureName
    ) =
    let specificCulture =
        CultureInfo.CreateSpecificCulture(cultureName)

    Assert.Equal(defaultSpecificCultureName, specificCulture.Name)
