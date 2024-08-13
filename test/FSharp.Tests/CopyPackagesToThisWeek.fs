module CopyPackagesToThisWeek

open FSharpPlus

let packages =
    [ "EPiServer.CMS.AspNetCore"
      "EPiServer.CMS.AspNetCore.HtmlHelpers"
      "EPiServer.CMS.AspNetCore.Mvc"
      "EPiServer.CMS.AspNetCore.Routing"
      "EPiServer.CMS.AspNetCore.TagHelpers"
      "EPiServer.CMS.AspNetCore.Templating"
      "EPiServer.CMS.Core"
      "EPiServer.Framework"
      "EPiServer.Framework.AspNetCore"
      "EPiServer.Hosting" ]

let extension = "nupkg"
let sourceBaseDir = @"\\nuget.ep.se\NuGet\"
let destinationDir = @"\\nuget.ep.se\Releases\thisweek\"

// Side effect
let copy source destination =
    System.IO.File.Copy(source, destination)

let copyPackagesToThisWeek version =
    packages
    |> map (fun package -> ($@"{sourceBaseDir}{package}\{version}\", $"{package}.{version}.{extension}"))
    |> map (fun (sourceDir, packageFilename) -> ($"{sourceDir}{packageFilename}", $"{destinationDir}{packageFilename}"))
    // Side effect
    |> map (fun (source, destination) -> copy source destination)

// Usage:
// copyPackagesToThisWeek "12.19.1"
