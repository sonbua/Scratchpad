module NuGetCache

// #r "nuget: Fake.Core.Target"
// #r "nuget: FSharpPlus"

open System.IO
open FSharpPlus
open Fake.IO

module internal PackageDirectory =
    let private defaultVersionKey versionString : int * int * int * string = (0, 0, 0, versionString)

    /// Transforms a version string into a comparable key
    let private versionKey versionString : int * int * int * string =
        let parts = versionString |> String.split [ "." ] |> toList

        if length parts <> 3 then
            defaultVersionKey versionString
        else
            match parts |> map tryParse with
            | [ Some major; Some minor; Some patch ] -> (major, minor, patch, versionString)
            | [ Some major; Some minor; None ] -> (major, minor, -1, versionString)
            | _ -> defaultVersionKey versionString

    let listRemovables (packageDir: DirectoryInfo) : Result<string list, string> =
        packageDir
        |> DirectoryInfo.getSubDirectories
        |> toList
        |> sortByDescending (_.Name >> versionKey)
        |> function
            | [] -> Error "No version"
            | [ _ ] -> Error "Nothing to remove"
            | _ :: removing -> removing |> rev |> map _.FullName |> Ok

let listRemovables (rootDir: DirectoryInfo) : string list =
    rootDir
    |> DirectoryInfo.getSubDirectories
    |> map PackageDirectory.listRemovables
    |> choose Result.toOption
    |> List.concat

type CleanupOptions = { CacheRootDir: string }

let cleanup options : string list =
    let removables = options.CacheRootDir |> DirectoryInfo.ofPath |> listRemovables
    removables |> map Directory.delete |> ignore
    removables


module Tests =
    open Expecto
    open Expecto.Logging

    [<Tests>]
    let specs =
        let logger = Log.create "NuGetCache"
        let writeln = Message.eventX >> logger.info

        testList
            "NuGetCache"
            [ test "Cleanup" {
                  let options = { CacheRootDir = @"C:\Users\song\.nuget\packages" }
                  options |> cleanup |> map writeln |> ignore
              } ]
