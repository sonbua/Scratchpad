module NuGetCache

open FSharpPlus
open Fake.IO

type Removable =
    { PackageDir: DirectoryInfo
      RemovingVersions: DirectoryInfo list }

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

    let listRemovables (packageDir: DirectoryInfo) : Result<Removable, string> =
        packageDir
        |> DirectoryInfo.getSubDirectories
        |> toList
        |> sortByDescending (_.Name >> versionKey)
        |> function
            | [] -> Error "No version"
            | [ _ ] -> Error "Nothing to remove"
            | _ :: removing ->
                removing
                |> rev
                |> fun rs ->
                    { PackageDir = packageDir
                      RemovingVersions = rs }
                    |> Ok

type CleanupOptions = { CacheRootDir: string }

let listRemovables (options: CleanupOptions) : Removable list =
    options
    |> _.CacheRootDir
    |> DirectoryInfo.ofPath
    |> DirectoryInfo.getSubDirectories
    |> toList
    |> map PackageDirectory.listRemovables
    |> choose Result.toOption

let cleanup options : Removable list =
    let removables = options |> listRemovables

    removables
    |> map _.RemovingVersions
    |> List.concat
    |> map (_.FullName >> Directory.delete)
    |> ignore

    removables
