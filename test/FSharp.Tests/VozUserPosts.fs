module VozUserPosts

module Regex =
    let findMatch pattern input = Regex.Match(input, pattern)

let extractUserId =
    Regex.findMatch "https://voz\.vn/u/.+?\.(\d+)/?$" >> _.Groups[1].Value

open FSharpPlus

let extractUsername =
    Regex.findMatch "^(.+?)https://"
    >> _.Groups[1].Value
    >> String.trimWhiteSpaces
    >> Uri.EscapeDataString

/// <summary>
/// Generates URL to page, which lists all posts of a given user.
/// </summary>
/// <param name="input">The input string, which has the format <code>&lt;username&gt; &lt;URL_to_users_profile_page&gt;</code>.</param>
/// <example><code>buitruong1689 https://voz.vn/u/buitruong1689.1192037/</code></example>
let userPosts input =
    let input = input |> String.trimWhiteSpaces
    let userId = input |> extractUserId
    let username = input |> extractUsername

    $"https://voz.vn/search/{userId}/?c[users]={username}&o=date"


open Expecto
open Expecto.Flip

[<Tests>]
let specs =
    testList "VozUserPosts"
        [ // theory data
          let userTheoryData =
              [ "buitruong1689 https://voz.vn/u/buitruong1689.1192037/",
                "https://voz.vn/search/1192037/?c[users]=buitruong1689&o=date"
                "Fire Of Heart https://voz.vn/u/fire-of-heart.873787/",
                "https://voz.vn/search/873787/?c[users]=Fire%20Of%20Heart&o=date" ]

          testTheory "Given username and URL to user's profile page" userTheoryData (fun (text, userPostsUrl) ->
              let actualUserPostsUrl = text |> userPosts
              actualUserPostsUrl |> Expect.equal "Should return URL to user's posts" userPostsUrl) ]
