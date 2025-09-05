module VozUserPosts

open FSharpPlus

let extractUserId =
    String.trimWhiteSpaces
    >> Regex.findMatch "https://voz\.vn/u/.+?\.(\d+)/?$"
    >> _.Groups[1].Value

let extractUsername = String.trimWhiteSpaces >> Uri.EscapeDataString

/// <summary>
/// Generates URL to page, which lists all posts of a given user.
/// </summary>
/// <param name="username">The username of the user. Example: <c>buitruong1689</c>.</param>
/// <param name="profileUrl">The URL to the user's profile page. Example: <c>https://voz.vn/u/buitruong1689.1192037/</c>.</param>
let userPosts username profileUrl =
    let userId = profileUrl |> extractUserId
    let username = username |> extractUsername

    $"https://voz.vn/search/{userId}/?c[users]={username}&o=date"
