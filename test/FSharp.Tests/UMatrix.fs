module UMatrix

// #r "nuget: FSharpPlus"
// #r "nuget: FsToolkit.ErrorHandling"

open FSharpPlus
open FsToolkit.ErrorHandling

type ComparisonResult =
    | Same
    | MoreSpecific
    | LessSpecific
    | Irrelevant

module ComparisonResult =
    let isSameOrMoreSpecific result : bool =
        match result with
        | Same
        | MoreSpecific -> true
        | _ -> false

module Hostname =
    let create h =
        match h |> Uri.CheckHostName with
        | UriHostNameType.Dns
        | UriHostNameType.IPv4 -> Ok h
        | UriHostNameType.Basic -> Error $"Cannot determine the hostname type of '{h}'"
        | UriHostNameType.IPv6 -> Error "IPv6 is not supported"
        | UriHostNameType.Unknown
        | _ -> Error $"Unknown hostname type '{h}'"

    let private isMoreQualifiedThan (h1: string) (h2: string) =
        h2.Length > h1.Length
        && h2 |> String.endsWith h1
        && h2[h2.Length - h1.Length - 1] = '.'

    let compare (h1: string) (h2: string) : ComparisonResult =
        if h1 = h2 then Same
        elif h2 |> isMoreQualifiedThan h1 then MoreSpecific
        elif h1 |> isMoreQualifiedThan h2 then LessSpecific
        else Irrelevant

type Source =
    | Specific of string
    /// Denotes "any context", a.k.a. the global scope.
    | Any

module Source =
    let create s : Result<Source, string> =
        match s with
        | "" -> Error "Source is empty"
        | "*" -> Ok Source.Any
        | hostname -> hostname |> Hostname.create |> map Source.Specific

    let toString s : string =
        match s with
        | Specific src -> src
        | Any -> "*"

    let compare s1 s2 : ComparisonResult =
        match s1, s2 with
        | s1, s2 when s1 = s2 -> Same
        | Source.Any, _ -> MoreSpecific
        | _, Source.Any -> LessSpecific
        | Specific s1, Specific s2 -> Hostname.compare s1 s2

type Destination =
    | FirstParty
    | Specific of string
    | Any

module Destination =
    let create d : Result<Destination, string> =
        match d with
        | "" -> Error "Destination is empty"
        | "*" -> Ok Destination.Any
        | "1st-party" -> Ok Destination.FirstParty
        | hostname -> hostname |> Hostname.create |> map Destination.Specific

    let toString d : string =
        match d with
        | FirstParty -> "1st-party"
        | Specific dst -> dst
        | Any -> "*"

    let compare d1 d2 : ComparisonResult =
        match d1, d2 with
        | d1, d2 when d1 = d2 -> Same
        | Destination.Any, _ -> MoreSpecific
        | _, Destination.Any -> LessSpecific
        | FirstParty, _ -> MoreSpecific
        | _, FirstParty -> LessSpecific
        | Specific d1, Specific d2 -> Hostname.compare d1 d2

type RequestType =
    | Cookie
    | Css
    | Image
    | Media
    | Script
    | Fetch
    | Frame
    | Other
    | Any

module RequestType =
    let create t : Result<RequestType, string> =
        match t with
        | "cookie" -> Ok RequestType.Cookie
        | "css" -> Ok RequestType.Css
        | "image" -> Ok RequestType.Image
        | "media" -> Ok RequestType.Media
        | "script" -> Ok RequestType.Script
        | "fetch" -> Ok RequestType.Fetch
        | "frame" -> Ok RequestType.Frame
        | "other" -> Ok RequestType.Other
        | "*" -> Ok RequestType.Any
        | unknown -> Error $"unsupported request type '{unknown}'"

    let toString t : string =
        match t with
        | Cookie -> "cookie"
        | Css -> "css"
        | Image -> "image"
        | Media -> "media"
        | Script -> "script"
        | Fetch -> "fetch"
        | Frame -> "frame"
        | Other -> "other"
        | Any -> "*"

    let compare t1 t2 : ComparisonResult =
        match t1, t2 with
        | t1, t2 when t1 = t2 -> Same
        | RequestType.Any, _ -> MoreSpecific
        | _, RequestType.Any -> LessSpecific
        | _ -> Irrelevant

type Action =
    | Allow
    | Block

module Action =
    let create a : Result<Action, string> =
        match a with
        | "allow" -> Ok Action.Allow
        | "block" -> Ok Action.Block
        | unknown -> Error $"unsupported action '{unknown}'"

    let toString a : string =
        match a with
        | Allow -> "allow"
        | Block -> "block"

type Rule =
    {
        /// Denotes the context from which a net request is made, also known as the "scope".
        Source: Source
        /// Denotes where the net request is destined.
        Destination: Destination
        RequestType: RequestType
        Action: Action
    }

module Rule =
    let create ruleString : Result<Rule, string> =
        result {
            let parts = ruleString |> String.split [ " " ] |> toList
            let! source = parts[0] |> Source.create
            let! destination = parts[1] |> Destination.create
            let! requestType = parts[2] |> RequestType.create
            let! action = parts[3] |> Action.create

            return
                { Source = source
                  Destination = destination
                  RequestType = requestType
                  Action = action }
        }
        |> Result.bind (function
            | { Source = Source.Specific _
                Destination = FirstParty } -> Error "Invalid combination: specific source with 1st-party destination"
            | r -> Ok r)

    let toString rule : string =
        let source = rule.Source |> Source.toString
        let destination = rule.Destination |> Destination.toString
        let requestType = rule.RequestType |> RequestType.toString
        let action = rule.Action |> Action.toString

        $"{source} {destination} {requestType} {action}"

    let compare r1 r2 : ComparisonResult =
        match r1, r2 with
        | { Source = s1
            Destination = d1
            RequestType = t1 },
          { Source = s2
            Destination = d2
            RequestType = t2 } when s1 = s2 && d1 = d2 && t1 = t2 -> Same
        | { Source = Source.Any
            Destination = Destination.FirstParty },
          { Source = Source.Specific s2
            Destination = Destination.Specific d2 } when
            Hostname.compare s2 d2 |> ComparisonResult.isSameOrMoreSpecific |> not
            ->
            Irrelevant
        | { Source = Source.Specific s1
            Destination = Destination.Specific d1 },
          { Source = Source.Any
            Destination = Destination.FirstParty } when
            Hostname.compare s1 d1 |> ComparisonResult.isSameOrMoreSpecific |> not
            ->
            Irrelevant
        | { Source = s1
            Destination = d1
            RequestType = t1 },
          { Source = s2
            Destination = d2
            RequestType = t2 } when
            Source.compare s1 s2 |> ComparisonResult.isSameOrMoreSpecific
            && Destination.compare d1 d2 |> ComparisonResult.isSameOrMoreSpecific
            && RequestType.compare t1 t2 |> ComparisonResult.isSameOrMoreSpecific
            ->
            MoreSpecific
        | { Source = s1
            Destination = d1
            RequestType = r1 },
          { Source = s2
            Destination = d2
            RequestType = r2 } when
            Source.compare s2 s1 |> ComparisonResult.isSameOrMoreSpecific
            && Destination.compare d2 d1 |> ComparisonResult.isSameOrMoreSpecific
            && RequestType.compare r2 r1 |> ComparisonResult.isSameOrMoreSpecific
            ->
            LessSpecific
        | _ -> Irrelevant

    /// Is used to order a ruleset, where rules are mostly irrelevant to each other.
    let comparer r1 r2 : int =
        compare r1 r2
        |> function
            | Same -> 0
            | MoreSpecific -> -1
            | LessSpecific -> 1
            | Irrelevant ->
                // Returns -1 if r2 is more specific than r1,
                // 1 if r1 is more specific than r2.
                // This should not return 0.
                // Precedence: source > destination > request type
                match r1, r2 with
                | { Source = s1 }, { Source = s2 } when Source.compare s1 s2 = MoreSpecific -> -1
                | { Source = s1 }, { Source = s2 } when Source.compare s1 s2 = LessSpecific -> 1
                | { Destination = d1 }, { Destination = d2 } when Destination.compare d1 d2 = MoreSpecific -> -1
                | { Destination = d1 }, { Destination = d2 } when Destination.compare d1 d2 = LessSpecific -> 1
                | { RequestType = t1 }, { RequestType = t2 } when RequestType.compare t1 t2 = MoreSpecific -> -1
                | { RequestType = t1 }, { RequestType = t2 } when RequestType.compare t1 t2 = LessSpecific -> 1
                | _ -> 0

type Request =
    { Source: string
      Destination: string
      Type: RequestType }

module Request =
    /// Example: "src.com dst.com script"
    let create requestString : Result<Request, string> =
        result {
            let parts = requestString |> String.split [ " " ] |> toList
            let source = parts[0]
            let destination = parts[1]
            let! requestType = parts[2] |> RequestType.create

            return
                { Source = source
                  Destination = destination
                  Type = requestType }
        }

    let private toRule request : Rule =
        { Source = request.Source |> Source.Specific
          Destination = request.Destination |> Destination.Specific
          RequestType = request.Type
          Action = Action.Allow }

    let matches rule request : bool =
        request |> toRule |> Rule.compare rule |> ComparisonResult.isSameOrMoreSpecific

type Ruleset = Rule list

module Ruleset =
    let create rs : Ruleset =
        rs
        |> String.split [ "\r\n"; "\n" ]
        |> toList
        |> map String.trimWhiteSpaces
        |> map Rule.create
        |> choose Result.toOption

    let evaluate ruleset request : Rule option =
        ruleset
        |> filter (flip Request.matches request)
        |> List.sortWith Rule.comparer
        |> tryLast
