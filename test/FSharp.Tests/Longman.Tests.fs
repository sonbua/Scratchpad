module LongmanTests

open FSharpPlus
open Longman
open FsUnit
open Xunit

type Tests() =
    [<Theory>]
    [<InlineData("because")>]
    member x.``WordFamily: No word family`` query =
         let actualWordFamily = query |> lookup |> _.WordFamily

         actualWordFamily |> should equal None


    static member WordFamilies: seq<obj []> = [
        [| "compensation"; [| "compensation"; "compensatory"; "compensate" |] |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.WordFamilies)>]
    member x.``WordFamily: Has word family`` query (wordFamily: string []) =
         let actualWordFamily = query |> lookup |> _.WordFamily |> Option.get

         actualWordFamily |> should equalSeq wordFamily

    [<Theory>]
    [<InlineData("example")>]
    [<InlineData("include")>]
    [<InlineData("get above")>]
    member x.``Entry: Id (no ID)`` query =
        let entry = query |> lookup |> _.Entries |> Seq.exactlyOne

        entry.Id |> should equal None


    static member Words: seq<obj []> = [
        [| "else"; Word "else" |]
        [| "get about"; PhrasalVerb "get about" |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.Words)>]
    member x.``Entry: Word`` query (word: Word) =
        let entry = query |> lookup |> _.Entries |> head

        entry.Word |> should equal word

    [<Theory>]
    [<InlineData("because")>]
    [<InlineData("case")>]
    member x.``Entry: Number (multiple entries)`` query =
        let entries = query |> lookup |> _.Entries

        entries.Length |> should be (greaterThan 1)
        entries.Head.No |> should equal (Some 1)

    [<Theory>]
    [<InlineData("example")>]
    [<InlineData("include")>]
    [<InlineData("get above")>]
    member x.``Entry: Number (single entry)`` query =
        let entry = query |> lookup |> _.Entries |> Seq.exactlyOne

        entry.No |> should equal None


    static member PronunciationTranscriptions: seq<obj []> = [
        [| "case"; [ "keɪs" ] |]
        [| "can"; [ "kən"; "kæn" ] |]
        [| "some"; [ "səm"; "sʌm" ] |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.PronunciationTranscriptions)>]
    member x.``Entry: Pronunciation: Transcriptions`` query (transcriptions: string list) =
        let entry = query |> lookup |> _.Entries |> head
        let actualTranscriptions = entry.Pronunciation.Transcriptions

        actualTranscriptions |> should equalSeq transcriptions

    [<Theory>]
    [<InlineData("biology", "-ˈɑːl-")>]
    [<InlineData("work", "wɜːrk")>]
    member x.``Entry: Pronunciation: American variant`` query (americanVariant: string) =
        let entry = query |> lookup |> _.Entries |> head

        entry.Pronunciation.AmericanVariant
        |> should equal (Some americanVariant)


    static member PronunciationAudioFiles: seq<obj []> = [
        [| "can"; [
            (British, "https://www.ldoceonline.com/media/english/breProns/ld44can.mp3?version=1.2.71")
            (American, "https://www.ldoceonline.com/media/english/ameProns/l3can.mp3?version=1.2.71")
        ] |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.PronunciationAudioFiles)>]
    member x.``Entry: Pronunciation: Audio files`` query (audios: (Label * string) list) =
        let entry = query |> lookup |> _.Entries |> head

        entry.Pronunciation.Audio
        |> should equalSeq audios

    [<Theory>]
    [<InlineData("word", "word__2")>]
    [<InlineData("else", "else__2")>]
    member x.``Entry: Senses: Sense: No definition`` query senseId =
        let entry = query |> lookup |> _.Entries |> head

        let actualDefinition =
            entry
            |> _.Senses
            |> List.pick (function
                | Sense sense when sense.Id = senseId -> Some sense
                | _ -> None)
            |> _.Definition

        actualDefinition |> should equal None


    static member SimpleExampleWithAudio: seq<obj []> = [
        [| "get about"; "get-about__1"; Simple { Text = "She’s 80 now, and doesn’t get about much anymore."; Audio = "https://www.ldoceonline.com/media/english/exaProns/ldoce6exa_00386.mp3?version=1.2.71" } |]
        [| "get about"; "get-about__2"; Simple { Text = "I don’t really want this to get about."; Audio = "https://www.ldoceonline.com/media/english/exaProns/p008-001290263.mp3?version=1.2.71" } |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.SimpleExampleWithAudio)>]
    member x.``Entry: Senses: Sense: Examples: Simple example with audio`` query senseId simpleExample =
        let entry = query |> lookup |> _.Entries |> head

        let actualExample =
            entry
            |> _.Senses
            |> List.pick (function
                | Sense sense when sense.Id = senseId -> Some sense
                | _ -> None)
            |> _.Examples
            |> head

        actualExample |> should equal simpleExample


    static member SenseThesauruses: seq<obj []> = [
        [| "go"; "go__7"; [ "become" ] |]
        [| "revise"; "revise__2"; [ "learn"; "study" ] |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.SenseThesauruses)>]
    member x.``Entry: Senses: Sense: Thesauruses`` query senseId (thesauruses: string list) =
        let entry = query |> lookup |> _.Entries |> head

        let actualThesauruses =
            entry
            |> _.Senses
            |> List.pick (function
                | Sense sense when sense.Id = senseId -> Some sense
                | _ -> None)
            |> _.Thesauruses

        actualThesauruses |> should equalSeq thesauruses


    static member SenseCrossRefs: seq<obj[]> = [
        [| "get"; "get__13"; [ "get (somebody) somewhere/anywhere/nowhere" ] |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.SenseCrossRefs)>]
    member x.``Entry: Senses: Sense: Cross references`` query senseId crossRefs =
        let entry = query |> lookup |> _.Entries |> head

        let actualCrossRefs =
            entry
            |> _.Senses
            |> List.pick (function
                | Sense sense when sense.Id = senseId -> Some sense
                | _ -> None)
            |> _.CrossRefs

        actualCrossRefs |> should equalSeq crossRefs


    static member SubsenseGroupDefinition: seq<obj []> = [
        [| "else"; "else__1"; 0; "besides or in addition to someone or something" |]
        [| "else"; "else__1"; 1; "used to talk about a different person, thing, place etc" |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.SubsenseGroupDefinition)>]
    member x.``Entry: Senses: SubsenseGroup: Subsenses: Definition`` query subsenseGroupId subsenseIndex definition =
        let entry = query |> lookup |> _.Entries |> head

        let actualDefinition =
            entry
            |> _.Senses
            |> List.pick (function
                | SubsenseGroup group when group.Id = subsenseGroupId -> Some group
                | _ -> None)
            |> _.Subsenses
            |> List.item subsenseIndex
            |> _.Definition

        actualDefinition |> should equal (Some definition)


    static member SubsenseGroups: seq<obj []> = [
        [| "else"; "else__1"; 2 |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.SubsenseGroups)>]
    member x.``Entry: Senses: SubsenseGroup: Subsenses: Count`` query senseId subsenseCount =
        let entry = query |> lookup |> _.Entries |> head

        let subsenses =
            entry
            |> _.Senses
            |> List.pick (function
                | SubsenseGroup group when group.Id = Some senseId -> Some group
                | _ -> None)
            |> _.Subsenses

        subsenses |> should haveLength subsenseCount


    static member SubsenseGroupThesauruses: seq<obj []> = [
        [| "get"; "get__4"; [ "buy" ] |]
        [| "get"; "get__5"; [ "earn" ] |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.SubsenseGroupThesauruses)>]
    member x.``Entry: Senses: SubsenseGroup: Thesauruses`` query subsenseGroupId (thesauruses: string list) =
        let entry = query |> lookup |> _.Entries |> head

        let actualThesauruses =
            entry
            |> _.Senses
            |> List.pick (function
                | SubsenseGroup group when group.Id = subsenseGroupId -> Some group
                | _ -> None)
            |> _.Thesauruses

        actualThesauruses |> should equalSeq thesauruses


    static member SubsenseGroupCrossRefs: seq<obj[]> = [
        [| "case"; "case__8"; 2; [ "bookcase"; "briefcase"; "pillowcase" ] |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.SubsenseGroupCrossRefs)>]
    member x.``Entry: Senses: SubsenseGroup: Subsenses: Cross references`` query subsenseGroupId subsenseIndex crossRefs =
        let entry = query |> lookup |> _.Entries |> head

        let actualCrossRefs =
            entry
            |> _.Senses
            |> List.pick (function
                | SubsenseGroup group when group.Id = subsenseGroupId -> Some group
                | _ -> None)
            |> _.Subsenses
            |> List.item subsenseIndex
            |> _.CrossRefs

        actualCrossRefs |> should equalSeq crossRefs


    static member SingleCrossReferences: seq<obj []> = [
        [| "get about"; [ "get" ] |]
        [| "else"; [ "if nothing else"; "be something else" ] |]
    ]
    [<Theory>]
    [<MemberData(nameof Tests.SingleCrossReferences)>]
    member x.``Entry: Cross references`` query (crossRefs: string list) =
        let entry = query |> lookup |> _.Entries |> head
        let actualCrossRefs = entry.CrossRefs

        actualCrossRefs |> should equalSeq crossRefs
