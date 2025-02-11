module LongmanTests

open Expecto
open Expecto.Flip
open FSharpPlus
open Longman

[<Tests>]
let specs =
    testList "LDOCE" [
        // theory data
        let noWordFamilyTheoryData = [ "because" ]
        testTheory "WordFamily: No word family" noWordFamilyTheoryData (fun query ->
            let actualWordFamily = query |> lookup |> _.WordFamily
            actualWordFamily |> Expect.equal "Should have no word family" None)

        // theory data
        let hasWordFamilyTheoryData = [ "compensation", [ "compensation"; "compensatory"; "compensate" ] ]
        testTheory "WordFamily: Has word family" hasWordFamilyTheoryData (fun (query, wordFamily: string list) ->
            let actualWordFamily = query |> lookup |> _.WordFamily |> Option.get
            actualWordFamily |> Expect.sequenceEqual "Should have word family" wordFamily)

        // theory data
        let noEntryIdTheoryData = [ "example"; "include"; "get above" ]
        testTheory "Entry: Id (no ID)" noEntryIdTheoryData (fun query ->
            let entry = query |> lookup |> _.Entries |> Seq.exactlyOne
            entry.Id |> Expect.isNone "There should be no ID")

        // theory data
        let wordTheoryData =
            [ "else", Word "else"
              "get about", PhrasalVerb "get about" ]
        testTheory "Entry: Word" wordTheoryData (fun (query, word: Word) ->
            let entry = query |> lookup |> _.Entries |> head
            entry.Word |> Expect.equal "Should have word" word)

        // theory data
        let multipleEntriesTheoryData = [ "because"; "case" ]
        testTheory "Entry: Number (multiple entries)" multipleEntriesTheoryData (fun query ->
            let entries = query |> lookup |> _.Entries
            (entries.Length, 1) |> Expect.isGreaterThan "Should be more than one entry"
            entries.Head.No |> Expect.equal "Entries should be numbered" (Some 1))

        // theory data
        let singleEntryTheoryData = [ "example"; "include"; "get above" ]
        testTheory "Entry: Number (single entry)" singleEntryTheoryData (fun query ->
            let entry = query |> lookup |> _.Entries |> Seq.exactlyOne
            entry.No |> Expect.isNone "Should not be numbered")

        // theory data
        let pronunciationTranscriptionsTheoryData =
            [ "case", [ "keɪs" ]
              "can", [ "kən"; "kæn" ]
              "some", [ "səm"; "sʌm" ] ]
        testTheory "Entry: Pronunciation: Transcriptions" pronunciationTranscriptionsTheoryData
            (fun (query, transcriptions: string list) ->
                 let entry = query |> lookup |> _.Entries |> head
                 let actualTranscriptions = entry.Pronunciation.Transcriptions
                 actualTranscriptions |> Expect.sequenceEqual "Should contain transcriptions" transcriptions)

        // theory data
        let pronunciationAmericanVariantTheoryData =
            [ "biology", "-ˈɑːl-"
              "work", "wɜːrk" ]
        testTheory "Entry: Pronunciation: American variant" pronunciationAmericanVariantTheoryData
            (fun (query, americanVariant: string) ->
                 let entry = query |> lookup |> _.Entries |> head
                 let actualAmericanVariant = entry.Pronunciation.AmericanVariant
                 actualAmericanVariant |> Expect.equal "Should exist American variant" (Some americanVariant))

        // theory data
        let pronunciationAudioFiles =
            [ "can", [ (British, "https://www.ldoceonline.com/media/english/breProns/ld44can.mp3?version=1.2.77")
                       (American, "https://www.ldoceonline.com/media/english/ameProns/l3can.mp3?version=1.2.77") ] ]
        testTheory "Entry: Pronunciation: Audio files" pronunciationAudioFiles
            (fun (query, audios: (Label * string) list) ->
                 let entry = query |> lookup |> _.Entries |> head
                 entry.Pronunciation.Audio |> Expect.sequenceEqual "Should have audios" audios)

        // theory data
        let senseNoDefinitionTheoryData =
            [ "word", "word__2"
              "else", "else__2" ]
        testTheory "Entry: Senses: Sense: No definition" senseNoDefinitionTheoryData
            (fun (query, senseId) ->
                 let entry = query |> lookup |> _.Entries |> head
                 let actualDefinition =
                     entry
                     |> _.Senses
                     |> List.pick (function
                         | Sense sense when sense.Id = Some senseId -> Some sense
                         | _ -> None)
                     |> _.Definition
                 actualDefinition |> Expect.isNone "Should be None")

        // theory data
        let simpleExampleWithAudioTheoryData =
            [ "get about", "get-about__1", Simple { Text = "She’s 80 now, and doesn’t get about much anymore."; Audio = "https://www.ldoceonline.com/media/english/exaProns/ldoce6exa_00386.mp3?version=1.2.77" }
              "get about", "get-about__2", Simple { Text = "I don’t really want this to get about."; Audio = "https://www.ldoceonline.com/media/english/exaProns/p008-001290263.mp3?version=1.2.77" } ]
        testTheory "Entry: Senses: Sense: Examples: Simple example with audio" simpleExampleWithAudioTheoryData
            (fun (query, senseId, simpleExample) ->
                 let entry = query |> lookup |> _.Entries |> head
                 let actualExample =
                     entry
                     |> _.Senses
                     |> List.pick (function
                         | Sense sense when sense.Id = Some senseId -> Some sense
                         | _ -> None)
                     |> _.Examples
                     |> head
                 actualExample |> Expect.equal "Should equal simple example" simpleExample)

        // theory data
        let senseThesaurusesTheoryData =
            [ "go", "go__7", [ "become" ]
              "revise", "revise__2", [ "learn"; "study" ] ]
        testTheory "Entry: Senses: Sense: Thesauruses" senseThesaurusesTheoryData
            (fun (query, senseId, thesauruses: string list) ->
                 let entry = query |> lookup |> _.Entries |> head
                 let actualThesauruses =
                     entry
                     |> _.Senses
                     |> List.pick (function
                         | Sense sense when sense.Id = Some senseId -> Some sense
                         | _ -> None)
                     |> _.Thesauruses
                 actualThesauruses |> Expect.sequenceEqual "Should have thesauruses" thesauruses)

        // theory data
        let senseCrossRefsTheoryData =
            [ "get", "get__13", [ "get (somebody) somewhere/anywhere/nowhere" ] ]
        testTheory "Entry: Senses: Sense: Cross references" senseCrossRefsTheoryData
            (fun (query, senseId, crossRefs) ->
                 let entry = query |> lookup |> _.Entries |> head
                 let actualCrossRefs =
                     entry
                     |> _.Senses
                     |> List.pick (function
                         | Sense sense when sense.Id = Some senseId -> Some sense
                         | _ -> None)
                     |> _.CrossRefs
                 actualCrossRefs |> Expect.sequenceEqual "Should have sense's cross references" crossRefs)

        // theory data
        let subsenseGroupDefinitionTheoryData =
            [ "else", "else__1", 0, "besides or in addition to someone or something"
              "else", "else__1", 1, "used to talk about a different person, thing, place etc" ]
        testTheory "Entry: Senses: SubsenseGroup: Subsenses: Definition" subsenseGroupDefinitionTheoryData
            (fun (query, subsenseGroupId, subsenseIndex, definition) ->
                 let entry = query |> lookup |> _.Entries |> head
                 let actualDefinition =
                     entry
                     |> _.Senses
                     |> List.pick (function
                         | SubsenseGroup group when group.Id = Some subsenseGroupId -> Some group
                         | _ -> None)
                     |> _.Subsenses
                     |> List.item subsenseIndex
                     |> _.Definition
                 actualDefinition |> Expect.equal "Should have definition" (Some definition))

        // theory data
        let subsenseGroupTheoryData = [ "else", "else__1", 2 ]
        testTheory "Entry: Senses: SubsenseGroup: Subsenses: Count" subsenseGroupTheoryData
            (fun (query, senseId, subsenseCount) ->
                 let entry = query |> lookup |> _.Entries |> head
                 let subsenses =
                     entry
                     |> _.Senses
                     |> List.pick (function
                         | SubsenseGroup group when group.Id = Some senseId -> Some group
                         | _ -> None)
                     |> _.Subsenses
                 subsenses |> Expect.hasLength "Should have correct subsense count" subsenseCount)

        // theory data
        let subsenseGroupThesaurusesTheoryData =
            [ "get", "get__4", [ "buy" ]
              "get", "get__5", [ "earn" ] ]
        testTheory "Entry: Senses: SubsenseGroup: Thesauruses" subsenseGroupThesaurusesTheoryData
            (fun (query, subsenseGroupId, thesauruses: string list) ->
                 let entry = query |> lookup |> _.Entries |> head
                 let actualThesauruses =
                     entry
                     |> _.Senses
                     |> List.pick (function
                         | SubsenseGroup group when group.Id = Some subsenseGroupId -> Some group
                         | _ -> None)
                     |> _.Thesauruses
                 actualThesauruses |> Expect.sequenceEqual "Should have thesauruses" thesauruses)

        // theory data
        let subsenseGroupCrossRefsTheoryData = [ "case", "case__8", 2, [ "bookcase"; "briefcase"; "pillowcase" ] ]
        testTheory "Entry: Senses: SubsenseGroup: Subsenses: Cross references" subsenseGroupCrossRefsTheoryData
            (fun (query, subsenseGroupId, subsenseIndex, crossRefs) ->
                 let entry = query |> lookup |> _.Entries |> head
                 let actualCrossRefs =
                     entry
                     |> _.Senses
                     |> List.pick (function
                         | SubsenseGroup group when group.Id = Some subsenseGroupId -> Some group
                         | _ -> None)
                     |> _.Subsenses
                     |> List.item subsenseIndex
                     |> _.CrossRefs
                 actualCrossRefs |> Expect.sequenceEqual "Should have subsense's cross references" crossRefs)

        // theory data
        let entryCrossReferencesTheoryData =
            [ "get about", [ "get" ]
              "else", [ "if nothing else"; "be something else" ] ]
        testTheory "Entry: Cross references" entryCrossReferencesTheoryData (fun (query, crossRefs: string list) ->
            let entry = query |> lookup |> _.Entries |> head
            let actualCrossRefs = entry.CrossRefs
            actualCrossRefs |> Expect.sequenceEqual "This entry should have cross references" crossRefs) ]
