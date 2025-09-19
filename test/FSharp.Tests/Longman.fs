/// Short for Longman Dictionary of Contemporary English (LDOCE)
module Longman

open FSharp.Data
open FSharpPlus
open Microsoft.FSharp.Core

module Option =
    let ifWith predicate onTrue inp =
        if inp |> predicate then Some(inp |> onTrue) else None

module Audio =
    /// Root node contains "data-src-mp3" attribute
    let extract = HtmlNode.attribute "data-src-mp3" >> HtmlAttribute.value

type WordFamily = string list

module WordFamily =
    /// Root node: class="wordfams"
    let extract: HtmlNode -> WordFamily =
        HtmlNode.cssSelectR ".w" >> List.map HtmlNode.innerText

type Word =
    | Word of string
    | PhrasalVerb of string

module Word =
    let private maybe wordCtor cssSelector node =
        node
        |> HtmlNode.cssSelectR cssSelector
        |> List.tryExactlyOne
        |> Option.map (HtmlNode.innerText >> wordCtor)

    let private (|PhrasalVerb|_|) = maybe Word.PhrasalVerb ".PHRVBHWD"
    let private (|Word|_|) = maybe Word.Word ".HWD"

    let extract (entryNode: HtmlNode) : Word =
        match entryNode with
        | PhrasalVerb w -> w
        | Word w -> w
        | _ -> failwith $"Unexpected entry's word: '{entryNode |> string}'"

type Label =
    | British
    | American

module Label =
    let private maybeLabel label cssSelector =
        Option.ifWith (HtmlNode.hasClass cssSelector) (Audio.extract >> tuple2 label)

    let private (|British|_|) = maybeLabel Label.British "brefile"
    let private (|American|_|) = maybeLabel Label.American "amefile"

    let private tryParse node =
        match node with
        | British l -> Some l
        | American l -> Some l
        | _ -> failwith $"Unexpected label: '{node |> string}'"

    let extract = HtmlNode.cssSelectR ".Head .speaker" >> List.choose tryParse

type Pronunciation =
    {
        /// Phonemic transcription
        /// See https://www.oxfordlearnersdictionaries.com/about/english/pronunciation_english
        Transcriptions: string list
        AmericanVariant: string option
        Audio: (Label * string) list
    }

module Pronunciation =
    let private extractTranscriptions =
        HtmlNode.cssSelectR ".Head > .PronCodes > .PRON"
        >> List.map (HtmlNode.innerText >> String.trimWhiteSpaces)

    let private extractAmericanVariant =
        HtmlNode.cssSelectR ".Head > .PronCodes > .AMEVARPRON"
        >> List.tryExactlyOne
        >> Option.map HtmlNode.directInnerText

    /// Root node: class="ldoceEntry"
    let extract (entryNode: HtmlNode) : Pronunciation =
        { Transcriptions = extractTranscriptions entryNode
          AmericanVariant = extractAmericanVariant entryNode
          Audio = Label.extract entryNode }

type SimpleExample = { Text: string; Audio: string }

module SimpleExample =
    /// Root node: class="EXAMPLE"
    let extract node : SimpleExample =
        { Text = node |> HtmlNode.innerText
          Audio = node |> HtmlNode.cssSelectR ".exafile" |> List.exactlyOne |> Audio.extract }

    /// Root node contains child nodes with class="EXAMPLE"
    let extractMany = HtmlNode.cssSelectR ".EXAMPLE" >> List.map extract

type GrammaticalType =
    | Preposition
    | PropositionalForm

type GrammaticalExamples =
    {
        Pattern: string
        Type: GrammaticalType
        /// Example of more than two examples: case__2
        Examples: SimpleExample list
    }

module GrammaticalExamples =
    let private maybeGrammaticalType grammaticalType cssSelector node =
        node
        |> HtmlNode.cssSelectR cssSelector
        |> List.tryExactlyOne
        |> Option.map HtmlNode.innerText
        |> Option.map (fun x ->
            { Pattern = x |> String.trimWhiteSpaces // Example: get-on__1
              Type = grammaticalType
              Examples = node |> SimpleExample.extractMany })

    let private (|Preposition|_|) =
        maybeGrammaticalType GrammaticalType.Preposition ".PROPFORMPREP"

    let private (|PropositionalForm|_|) =
        maybeGrammaticalType GrammaticalType.PropositionalForm ".PROPFORM"

    /// Root node: class="GramExa"
    let extract (exampleWithGrammarNode: HtmlNode) : GrammaticalExamples =
        match exampleWithGrammarNode with
        | Preposition t -> t
        | PropositionalForm t -> t
        | _ -> failwith $"Unexpected grammatical pattern: '{exampleWithGrammarNode |> string}'"

type CollocationalExamples =
    { Pattern: string
      Examples: SimpleExample list }

module CollocationalExamples =
    /// Root node: class="ColloExa"
    let extract (collocationNode: HtmlNode) : CollocationalExamples =
        collocationNode
        |> HtmlNode.cssSelectR ".COLLO"
        |> List.tryExactlyOne
        |> Option.map HtmlNode.innerText
        |> Option.map (fun x ->
            { Pattern = x
              Examples = collocationNode |> SimpleExample.extractMany })
        |> Option.defaultWith (fun () -> failwith $"Unexpected collocational example: '{collocationNode |> string}'")

type Example =
    | Simple of SimpleExample
    /// Example: case__1
    | Grammatical of GrammaticalExamples
    /// Example: go__4
    | Collocational of CollocationalExamples

module Example =
    let private (|GrammaticalExample|_|) =
        Option.ifWith (HtmlNode.hasClass "GramExa") (GrammaticalExamples.extract >> Grammatical)

    let private (|CollocationalExample|_|) =
        Option.ifWith (HtmlNode.hasClass "ColloExa") (CollocationalExamples.extract >> Collocational)

    let private (|SimpleExample|_|) =
        Option.ifWith (HtmlNode.hasClass "EXAMPLE") (SimpleExample.extract >> Simple)

    let private tryParse =
        function
        | GrammaticalExample e
        | CollocationalExample e
        | SimpleExample e -> Some e
        | _ -> None

    /// Root node: class="Sense" or class="Subsense"
    let extract: HtmlNode -> Example list = HtmlNode.elements >> List.choose tryParse

type SenseData =
    {
        Id: string option
        /// Example: word__1
        /// No definition: word__2
        Definition: string option
        Examples: Example list
        /// Example: get__13
        CrossRefs: string list
        /// Example: go__7, revise__2
        Thesauruses: string list
    }

type SubsenseData =
    {
        Definition: string option
        Examples: Example list
        /// Example: case__8
        CrossRefs: string list
    }

type SubsenseGroupData =
    {
        Id: string option
        Subsenses: SubsenseData list
        /// Example: get__4, get__5
        Thesauruses: string list
    }

type Sense =
    | Sense of SenseData
    | SubsenseGroup of SubsenseGroupData

module Sense =
    /// Root node: class="Sense"
    let private extractId: HtmlNode -> string option =
        HtmlNode.tryGetAttribute "id" >> Option.map HtmlAttribute.value

    /// Root node: class="Sense" or class="Subsense"
    let private extractDefinition =
        HtmlNode.cssSelectR ".DEF"
        >> List.tryExactlyOne
        >> Option.map (HtmlNode.innerText >> String.trimWhiteSpaces)

    /// Root node: class="Sense" or class="Subsense"
    let private extractCrossRefs =
        HtmlNode.cssSelectR ".Crossref"
        >> List.collect (HtmlNode.cssSelectR ".REFHWD")
        >> List.map HtmlNode.innerText

    /// Root node: class="Sense"
    let private extractThesauruses =
        HtmlNode.cssSelectR ".Thesref .REFHWD" >> List.map HtmlNode.innerText

    module SenseData =
        /// Root node: class="Sense"
        let extract (senseNode: HtmlNode) : SenseData =
            { Id = extractId senseNode
              Definition = extractDefinition senseNode
              Examples = Example.extract senseNode
              CrossRefs = extractCrossRefs senseNode
              Thesauruses = extractThesauruses senseNode }

    module SubsenseData =
        /// Root node: class="Subsense"
        let extract (subsenseNode: HtmlNode) : SubsenseData =
            { Definition = extractDefinition subsenseNode
              Examples = Example.extract subsenseNode
              CrossRefs = extractCrossRefs subsenseNode }

    let private (|SubsenseGroup|_|) subsenseGroupNode =
        subsenseGroupNode
        |> HtmlNode.cssSelectR ".Subsense"
        |> List.map SubsenseData.extract
        |> function
            | [] -> None
            | xs ->
                Sense.SubsenseGroup
                    { Id = extractId subsenseGroupNode
                      Subsenses = xs
                      Thesauruses = extractThesauruses subsenseGroupNode }
                |> Some

    let private (|Sense|_|) senseNode =
        try
            senseNode |> SenseData.extract |> Sense.Sense |> Some
        with _ ->
            None

    /// Root node: class="Sense"
    let extract (senseNode: HtmlNode) : Sense =
        match senseNode with
        | SubsenseGroup s -> s
        | Sense s -> s
        | _ -> failwith $"Unexpected sense or subsenses node: {senseNode |> string}"

type Entry =
    { Id: string option
      No: int option
      Word: Word
      Pronunciation: Pronunciation
      Senses: Sense list
      CrossRefs: string list }

module Entry =
    let private extractId =
        HtmlNode.tryGetAttribute "id" >> Option.map HtmlAttribute.value

    let private extractNo =
        HtmlNode.cssSelectR ".HOMNUM"
        >> List.tryExactlyOne
        >> Option.map (HtmlNode.innerText >> int)

    let private extractCrossRefs =
        HtmlNode.cssSelectR ".Tail .Crossref"
        >> List.collect (HtmlNode.cssSelectR ".crossRef")
        >> List.map HtmlNode.innerText

    let private extractSenses = HtmlNode.cssSelectR ".Sense" >> List.map Sense.extract

    /// Root node: class="ldoceEntry"
    let extract (entryNode: HtmlNode) : Entry =
        { Id = extractId entryNode
          No = extractNo entryNode
          Word = Word.extract entryNode
          Pronunciation = Pronunciation.extract entryNode
          Senses = extractSenses entryNode
          CrossRefs = extractCrossRefs entryNode }

type Dictionary =
    { WordFamily: WordFamily option
      Entries: Entry list }

module Dictionary =
    let private extractWordFamily =
        HtmlNode.cssSelectR ".wordfams"
        >> List.tryExactlyOne
        >> Option.map WordFamily.extract

    let private extractEntries =
        HtmlNode.cssSelectR ".dictentry .ldoceEntry" >> List.map Entry.extract

    /// Root node: class="dictionary"
    let extract dictionaryNode : Dictionary =
        { WordFamily = extractWordFamily dictionaryNode
          Entries = extractEntries dictionaryNode }

let lookup (word: string) : Dictionary =
    $"https://www.ldoceonline.com/search/english/direct/?q={word}"
    |> HtmlDocument.Load
    |> HtmlDocument.body
    |> HtmlNode.cssSelectR ".dictionary"
    |> List.exactlyOne
    |> Dictionary.extract
