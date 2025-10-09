/// Short for Longman Dictionary of Contemporary English (LDOCE)
module Longman

open FSharp.Data
open FSharpPlus
open FSharpPlus.Data
open FsToolkit.ErrorHandling
open Microsoft.FSharp.Core

module Option =
    let ifWith predicate onTrue inp =
        if inp |> predicate then Some(inp |> onTrue) else None

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
        |> function
            | [] -> None
            | [ node ] -> node |> HtmlNode.innerText |> wordCtor |> Ok |> Some
            | _ -> Some(Error $"There should be at most one word (CSS selector '{cssSelector}') in entry: '{node}'")

    let private (|PhrasalVerb|_|) = maybe Word.PhrasalVerb ".PHRVBHWD"
    let private (|Word|_|) = maybe Word.Word ".HWD"

    let extract (entryNode: HtmlNode) : Result<Word, string> =
        match entryNode with
        | PhrasalVerb w
        | Word w -> w
        | _ -> Error $"Unknown entry's word: '{entryNode}'"

module Audio =
    /// Root node contains "data-src-mp3" attribute
    let extract = HtmlNode.attribute "data-src-mp3" >> HtmlAttribute.value

type Label =
    | British
    | American

module Label =
    let private maybeLabel label cssSelector =
        Option.ifWith (HtmlNode.hasClass cssSelector) (Audio.extract >> tuple2 label)

    let private (|British|_|) = maybeLabel Label.British "brefile"
    let private (|American|_|) = maybeLabel Label.American "amefile"

    let private extract node =
        match node with
        | British l -> Ok l
        | American l -> Ok l
        | _ -> Error $"Unknown label: '{node}'"

    let extractMany: HtmlNode -> Result<(Label * string) list, string> =
        HtmlNode.cssSelectR ".Head .speaker" >> List.traverseResultM extract

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
    let extract (entryNode: HtmlNode) : Result<Pronunciation, string> =
        result {
            let transcriptions = entryNode |> extractTranscriptions
            let americanVariant = entryNode |> extractAmericanVariant
            let! audio = entryNode |> Label.extractMany

            return
                { Transcriptions = transcriptions
                  AmericanVariant = americanVariant
                  Audio = audio }
        }

type SimpleExample = { Text: string; Audio: string }

module SimpleExample =
    /// Root node: class="EXAMPLE"
    let extract (node: HtmlNode) : Result<SimpleExample, string> =
        result {
            let text = node |> HtmlNode.innerText

            let! audio =
                node
                |> HtmlNode.cssSelectR ".exafile"
                |> List.tryExactlyOne
                |> Result.requireSome
                    $"There should be exactly one audio (CSS selector `.exafile`) for simple example: '{node}'"
                |> Result.map Audio.extract

            return { Text = text; Audio = audio }
        }

    /// Root node contains child nodes with class="EXAMPLE"
    let extractMany: HtmlNode -> Result<SimpleExample list, string> =
        HtmlNode.cssSelectR ".EXAMPLE" >> List.traverseResultM extract

type GrammaticalType =
    | Preposition
    | PropositionalForm

type GrammaticalExamples =
    {
        Pattern: string
        Type: GrammaticalType
        /// Example of more than one example: case__2
        Examples: SimpleExample list
    }

module GrammaticalExamples =
    let private maybeGrammaticalType grammaticalType cssSelector node =
        node
        |> HtmlNode.cssSelectR cssSelector
        |> function
            | [] -> None
            | [ patternNode ] ->
                result {
                    let pattern = patternNode |> HtmlNode.innerText |> String.trimWhiteSpaces // Example: get-on__1
                    let! examples = node |> SimpleExample.extractMany

                    return
                        { Pattern = pattern
                          Type = grammaticalType
                          Examples = examples }
                }
                |> Some
            | _ ->
                $"There should be at most one grammatical type '{grammaticalType}' (CSS selector '{cssSelector}'). Node: '{node}'"
                |> Error
                |> Some

    let private (|Preposition|_|) =
        maybeGrammaticalType GrammaticalType.Preposition ".PROPFORMPREP"

    let private (|PropositionalForm|_|) =
        maybeGrammaticalType GrammaticalType.PropositionalForm ".PROPFORM"

    /// Root node: class="GramExa"
    let extract (exampleWithGrammarNode: HtmlNode) : Result<GrammaticalExamples, string> =
        match exampleWithGrammarNode with
        | Preposition t
        | PropositionalForm t -> t
        | _ -> Error $"Unknown grammatical pattern: '{exampleWithGrammarNode}'"

type CollocationalExamples =
    {
        /// Example: set__6
        Patterns: string nelist
        Examples: SimpleExample list
    }

module CollocationalExamples =
    /// Root node: class="ColloExa"
    let extract (collocationNode: HtmlNode) : Result<CollocationalExamples, string> =
        collocationNode
        |> HtmlNode.cssSelectR ".COLLO"
        |> map (HtmlNode.directInnerText >> String.trimWhiteSpaces) // Example: set__6
        |> function
            | [] -> Error $"No collocational patterns found: '{collocationNode}'"
            | patterns ->
                result {
                    let patterns = patterns |> NonEmptyList.ofList
                    let! examples = collocationNode |> SimpleExample.extractMany

                    return
                        { Patterns = patterns
                          Examples = examples }
                }

type Example =
    | Simple of SimpleExample
    /// Example: case__1
    | Grammatical of GrammaticalExamples
    /// Example: go__4
    | Collocational of CollocationalExamples

module Example =
    let private (|GrammaticalExample|_|) =
        Option.ifWith (HtmlNode.hasClass "GramExa") (GrammaticalExamples.extract >> Result.map Grammatical)

    let private (|CollocationalExample|_|) =
        Option.ifWith (HtmlNode.hasClass "ColloExa") (CollocationalExamples.extract >> Result.map Collocational)

    let private (|SimpleExample|_|) =
        Option.ifWith (HtmlNode.hasClass "EXAMPLE") (SimpleExample.extract >> Result.map Simple)

    let private nonExampleElements = [ "img" ]

    let private isNonExampleElement (node: HtmlNode) =
        let nodeName = node |> HtmlNode.name
        nonExampleElements |> List.contains nodeName

    let private nonExampleClasses =
        [ "sensenum"
          "SIGNPOST"
          "GRAM"
          "ACTIV"
          "DEF"
          "Thesref"
          "Crossref"
          "REGISTERLAB"
          "FIELD"
          "SYN"
          "OPP"
          "GEO"
          "BREQUIV"
          "AMEQUIV"
          "GramBox"
          "RELATEDWD"
          "Inflections"
          "LEXUNIT"
          "Variant"
          // ads
          "am-dictionary" ]

    let private hasNonExampleClass (node: HtmlNode) =
        nonExampleClasses |> exists (node |> flip HtmlNode.hasClass)

    let private tryParse node =
        match node with
        | GrammaticalExample e
        | CollocationalExample e
        | SimpleExample e -> Some e
        | _ -> Some(Error $"Unknown example node: '{node}'")

    /// Root node: class="Sense" or class="Subsense"
    let extract: HtmlNode -> Result<Example list, string> =
        HtmlNode.elements
        >> List.filter (isNonExampleElement >> not)
        >> List.filter (hasNonExampleClass >> not)
        >> List.choose tryParse
        >> List.sequenceResultM

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
        /// Example: big__4, go__1
        LexicalUnit: string option
        Definition: string
        Examples: Example list
        /// Example: case__8
        CrossRefs: string list
    }

type SubsenseGroupData =
    {
        Id: string option
        Subsenses: SubsenseData nelist
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
    let private extractDefinition node =
        node
        |> HtmlNode.cssSelectR ".DEF"
        |> function
            | [] -> Ok None
            | [ defNode ] -> Ok(Some(defNode |> HtmlNode.innerText |> String.trimWhiteSpaces))
            | _ ->
                $"There should be at most one definition (CSS selector '.DEF') in sense or subsense node: '{node}'"
                |> Error

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
        let extract (senseNode: HtmlNode) : Result<SenseData, string> =
            result {
                let id = senseNode |> extractId
                let! definition = senseNode |> extractDefinition
                let! examples = senseNode |> Example.extract
                let crossRefs = senseNode |> extractCrossRefs
                let thesauruses = senseNode |> extractThesauruses

                return
                    { Id = id
                      Definition = definition
                      Examples = examples
                      CrossRefs = crossRefs
                      Thesauruses = thesauruses }
            }

    module SubsenseData =
        /// Root node: class="Subsense"
        let private extractLexicalUnit node =
            node
            |> HtmlNode.cssSelectR ".LEXUNIT"
            |> function
                | [] -> Ok None
                | [ luNode ] -> Ok(Some(luNode |> HtmlNode.innerText))
                | _ ->
                    $"There should be at most one lexical unit (CSS selector '.LEXUNIT') in subsense node: '{node}'"
                    |> Error

        /// Root node: class="Subsense"
        let extract (subsenseNode: HtmlNode) : Result<SubsenseData, string> =
            result {
                let! lexicalUnit = subsenseNode |> extractLexicalUnit
                let! definitionOpt = subsenseNode |> extractDefinition

                let! definition =
                    definitionOpt
                    |> Result.requireSome $"There should be exactly one definition in subsense node: '{subsenseNode}'"

                let! examples = subsenseNode |> Example.extract
                let crossRefs = subsenseNode |> extractCrossRefs

                return
                    { LexicalUnit = lexicalUnit
                      Definition = definition
                      Examples = examples
                      CrossRefs = crossRefs }
            }

    let private (|SubsenseGroup|_|) (subsenseGroupNode: HtmlNode) : Result<Sense, string> option =
        option {
            let! subsenseNodes = subsenseGroupNode |> HtmlNode.cssSelectR ".Subsense" |> NonEmptyList.tryOfList

            return
                subsenseNodes
                |> NonEmptyList.traverse SubsenseData.extract
                |> Result.map (fun subsenses ->
                    Sense.SubsenseGroup
                        { Id = subsenseGroupNode |> extractId
                          Subsenses = subsenses
                          Thesauruses = subsenseGroupNode |> extractThesauruses })
        }

    let private (|Sense|_|) senseNode =
        senseNode |> SenseData.extract |> Result.map Sense.Sense |> Some

    /// Root node: class="Sense"
    let extract (senseNode: HtmlNode) : Result<Sense, string> =
        match senseNode with
        | SubsenseGroup s
        | Sense s -> s
        | _ ->
            $"Unknown sense or subsenses node: '{senseNode}'. This should not happen as Sense always returns Some."
            |> Error

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

    let private extractSenses =
        HtmlNode.cssSelectR ".Sense" >> List.traverseResultM Sense.extract

    /// Root node: class="ldoceEntry"
    let extract (entryNode: HtmlNode) : Result<Entry, string> =
        result {
            let id = entryNode |> extractId
            let no = entryNode |> extractNo
            let! word = entryNode |> Word.extract
            let! pronunciation = entryNode |> Pronunciation.extract
            let! senses = entryNode |> extractSenses
            let crossRefs = entryNode |> extractCrossRefs

            return
                { Id = id
                  No = no
                  Word = word
                  Pronunciation = pronunciation
                  Senses = senses
                  CrossRefs = crossRefs }
        }

type Dictionary =
    { WordFamily: WordFamily option
      Entries: Entry list }

module Dictionary =
    let private extractWordFamily node =
        node
        |> HtmlNode.cssSelectR ".wordfams"
        |> function
            | [] -> Ok None
            | [ wfNode ] -> Ok(Some(wfNode |> WordFamily.extract))
            | _ -> Error $"There should be at most one word family node. Node: '{node}'"

    let private extractEntries node =
        node
        |> HtmlNode.cssSelectR ".dictentry .ldoceEntry"
        |> List.traverseResultM Entry.extract

    /// Root node: class="dictionary"
    let extract dictionaryNode : Result<Dictionary, string> =
        result {
            let! wordFamily = dictionaryNode |> extractWordFamily
            let! entries = dictionaryNode |> extractEntries

            return
                { WordFamily = wordFamily
                  Entries = entries }
        }

let lookup (word: string) : Result<Dictionary, string> =
    $"https://www.ldoceonline.com/search/english/direct/?q={word}"
    |> HtmlDocument.Load
    |> HtmlDocument.body
    |> HtmlNode.cssSelectR ".dictionary"
    |> List.exactlyOne
    |> Dictionary.extract
