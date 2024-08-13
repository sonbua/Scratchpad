module LongmanActivator

/// A keyword represents a group of individual word-meanings or phrase-meanings that generally share the same idea,
/// concept, or semantic area.
/// Example: eat
type Keyword =
    | Keyword of string
    /// Example: about/approximately
    | KeywordGroup of string list

type Reference =
    | Reference of Keyword list
    | Opposite of Keyword list
    | Contextual of context: string * Keyword list

/// Tells you whether a word is British or American
type Label =
    | British
    | American

/// Example: nibble
type Word =
    | Word of string
    | Phrase of string
    | LabeledWord of word: string * label: Label
    | LabeledPhrase of phrase: string * label: Label

type Pattern =
    | Plain of string
    /// Example: accident 2 -> pile-up -> multiple pile-up (British)
    /// Example: accidentally 1 -> accidentally/by accident -> completely by accident ALSO quite by accident (British)
    | Labeled of pattern: string * Label

/// Example: cry your eyes out (= cry a lot because you are very upset)
/// The poor kid's so miserable, he's upstairs crying his eyes out.
type PatternAndUsage =
    { Pattern: Pattern list
      Definition: string option
      Examples: string list }

type PatternData =
    /// Shows which prepositions and grammatical patterns to use
    | Grammar of PatternAndUsage
    /// Shows words that are commonly used with the word you're looking at; or
    /// selection restriction on the type of subject or object a verb is normally used with,
    /// or on the type of noun that an adjective normally qualifies.
    | Collocation of PatternAndUsage

type Subentry =
    { Word: Word list
      Definition: string option
      Examples: string list
      Patterns: PatternData list }

type Entry =
    { Word: Word list
      /// Example: about 2 -> focus on ALSO centre on (British) / center on (American)
      Alternative: Word list
      /// Shows you how the word is different from the other words in the section
      /// Example: to eat only a small part of a meal, especially because you feel ill or unhappy
      Definition: string option
      /// Show how the word is typically used and help you to get a 'feel' for the word
      /// I sat picking at my dinner, wishing I were somewhere else.
      Examples: string list
      Patterns: PatternData list
      /// Example: accept 5 -> accept -> acceptance
      WordFamily: Subentry list }

/// Aka section or subsection
type AspectData =
    { No: int
      /// Aka heading or name
      /// Example: eat 7 : to eat small amounts of food
      Meaning: string
      /// Example: eat 7 -> [nibble; pick at; hardly touch your food/dinner/meal etc]
      Entries: Entry list }

type Aspect =
    | Aspects of AspectData list
    /// Example: above; because; check
    | SingleAspect of Entry list

type Concept =
    { Keyword: Keyword
      GeneralDefinition: string option
      /// Useful references to other related keywords in the dictionary.
      AccessMap: Reference list
      /// Major aspects of the concept.
      /// Aka sections.
      /// Example: about -> [about a subject or person; to have something as the main subject]
      Aspects: Aspect }
