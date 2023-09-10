module FSharp.Tests

module internal Bouncers =
    type GuardingBouncer = private GuardingBouncer of name: string * count: int
    type HavingABreakBouncer = private HavingABreakBouncer of name: string * count: int

    type Bouncer =
        | Guarding of GuardingBouncer
        | HavingABreak of HavingABreakBouncer

    type private IsGuarding = Bouncer -> bool
    type private Name = Bouncer -> string
    type private Count = Bouncer -> int
    type private StartGuarding = HavingABreakBouncer -> GuardingBouncer
    type private HaveABreak = GuardingBouncer -> HavingABreakBouncer
    type private ReportNewPeople = int -> GuardingBouncer -> GuardingBouncer
    type private StartShift = string -> HavingABreakBouncer

    let isGuarding: IsGuarding =
        fun bouncer ->
            match bouncer with
            | Guarding _ -> true
            | _ -> false

    let name: Name =
        fun bouncer ->
            match bouncer with
            | Guarding(GuardingBouncer(name, _)) -> name
            | HavingABreak(HavingABreakBouncer(name, _)) -> name

    let count: Count =
        fun bouncer ->
            match bouncer with
            | Guarding(GuardingBouncer(_, count)) -> count
            | HavingABreak(HavingABreakBouncer(_, count)) -> count

    let startGuarding: StartGuarding =
        fun (HavingABreakBouncer(name, count)) -> GuardingBouncer(name, count)

    let haveABreak: HaveABreak =
        fun (GuardingBouncer(name, count)) -> HavingABreakBouncer(name, count)

    let reportNewPeople: ReportNewPeople =
        fun newPeople (GuardingBouncer(name, count)) -> GuardingBouncer(name, count + newPeople)

    let startShift: StartShift = fun name -> HavingABreakBouncer(name, 0)


module BouncerShift =
    open Bouncers

    type BouncerShift = private BouncerShift of Map<string, Bouncer>

    type private Start = string list -> BouncerShift
    type private StartGuarding = string -> BouncerShift -> BouncerShift
    type private ScheduleBreakFor = string -> BouncerShift -> BouncerShift
    type private ReportNewPeople = string -> int -> BouncerShift -> BouncerShift

    let start: Start =
        let ensureAtLeastThreeBouncers names =
            let count = names |> List.length

            if count < 3 then
                failwith "Require at least three bouncers."

        fun names ->
            ensureAtLeastThreeBouncers names

            names
            |> List.map (fun name -> (name, name |> startShift |> Bouncer.HavingABreak))
            |> Map.ofList
            |> BouncerShift

    let startGuarding: StartGuarding =
        let startGuarding' bouncer =
            match bouncer with
            | HavingABreak rested -> rested |> startGuarding |> Bouncer.Guarding
            | Guarding _ -> bouncer

        fun name (BouncerShift bouncerMap) -> bouncerMap |> Map.change name (Option.map startGuarding') |> BouncerShift

    let scheduleBreakFor: ScheduleBreakFor =
        let ensureAtLeastThreeGuards bouncerMap =
            let guardCount =
                bouncerMap |> Map.toSeq |> Seq.map snd |> Seq.filter isGuarding |> Seq.length

            if guardCount < 3 then
                failwith "There must be at least two bouncers guarding."

        let haveABreak' bouncer =
            match bouncer with
            | Guarding guard -> guard |> haveABreak |> Bouncer.HavingABreak
            | HavingABreak _ -> bouncer

        fun name (BouncerShift bouncerMap) ->
            ensureAtLeastThreeGuards bouncerMap

            bouncerMap |> Map.change name (Option.map haveABreak') |> BouncerShift

    let reportNewPeople: ReportNewPeople =
        let wouldBeLessThan100PeopleWith newPeople bouncerMap =
            let partyingPeople = bouncerMap |> Map.toSeq |> Seq.map snd |> Seq.sumBy count

            partyingPeople + newPeople <= 100

        let reportNewPeople' newPeople bouncer =
            match bouncer with
            | Guarding guard -> guard |> reportNewPeople newPeople |> Bouncer.Guarding
            | HavingABreak _ -> failwithf $"{bouncer |> name} is currently not guarding."

        fun name newPeople (BouncerShift bouncerMap) ->
            if wouldBeLessThan100PeopleWith newPeople bouncerMap then
                bouncerMap
                |> Map.change name (Option.map (reportNewPeople' newPeople))
                |> BouncerShift
            else
                failwith "There should be no more than 100 people in the party."
