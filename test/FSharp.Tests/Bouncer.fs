module BouncerShift

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
        function
        | Guarding _ -> true
        | _ -> false

    let name: Name =
        function
        | Guarding(GuardingBouncer(name, _)) -> name
        | HavingABreak(HavingABreakBouncer(name, _)) -> name

    let count: Count =
        function
        | Guarding(GuardingBouncer(_, count)) -> count
        | HavingABreak(HavingABreakBouncer(_, count)) -> count

    let startGuarding: StartGuarding =
        fun (HavingABreakBouncer(name, count)) -> GuardingBouncer(name, count)

    let haveABreak: HaveABreak =
        fun (GuardingBouncer(name, count)) -> HavingABreakBouncer(name, count)

    let reportNewPeople: ReportNewPeople =
        fun newPeople (GuardingBouncer(name, count)) -> GuardingBouncer(name, count + newPeople)

    let startShift: StartShift = fun name -> HavingABreakBouncer(name, 0)


open Bouncers
open Reusables

type BouncerShift = private BouncerShift of Map<string, Bouncer>

type BouncerShiftProblem =
    | BouncerAlreadyGuarding
    | AtLeastTwoGuarding
    | BouncerAlreadyHavingABreak
    | BouncerIsNotGuarding
    | Exceeding100People

type private Start = string list -> BouncerShift
type private StartGuarding = string -> BouncerShift -> Result<BouncerShift, BouncerShiftProblem * BouncerShift>
type private ScheduleBreakFor = string -> BouncerShift -> Result<BouncerShift, BouncerShiftProblem * BouncerShift>
type private ReportNewPeople = string -> int -> BouncerShift -> Result<BouncerShift, BouncerShiftProblem * BouncerShift>

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
    fun name (BouncerShift bouncerMap) ->
        bouncerMap
        |> Map.find name
        |> function
            | Guarding _ -> Error(BouncerAlreadyGuarding, BouncerShift bouncerMap)
            | HavingABreak rested ->
                rested
                |> startGuarding
                |> Bouncer.Guarding
                |> fun guard -> bouncerMap |> Map.change2 name guard
                |> BouncerShift
                |> Ok

let scheduleBreakFor: ScheduleBreakFor =
    let lessThanThreeGuarding bouncerMap =
        let guardCount =
            bouncerMap |> Map.filter (fun _ bouncer -> bouncer |> isGuarding) |> Map.count

        guardCount < 3

    fun name (BouncerShift bouncerMap) ->
        if bouncerMap |> lessThanThreeGuarding then
            Error(AtLeastTwoGuarding, BouncerShift bouncerMap)
        else
            bouncerMap
            |> Map.find name
            |> function
                | HavingABreak _ -> Error(BouncerAlreadyHavingABreak, BouncerShift bouncerMap)
                | Guarding guard ->
                    guard
                    |> haveABreak
                    |> Bouncer.HavingABreak
                    |> fun rested -> bouncerMap |> Map.change2 name rested
                    |> BouncerShift
                    |> Ok

let reportNewPeople: ReportNewPeople =
    let wouldExceed100PeopleWith newPeople bouncerMap =
        let partyingPeople = bouncerMap |> Map.toSeq |> Seq.map snd |> Seq.sumBy count

        partyingPeople + newPeople > 100

    fun name newPeople (BouncerShift bouncerMap) ->
        if bouncerMap |> wouldExceed100PeopleWith newPeople then
            Error(Exceeding100People, BouncerShift bouncerMap)
        else
            bouncerMap
            |> Map.find name
            |> function
                | HavingABreak _ -> Error(BouncerIsNotGuarding, BouncerShift bouncerMap)
                | Guarding guard ->
                    guard
                    |> reportNewPeople newPeople
                    |> Bouncer.Guarding
                    |> fun guard -> bouncerMap |> Map.change2 name guard
                    |> BouncerShift
                    |> Ok
