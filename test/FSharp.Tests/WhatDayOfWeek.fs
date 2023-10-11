module WhatDayOfWeek

open System
open FSharpPlus
open FsUnit
open Xunit

let countDaysUpToYear year =
    let year = year - 1
    let century = year / 100

    year * 365 + year / 4 - century + century / 4

let (|LeapYear|CommonYear|) year =
    let mod' x y = y % x = 0

    match year with
    | _ when mod' 400 year -> LeapYear
    | _ when mod' 100 year -> CommonYear
    | _ when mod' 4 year -> LeapYear
    | _ -> CommonYear

let daysInMonth year month =
    match year with
    | LeapYear -> [ 31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 ] // number of days in each month in a leap year
    | CommonYear -> [ 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 ]
    |> item (month - 1)

let countDaysInYearUpToMonth year month =
    [ 1 .. (month - 1) ] |> map (daysInMonth year) |> sum

let countDays (year, month, day) =
    countDaysUpToYear year + countDaysInYearUpToMonth year month + day

let whatDayOfWeek = countDays >> (fun x -> x % 7) >> enum<DayOfWeek>

[<Theory>]
[<InlineData(2022, 7, 5, DayOfWeek.Tuesday)>]
[<InlineData(2022, 1, 1, DayOfWeek.Saturday)>]
let ``Given year month day Should return correct day of week`` year month day (expected: DayOfWeek) =
    whatDayOfWeek (year, month, day) |> should equal expected
