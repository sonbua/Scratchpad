/// Converted from https://github.com/nghiaht/AmLichVietNam
module Calendar

/// Returns the number of days since 1 January 4713 BC (Julian calendar)
let private jdFromDate (dd: int) (mm: int) (yy: int) : int =
    let a = (14 - mm) / 12
    let y = yy + 4800 - a
    let m = mm + 12 * a - 3
    let jd = dd + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045

    if (jd < 2299161) then
        dd + (153 * m + 2) / 5 + 365 * y + y / 4 - 32083
    //jd = jd - 1721425;
    else
        jd

/// <summary>
/// http://www.tondering.dk/claus/calendar.html
/// Section: Is there a formula for calculating the Julian day number?
/// </summary>
/// <param name="jd">The number of days since 1 January 4713 BC (Julian calendar).</param>
let private jdToDate (jd: int) : int * int * int =
    if (jd > 2299160) then
        // After 5/10/1582, Gregorian calendar
        let a = jd + 32044
        let b = (4 * a + 3) / 146097
        let c = a - (b * 146097) / 4
        (b, c)
    else
        let b = 0
        let c = jd + 32082
        (b, c)
    |> fun (b, c) ->
        let d = (4 * c + 3) / 1461
        let e = c - (1461 * d) / 4
        let m = (5 * e + 2) / 153
        let day = e - (153 * m + 2) / 5 + 1
        let month = m + 3 - 12 * (m / 10)
        let year = b * 100 + d - 4800 + m / 10
        day, month, year

let private toInt (f: float) : int = f |> floor |> int

let private SunLongitudeAA98 (jdn: float) : float =
    let T = (jdn - 2451545.0) / 36525.0 // Time in Julian centuries from 2000-01-01 12:00:00 GMT
    let T2 = T * T
    let dr = Math.PI / (float 180) // degree to radian
    let M = 357.52910 + 35999.05030 * T - 0.0001559 * T2 - 0.00000048 * T * T2 // mean anomaly, degree
    let L0 = 280.46645 + 36000.76983 * T + 0.0003032 * T2 // mean longitude, degree

    let DL =
        (1.914600 - 0.004817 * T - 0.000014 * T2) * Math.Sin(dr * M)
        + (0.019993 - 0.000101 * T) * Math.Sin(dr * 2.0 * M)
        + 0.000290 * Math.Sin(dr * 3.0 * M)

    let L = L0 + DL // true longitude, degree

    // Normalize to (0, 360)
    L - 360.0 * ((L / 360.0) |> toInt |> float)

/// <summary>
/// Julian day number of the kth new moon after (or before) the New Moon of 1900-01-01 13:51 GMT.
/// Accuracy: 2 minutes
/// Algorithm from: Astronomical Algorithms, by Jean Meeus, 1998
/// </summary>
/// <returns>The Julian date number (number of days since noon UTC on 1 January 4713 BC) of the New Moon</returns>
let private NewMoonAA98 (k: int) : float =
    let k = float k
    let T = k / 1236.85 // Time in Julian centuries from 1900 January 0.5
    let T2 = T * T
    let T3 = T2 * T
    let dr = Math.PI / 180.0
    let Jd1 = 2415020.75933 + 29.53058868 * k + 0.0001178 * T2 - 0.000000155 * T3
    let Jd1 = Jd1 + 0.00033 * Math.Sin((166.56 + 132.87 * T - 0.009173 * T2) * dr) // Mean new moon
    let M = 359.2242 + 29.10535608 * k - 0.0000333 * T2 - 0.00000347 * T3 // Sun's mean anomaly
    let Mpr = 306.0253 + 385.81691806 * k + 0.0107306 * T2 + 0.00001236 * T3 // Moon's mean anomaly
    let F = 21.2964 + 390.67050646 * k - 0.0016528 * T2 - 0.00000239 * T3 // Moon's argument of latitude

    let C1 =
        (0.1734 - 0.000393 * T) * Math.Sin(M * dr) + 0.0021 * Math.Sin(dr * 2.0 * M)

    let C1 = C1 - 0.4068 * Math.Sin(Mpr * dr) + 0.0161 * Math.Sin(dr * 2.0 * Mpr)
    let C1 = C1 - 0.0004 * Math.Sin(dr * 3.0 * Mpr)
    let C1 = C1 + 0.0104 * Math.Sin(dr * 2.0 * F) - 0.0051 * Math.Sin(dr * (M + Mpr))

    let C1 =
        C1 - 0.0074 * Math.Sin(dr * (M - Mpr)) + 0.0004 * Math.Sin(dr * (2.0 * F + M))

    let C1 =
        C1
        - 0.0004 * Math.Sin(dr * (2.0 * F - M))
        - 0.0006 * Math.Sin(dr * (2.0 * F + Mpr))

    let C1 =
        C1
        + 0.0010 * Math.Sin(dr * (2.0 * F - Mpr))
        + 0.0005 * Math.Sin(dr * (2.0 * Mpr + M))

    let deltaT =
        if (T < -11) then
            0.001 + 0.000839 * T + 0.0002261 * T2 - 0.00000845 * T3 - 0.000000081 * T * T3
        else
            -0.000278 + 0.000265 * T + 0.000262 * T2

    Jd1 + C1 - deltaT

let private NewMoon (k: int) : float =
    //return CC2K.newMoonTime(k);
    NewMoonAA98 k

/// Solar longitude in degrees
/// Algorithm from: Astronomical Algorithms, by Jean Meeus, 1998
/// <param name="jdn">Number of days since noon UTC on 1 January 4713 BC.</param>
let private SunLongitude (jdn: float) : float =
    //return CC2K.sunLongitude(jdn);
    SunLongitudeAA98 jdn

let private getSunLongitude (dayNumber: int) (timeZone: float) : float =
    SunLongitude((float dayNumber) - 0.5 - timeZone / 24.0)

let private getNewMoonDay (k: int) (timeZone: float) : int =
    (NewMoon k + 0.5 + timeZone / 24.0) |> toInt

let private getLunarMonth11 (yy: int) (timeZone: float) : int =
    let off = ((31, 12, yy) |||> jdFromDate |> float) - 2415021.076998695
    let k = (off / 29.530588853) |> toInt
    let nm = getNewMoonDay k timeZone
    let sunLong = (getSunLongitude nm timeZone) / 30.0 |> toInt

    if (sunLong >= 9) then
        getNewMoonDay (k - 1) timeZone
    else
        nm

let private getLeapMonthOffset (a11: int) (timeZone: float) : int =
    let k = (0.5 + ((float a11) - 2415021.076998695) / 29.530588853) |> toInt

    let getSunLongitude k i timeZone =
        let newMoonDay = getNewMoonDay (k + i) timeZone
        (getSunLongitude newMoonDay timeZone) / 30.0 |> toInt

    let mutable last = getSunLongitude k 1 timeZone // Month 11 contains point of sun longitude 3*PI/2 (December solstice)
    let mutable arc = getSunLongitude k 2 timeZone
    let mutable i = 2 // We start with the month following lunar month 11

    while (arc <> last && i < 14) do
        last <- arc
        i <- i + 1
        arc <- getSunLongitude k i timeZone

    i - 1

let solar2Lunar (day: int) (month: int) (year: int) (timeZone: float) : int * int * int * bool =
    let dayNumber = jdFromDate day month year
    let k = (((float dayNumber) - 2415021.076998695) / 29.530588853) |> toInt

    let monthStart =
        let v = getNewMoonDay (k + 1) timeZone
        if (v > dayNumber) then getNewMoonDay k timeZone else v

    let a11, b11, tmpLunarYear =
        let lunarMonth11 = getLunarMonth11 year timeZone

        if (lunarMonth11 >= monthStart) then
            getLunarMonth11 (year - 1) timeZone, lunarMonth11, year
        else
            lunarMonth11, getLunarMonth11 (year + 1) timeZone, year + 1

    let lunarDay = dayNumber - monthStart + 1
    let diff = (monthStart - a11) / 29 // |> toInt
    let mutable lunarLeap = false
    let mutable lunarMonth = diff + 11

    if (b11 - a11 > 365) then
        let leapMonthDiff = getLeapMonthOffset a11 timeZone

        if (diff >= leapMonthDiff) then
            lunarMonth <- diff + 10

            if (diff = leapMonthDiff) then
                lunarLeap <- true

    if (lunarMonth > 12) then
        lunarMonth <- lunarMonth - 12

    let lunarYear =
        if (lunarMonth >= 11 && diff < 4) then
            tmpLunarYear - 1
        else
            tmpLunarYear

    lunarDay, lunarMonth, lunarYear, lunarLeap

let lunar2Solar
    (lunarDay: int)
    (lunarMonth: int)
    (lunarYear: int)
    (lunarLeap: bool)
    (timeZone: float)
    : int * int * int =
    let a11, b11 =
        if (lunarMonth < 11) then
            let a11 = getLunarMonth11 (lunarYear - 1) timeZone
            let b11 = getLunarMonth11 lunarYear timeZone
            a11, b11
        else
            let a11 = getLunarMonth11 lunarYear timeZone
            let b11 = getLunarMonth11 (lunarYear + 1) timeZone
            a11, b11

    let k = (0.5 + ((float a11) - 2415021.076998695) / 29.530588853) |> toInt
    let mutable off = lunarMonth - 11

    if (off < 0) then
        off <- off + 12

    if (b11 - a11 > 365) then
        let leapOff = getLeapMonthOffset a11 timeZone
        let mutable leapMonth = leapOff - 2

        if (leapMonth < 0) then
            leapMonth <- leapMonth + 12

        if (lunarLeap && lunarMonth <> leapMonth) then
            // TODO: return new int[] { 0, 0, 0 };
            failwith "not implemented"
        elif (lunarLeap || off >= leapOff) then
            off <- off + 1

    let monthStart = getNewMoonDay (k + off) timeZone
    jdToDate (monthStart + lunarDay - 1)
