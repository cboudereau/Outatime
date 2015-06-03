module TestData

open FsCheck
open Temporality

let getPeriod (d1,d2) = 
    let minDate = min d1 d2
    let maxDate = max d1 d2
    { StartDate = minDate
      EndDate = maxDate }

type EmptyPeriod = 
    static member Values() =
        Arb.generate<DateTime>
        |> Gen.map(fun d -> getPeriod (d,d))
        |> Arb.fromGen

type RandomPeriod = 
    static member Values() = 
        let randomPeriod = 
            Arb.generate<DateTime>
            |> Gen.two
            |> Gen.map (getPeriod)
        
        let emptyPeriod = 
            Arb.generate<DateTime>
            |> Gen.map(fun d -> getPeriod(d,d))

        Gen.frequency [ (3, randomPeriod); (1, emptyPeriod) ]
        |> Arb.fromGen

let randomValidPeriodGen = 
    Arb.generate<DateTime * DateTime> |> Gen.map (getPeriod)

let toTemporaries l = 
    let rec internalToTemporaries s l = 
        seq { 
            match l with
            | (value, duration) :: tail -> 
                yield { Period = Period.from s duration
                        Value = value }
                yield! internalToTemporaries (s + duration) tail
            | [] -> yield! []
        }
    internalToTemporaries DateTime.MinValue l

let validTimeSpanGen = 
    Gen.choose (int (TimeSpan.forNever.TotalMilliseconds), int (TimeSpan.forEver.TotalMilliseconds / 150000.)) 
    |> Gen.map (fun i -> TimeSpan.FromMilliseconds(float i))

let validContiguousPeriodsGen = 
    let rec toPeriods s l = 
        seq { 
            match l with
            | duration :: tail -> 
                yield Period.from s duration
                yield! toPeriods (s + duration) tail
            | [] -> yield! []
        }
    validTimeSpanGen
    |> Gen.listOf
    |> Gen.map (toPeriods DateTime.MinValue)

let singleValueContiguousTemporalGen value = 
    let toTemporary period = { Period = period; Value = value } 
    let toTemporal periods = periods |> Seq.map (toTemporary) |> Temporal.toTemporal
    validContiguousPeriodsGen |> Gen.map (toTemporal)

let contiguousTemporalGen valueGen = 
    Gen.map2(fun value period -> (value, period)) valueGen validContiguousPeriodsGen

type NoOverlapTemporaries = 
    static member Values() = 
        let getDuration d1 d2 = 
            if d2 > d1 then d2 - d1
            else d1 - d2
        Gen.frequency [ 1, gen { return "Hello" }
                        2, gen { return "World" } ]
        |> Gen.map (fun s -> (s, TimeSpan.FromDays(1.)))
        |> Gen.listOf
        |> Gen.map (toTemporaries)
        |> Arb.fromGen

type HelloValidRepresentableTemporal = 
    static member Values() = singleValueContiguousTemporalGen "hello" |> Arb.fromGen

type HelloRandomTemporal = 
    static member Values() = 
        let toPositiveDuration (timeSpan : TimeSpan) = 
            if timeSpan < TimeSpan.Zero then timeSpan.Negate()
            else timeSpan
        
        let toTestTemporary (n, date) = 
            { Period = 
                  { StartDate = date
                    EndDate = 
                        date + (n
                                |> TimeSpan.forNDays
                                |> toPositiveDuration) }
              Value = "hello" }
        
        let toTemporal u = 
            u
            |> Seq.map (toTestTemporary)
            |> Temporal.toTemporal
        
        Arb.generate<int * DateTime>
        |> Gen.listOf
        |> Gen.map (toTemporal)
        |> Arb.fromGen

let randomTemporalGen = 
    Arb.generate<DateTime * DateTime * string>
    |> Gen.map (fun (d1, d2, value) -> 
           { Period = getPeriod (d1, d2)
             Value = value })
    |> Gen.listOf
    |> Gen.map (fun temporaries -> temporaries |> Temporal.toTemporal)

type RandomViewTemporal = 
    static member Values() = 
        let randomTemporal = randomTemporalGen
        let randomPeriod = randomValidPeriodGen
        Gen.map2 (fun period temporal -> (period, temporal)) randomPeriod randomTemporal |> Arb.fromGen
