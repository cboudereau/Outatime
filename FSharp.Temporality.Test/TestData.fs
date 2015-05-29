module TestData

open FsCheck

open Temporality

let getPeriod d1 d2 = 
    let minDate = min d1 d2
    let maxDate = max d1 d2
    { StartDate = minDate
      EndDate = maxDate }

type TwoPeriodsArb = 
    static member Values() =
        Arb.generate<DateTime>
        |> Gen.four
        |> Gen.map(fun (d1, d2, d3, d4) -> ( getPeriod d1 d2, getPeriod d3 d4 ))
        |> Arb.fromGen

let toTemporaries l = 
    let rec internalToTemporaries s l =
        seq {
            match l with
            | (value, duration) :: tail -> 
                yield { Period = Period.from s duration; Value = value }
                yield! internalToTemporaries (s + duration) tail
            | [] -> yield! [] }

    internalToTemporaries DateTime.MinValue l

type NoOverlapTemporaries = 
    static member Values() = 
        let getDuration d1 d2 = if d2 > d1 then d2 - d1 else d1 - d2
        
        Gen.frequency [ 1, gen { return "Hello" }; 2, gen { return "World" } ]
        |> Gen.map(fun s -> (s, TimeSpan.FromDays(1.)))
        |> Gen.listOf
        |> Gen.map(toTemporaries)
        |> Arb.fromGen