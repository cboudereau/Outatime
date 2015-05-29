module TemporalTest

open Temporality
open Xunit
open FsCheck
open FsUnit.Xunit

let jan15 day = DateTime(2015, 01, day)

let toPositiveDuration (timeSpan : TimeSpan) = 
    if timeSpan < TimeSpan.Zero then timeSpan.Negate()
    else timeSpan

let getPeriod d1 d2 = 
    let minDate = min d1 d2
    let maxDate = max d1 d2
    { StartDate = minDate
      EndDate = maxDate }

let getTemporary d1 d2 v = { Period = getPeriod d1 d2; Value = v }

let toPeriod (d1, d2) = 
    match d2 > d1 with
    | true -> { StartDate = d1; EndDate = d2 }
    | _ -> { StartDate = d2; EndDate = d1 }

let toTemporary (v, d1, d2) = 
    let p = toPeriod (d1, d2)
    { Period = p; Value = v }

[<Fact>]
let ``TimeSpan composition test``()=
    TimeSpan.forEver + TimeSpan.forNever |> should equal TimeSpan.forEver
    TimeSpan.forEver - TimeSpan.forNever |> should equal TimeSpan.forEver
    TimeSpan.forNever - TimeSpan.forNever |> should equal TimeSpan.forNever
    TimeSpan.forNever + TimeSpan.forNever |> should equal TimeSpan.forNever

    DateTime.MaxValue - DateTime.MinValue |> should equal TimeSpan.forEver

[<Fact>]
let ``Period should be display with math interval notation for half open interval``()=
    let p = { StartDate = jan15 1; EndDate = jan15 2 }
    
    p.ToString() |> should equal "[1/1/2015 0:00:00, 2/1/2015 0:00:00)" 

    Period.Always.ToString() |> should equal "Always"
    Period.Never.ToString() |> should equal "Never"

[<Fact>]
let ``Temporary should be displayed same as Period``()=
    let t = { Period = Period.Always; Value = "Hello" }

    t.ToString() |> should equal @"Always : ""Hello"""

[<Fact>]
let ``split period of 20 days by 5 should be 4 period of 5 days``()=
    let actual = 
        [ { Period = Period.from (jan15 1) (TimeSpan.forNDays 19); Value = "Hello" } ]
        |> Temporal.toTemporal
        |> Temporal.split (TimeSpan.forNDays 5)
    actual
    |> should equal 
        ([ { Period = Period.from (jan15 1) (TimeSpan.forNDays 5); Value = "Hello" }
           { Period = Period.from (jan15 6) (TimeSpan.forNDays 5); Value = "Hello" }
           { Period = Period.from (jan15 11) (TimeSpan.forNDays 5); Value = "Hello" }
           { Period = Period.from (jan15 16) (TimeSpan.forNDays 4); Value = "Hello" } ] |> Temporal.toTemporal)

[<Fact>]
let ``split empty period is empty period``() = 
    let arb = 
        Arb.generate<DateTime>
        |> Gen.map (fun d -> 
               [ { Period = 
                       { StartDate = d
                         EndDate = d }
                   Value = "pouet" } ]
               |> Temporal.toTemporal)
        |> Arb.fromGen
    
    let ``check that startdate is equal to enddate`` t = 
        t |> Temporal.split (TimeSpan.forNDays 5) = t
    
    Check.QuickThrowOnFailure(Prop.forAll arb <| ``check that startdate is equal to enddate``)

[<Fact>]
let ``split period by n days``()=
    let toTestTemporary (n, date) = 
        { Period = { StartDate = date; EndDate = date + (n |> TimeSpan.forNDays |> toPositiveDuration) }
          Value = "hello" }
    
    let toTestTemporaries u = u |> Seq.map(toTestTemporary) |> Temporal.toTemporal

    let arb = 
        Arb.generate<int*DateTime>
        |> Gen.listOf
        |> Gen.map(toTestTemporaries)
        |> Arb.fromGen
    
    let splitPeriod = TimeSpan.forNDays 5

    let ``check that period are sorted by start date in order to have correct interval`` temporal = 
        let actual = temporal |> Temporal.split splitPeriod
        (actual.Values |> Seq.sortBy(fun t -> t.Period.StartDate) |> Seq.toList) = (actual.Values)

    let ``check that all period are less than split period`` temporal = 
        (temporal
        |> Temporal.split splitPeriod).Values
        |> Seq.forall(fun v -> v.Period.Duration <= splitPeriod)

    Check.QuickThrowOnFailure(Prop.forAll arb <| ``check that all period are less than split period``)
    Check.QuickThrowOnFailure(Prop.forAll arb <| ``check that period are sorted by start date in order to have correct interval``)

[<Fact>]
let ``when value are equal on intersect periods should merge``() =
    [] |> Temporal.toTemporal |> Temporal.merge |> should equal ([] |> Temporal.toTemporal)
    
    let actual = 
        [ { Period = Period.from (jan15 1) (TimeSpan.forNDays 5); Value = "Hello" }
          { Period = Period.from (jan15 11) (TimeSpan.forNDays 5); Value = "Hello" }
          { Period = Period.from (jan15 06) (TimeSpan.forNDays 5); Value = "World" }
          { Period = Period.from (jan15 21) (TimeSpan.forNDays 5); Value = "Hello" }
          { Period = Period.from (jan15 16) (TimeSpan.forNDays 5); Value = "Hello" }
          { Period = Period.from (jan15 26) (TimeSpan.forNDays 5); Value = "Hello" } ] 
        |> Temporal.toTemporal |> Temporal.merge

    let expected = 
        [ { Period = Period.from (jan15 01) (TimeSpan.forNDays 5); Value = "Hello" }
          { Period = Period.from (jan15 06) (TimeSpan.forNDays 5); Value = "World" }
          { Period = Period.from (jan15 11) (TimeSpan.forNDays 20); Value = "Hello" } ]
        |> Temporal.toTemporal

    actual |> should equal expected

    let actual = 
        [ { Period = { StartDate = jan15 01; EndDate = jan15 30 }; Value = "Hello"}
          { Period = { StartDate = jan15 05; EndDate = jan15 25 }; Value = "Hello"} ]
        |> Temporal.toTemporal 
        |> Temporal.merge

    let expected = 
        [ { Period = { StartDate = jan15 01; EndDate = jan15 30 }; Value = "Hello"} ]
        |> Temporal.toTemporal

    actual |> should equal expected

[<Fact>]
let ``should list temporaries for a given period``()=
    let temporal = 
        [ { Period = Period.from (jan15 1) (TimeSpan.forNDays 10); Value = "Hello" }
          { Period = Period.from (jan15 11) (TimeSpan.forNDays 10); Value = "Toto" } ]
        |> Temporal.toTemporal
    
    temporal |> Temporal.view Period.Always |> should equal temporal
    temporal |> Temporal.view Period.Never = ([] |> Temporal.toTemporal) |> should equal true

    temporal 
    |> Temporal.view (Period.from (jan15 5) (TimeSpan.forNDays 10)) 
    |> should equal
        ([ { Period = { StartDate = (jan15 05); EndDate = (jan15 11)}; Value = "Hello" }
           { Period = { StartDate = (jan15 11); EndDate = (jan15 15)}; Value = "Toto" } ] |> Temporal.toTemporal)