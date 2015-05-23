module TemporalTest

open Temporality
open Xunit
open FsCheck
open FsUnit.Xunit

let jan15 day = DateTime(2015, 01, day)

let neutral = TimeSpan.FromDays(0.)
let toPositiveDuration (timeSpan : TimeSpan) = 
    if timeSpan < neutral then timeSpan.Negate()
    else timeSpan

let getPeriod d1 d2 = 
    let minDate = min d1 d2
    let maxDate = max d1 d2
    { StartDate = minDate
      Duration = maxDate - minDate }

[<Fact>]
let ``period intesection test``()=
    

    let arb = 
        Arb.generate<DateTime>
        |> Gen.four
        |> Gen.map(fun (d1, d2, d3, d4) -> ( getPeriod d1 d2, getPeriod d3 d4 ))
        |> Arb.fromGen
    
    let ``check that the largest period of two periods intersect with them`` (p1, p2) =
        
        let largestPeriod = 
            let minStartDate = min p1.StartDate p2.StartDate
            let maxEndDate = max p1.EndDate p2.EndDate
            { StartDate = minStartDate
              Duration = maxEndDate - minStartDate }
        
        let ``largest period intersect with all period`` = 
            match (Period.Intersect largestPeriod p1), (Period.Intersect largestPeriod p2) with
            | Some a1, Some a2 -> a1 = p1 && a2 = p2
            | _, _ when p2.StartDate > p1.EndDate -> true
            | _, _ -> false

        let ``Never never intesect with period`` = 
            match (Period.Intersect Never p1), (Period.Intersect p2 Never) with
            | None, None -> true
            | _ -> false

        ``largest period intersect with all period``
        && ``Never never intesect with period``

    Check.QuickThrowOnFailure(Prop.forAll arb <| ``check that the largest period of two periods intersect with them``) 

[<Fact>]
let ``split empty period is empty period``() = 
    let arb = 
        Arb.generate<DateTime>
        |> Gen.map (fun d -> 
               [ { Period = 
                       { StartDate = d
                         Duration = TimeSpan.MinValue }
                   Value = "pouet" } ]
               |> Temporality.toTemporal)
        |> Arb.fromGen
    
    let ``check that startdate is equal to enddate`` t = 
        t |> Temporality.split (forNDays 5) = t
    
    Check.QuickThrowOnFailure(Prop.forAll arb <| ``check that startdate is equal to enddate``)

[<Fact>]
let ``split period by n days``()=
    let toTestTemporary (n, date) = 
        { Period = { StartDate=date; Duration = forNDays n }
          Value = "hello" }
    
    let toTestTemporaries u = u |> Seq.map(toTestTemporary) |> Temporality.toTemporal

    let arb = 
        Arb.generate<int*DateTime>
        |> Gen.listOf
        |> Gen.map(toTestTemporaries)
        |> Arb.fromGen
    
    let splitPeriod = forNDays 5

    let ``check that period are sorted by start date in order to have correct interval`` temporal = 
        let actual = temporal |> split splitPeriod
        (actual.Values |> Seq.sortBy(fun t -> t.Period.StartDate) |> Seq.toList) = (actual.Values |> Seq.toList)

    let ``check that all period are less than split period`` temporal = 
        let actual = temporal |> Temporality.split splitPeriod
        actual.Values
        |> Seq.map(fun v -> v.Period.Duration <= splitPeriod)
        |> Seq.forall(fun b -> b)

    Check.QuickThrowOnFailure(Prop.forAll arb <| ``check that all period are less than split period``)
    Check.QuickThrowOnFailure(Prop.forAll arb <| ``check that period are sorted by start date in order to have correct interval``)

[<Fact>]
let ``contigous period with same value should be merged``()=
    let arb = 
        Arb.generate<DateTime>
        |> Gen.four
        |> Gen.map(
            fun (d1, d2, d3, d4) -> 
                ( getPeriod d1 d2, getPeriod d3 d4 ))
        |> Arb.fromGen

    let ``check that largest period of two with same value are all merged`` (p1,p2) =
        let sameValue = "Hello"
        let getTemporary p v = { Period = p; Value = v }
        let maxPeriod = 
            let startDate = min p1.StartDate p2.StartDate 
            let endDate = max p1.EndDate p2.EndDate
            { StartDate = startDate
              Duration = endDate - startDate }
        let tMax = getTemporary maxPeriod sameValue
        let t1 = getTemporary p1 sameValue
        let t2 = getTemporary p2 "World"

        let merged = [tMax; t1; t2] |> Temporality.toTemporal |> Temporality.merge
        
        if(t2.Period.StartDate < t1.Period.StartDate) 
        then merged = ([tMax; t1; t2] |> Temporality.toTemporal)
        else merged = ([tMax; t2] |> Temporality.toTemporal)
        
    Check.QuickThrowOnFailure(Prop.forAll arb <| ``check that largest period of two with same value are all merged``)

[<Fact>]
let ``merge test``() =
    [] |> toTemporal |> merge |> should equal ([] |> toTemporal)
    
    let actual = 
        [ { Period = { StartDate = (DateTime(2015,01,01)); Duration = forNDays 5 }; Value = "Hello" }
          { Period = { StartDate = (DateTime(2015,01,11)); Duration = forNDays 5 }; Value = "Hello" }
          { Period = { StartDate = (DateTime(2015,01,06)); Duration = forNDays 5 }; Value = "World" }
          { Period = { StartDate = (DateTime(2015,01,21)); Duration = forNDays 5 }; Value = "Hello" }
          { Period = { StartDate = (DateTime(2015,01,16)); Duration = forNDays 5 }; Value = "Hello" }
          { Period = { StartDate = (DateTime(2015,01,26)); Duration = forNDays 5 }; Value = "Hello" } ] 
        |> toTemporal |> merge

    let expected = 
        [ { Period = { StartDate = (DateTime(2015,01,01)); Duration = forNDays 5 }; Value = "Hello" }
          { Period = { StartDate = (DateTime(2015,01,06)); Duration = forNDays 5 }; Value = "World" }
          { Period = { StartDate = (DateTime(2015,01,11)); Duration = forNDays 20 }; Value = "Hello" } ]
        |> toTemporal

    actual |> should equal expected