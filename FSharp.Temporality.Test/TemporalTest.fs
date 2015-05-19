module TemporalTest

open Temporality
open Xunit
open FsCheck

let jan15 day = DateTime(2015, 01, day)

[<Fact>]
let ``split empty period is empty period``() = 
    let arb = 
        Arb.generate<DateTime>
        |> Gen.map (fun d -> 
               [ { Period = 
                       { StartDate = d
                         EndDate = d }
                   Value = "pouet" } ]
               |> Temporality.toTemporal)
        |> Arb.fromGen
    
    let ``check that startdate is equal to enddate`` t = 
        t |> Temporality.split (forNDays 5) = t
    
    Check.QuickThrowOnFailure(Prop.forAll arb <| ``check that startdate is equal to enddate``)

[<Fact>]
let ``split period by n days``()=
    let toTestTemporary (n, date) = 
        { Period = { StartDate=date; EndDate = date + (forNDays n) }
          Value = "hello" }
    
    let toTestTemporaries u = u |> Seq.map(toTestTemporary) |> Temporality.toTemporal

    let arb = 
        Arb.generate<int*DateTime>
        |> Gen.listOf
        |> Gen.map(toTestTemporaries)
        |> Arb.fromGen
    
    let splitPeriod = forNDays 5

    let ``check that all period are less than split period`` temporal = 
        let actual = temporal |> Temporality.split splitPeriod
        actual.Values
        |> Seq.map(fun v -> v.Period.DayLength <= splitPeriod)
        |> Seq.forall(fun b -> b)

    Check.QuickThrowOnFailure(Prop.forAll arb <| ``check that all period are less than split period``)
