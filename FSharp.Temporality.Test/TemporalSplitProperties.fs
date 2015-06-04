module TemporalSplitProperties

open FsCheck
open FsCheck.Xunit

open Temporality
    
[<Arbitrary(typeof<TestData.RandomTemporal>)>]
module SplitTemporaries = 

    let splitPeriod = TimeSpan.forNDays 5

    [<Property>]
    let ``check that all period are less than split period`` (temporal:Temporal<string>) = 
        (temporal
        |> Temporal.split splitPeriod).Values
        |> Seq.forall(fun v -> v.Period.Duration <= splitPeriod)
