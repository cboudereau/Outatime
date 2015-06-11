module TemporalSplitProperties

open FsCheck
open FsCheck.Xunit

open Temporality
    
[<Arbitrary(typeof<TestData.RandomStringTemporal>)>]
module SplitTemporaries = 

    let splitPeriod = System.TimeSpan.FromDays(1000.)

    [<Property>]
    let ``check that all period are less than split period`` (temporal:Temporal<string>) = 
        (temporal
        |> Temporal.split splitPeriod).Values
        |> Seq.forall(fun v -> v.Period.Duration <= splitPeriod)
