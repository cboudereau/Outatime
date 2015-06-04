module TemporalSplitProperties

open FsCheck
open FsCheck.Xunit

open Temporality
    
[<Arbitrary(typeof<TestData.RandomTemporal>)>]
module SplitTemporaries = 

    [<Property>]
    let ``check that all period are less than split period`` (temporal:Temporal<string>) t = 
        (temporal
        |> Temporal.split t).Values
        |> Seq.forall(fun v -> v.Period.Duration <= t)
