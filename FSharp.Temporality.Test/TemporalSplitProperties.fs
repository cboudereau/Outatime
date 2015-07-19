module TemporalSplitProperties

open FsCheck
open FsCheck.Xunit

open FsUnit.Xunit
open Temporality

let Given v = v
let When f v = 
    v 
    |> Temporal.toTemporal 
    |> f 
    |> Temporal.temporaries

let Then check expected actual = 
    check (expected |> Temporal.toTemporal |> Temporal.temporaries) actual

let shouldEqual = should equal

let jan15 n = (DateTime(2015,1,n))

[<Xunit.Fact>]
let ``simple split test``()=
    Given 
        [ Period.from (jan15 01) (jan15 11) |> Temporary.create "HelloWorld" ]
    |> When (TimeSpan.FromDays(5.) |> Temporal.split)
    |> Then shouldEqual
        [ Period.from (jan15 01) (jan15 06) |> Temporary.create "HelloWorld"
          Period.from (jan15 06) (jan15 11) |> Temporary.create "HelloWorld" ]
    
[<Arbitrary(typeof<TestData.RandomStringTemporal>)>]
module SplitTemporaries = 

    let splitPeriod = System.TimeSpan.FromDays(1000.)

    [<Property>]
    let ``check that all period are less than split period`` (temporal:Temporal<string>) = 
        (temporal
        |> Temporal.split splitPeriod).Values
        |> Seq.forall(fun v -> v.Period.Duration <= splitPeriod)
