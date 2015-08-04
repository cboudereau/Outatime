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
        [ jan15 01 ==> jan15 11 := "HelloWorld" ]
    |> When (TimeSpan.FromDays(5.) |> Temporal.split)
    |> Then shouldEqual
        [ jan15 01 ==> jan15 06 := "HelloWorld"
          jan15 06 ==> jan15 11 := "HelloWorld" ]
    
[<Arbitrary(typeof<TestData.RandomStringTemporal>)>]
module SplitTemporaries = 

    let splitPeriod = System.TimeSpan.FromDays(1000.)

    [<Property>]
    let ``check that all period are less than split period`` (temporal:Temporal<string>) = 
        (temporal
        |> Temporal.split splitPeriod).Values
        |> Seq.forall(fun v -> v.Period.Duration <= splitPeriod)
