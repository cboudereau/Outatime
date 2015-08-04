module Bdd
open Temporality
open FsUnit.Xunit

let Given v = v
let When f v = 
    v 
    |> Temporal.toTemporal 
    |> f 
    |> Temporal.temporaries

let Then check expected = 
    check expected

let shouldEqual expected actual = actual |> should equal (expected |> Temporal.toTemporal |> Temporal.temporaries)

let shouldBeEmpty actual = actual |> Seq.length |> should equal 0