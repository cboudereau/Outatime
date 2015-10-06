module Bdd
open Temporality
open FsUnit.Xunit

let Given v = v
let When f v = v |> f 

let Then check expected = 
    check expected

let shouldEqual expected actual = actual |> should equal expected

let shouldBeEmpty actual = actual |> Seq.length |> should equal 0