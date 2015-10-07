module Bdd
open Temporality
open FsUnit.Xunit

//let Given v = v
//let When f v = v |> f
//
//let Then check expected = 
//    check expected
//
//let shouldEqual expected actual = actual |> should equal expected
//
//let shouldBeEmpty actual = actual |> Seq.length |> should equal 0

let When f = f
let With v f = f v
let For = With
let And = With
let Then check expected = check expected

let shouldEqual<'a> (expected:'a) (actual:'a) = actual |> should equal expected

let Expect<'a> = Then shouldEqual<'a>

let shouldBeEmpty actual = actual |> Seq.length |> should equal 0
