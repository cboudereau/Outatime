module Bdd
open FsUnit.Xunit

let When f = f
let With v f = f v
let For = With
let And = With
let Then check expected = check expected

let shouldEqual<'a> (expected:'a) (actual:'a) = actual |> should equal expected

let Expect<'a> = Then shouldEqual<'a>

let shouldBeEmpty actual = actual |> Seq.length |> should equal 0
