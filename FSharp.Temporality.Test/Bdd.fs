module Bdd
open Xunit

let When f = f
let With v f = f v
let For = With
let And = With
let Then check expected = check expected

let shouldEqual<'a> (expected:'a) (actual:'a) = Assert.Equal(expected, actual)

let Expect<'a> = Then shouldEqual<'a>

let shouldBeEmpty actual = Assert.Equal(0, actual |> Seq.length)