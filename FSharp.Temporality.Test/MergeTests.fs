module MergeTests

open Temporality
open Bdd

let jan15 n = (DateTime(2015,1,n))

[<Xunit.Fact>]
let ``simple merge test``()=
    Given 
        [ jan15 01 => jan15 02 := "Hello"
          jan15 02 => jan15 05 := "Hello"
          jan15 05 => jan15 10 := "World"
          jan15 10 => jan15 20 := "World" ]
    |> When Temporality.merge
    |> Then shouldEqual
        [ jan15 01 => jan15 05 := "Hello"
          jan15 05 => jan15 20 := "World" ]