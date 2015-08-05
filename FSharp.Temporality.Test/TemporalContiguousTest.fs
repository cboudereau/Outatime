module TemporalContiguousTest

open Xunit
open Temporality
open Bdd

let jan15 day = (System.DateTime(2015,01,day))

[<Fact>]
let ``When there is empty period, when contiguous temporal, expect an option type with None value on empty period``()= 
    let ``I want to have contiguous periods`` = Temporal.toContiguous
    
    Given 
        [ jan15 01 ==> jan15 03 := "Hello"
          jan15 10 ==> jan15 13 := "Toto" ]
    |> When ``I want to have contiguous periods``
    |> Then shouldEqual 
        [ jan15 01 ==> jan15 3 := "Hello" |> Some
          jan15 03 ==> jan15 10 := None
          jan15 10 ==> jan15 13 := "Toto" |> Some ]

[<Fact>]
let ``When temporaries are empty expect an empty temporal``() =
    Given Seq.empty 
    |> When Temporal.toContiguous 
    |> Then shouldBeEmpty