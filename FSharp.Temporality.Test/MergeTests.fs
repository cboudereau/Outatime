module MergeTests

open Temporality
open Bdd
open Xunit

let jan15 n = (DateTime(2015,1,n))

[<Fact>]
let ``given contiguous temporary expect a merged temporary``()=
    Given 
        [ jan15 01 => jan15 02 := "Hello"
          jan15 02 => jan15 05 := "Hello"
          jan15 05 => jan15 10 := "World"
          jan15 10 => jan15 20 := "World" ]
    |> When Temporality.merge
    |> Then shouldEqual
        [ jan15 01 => jan15 05 := "Hello"
          jan15 05 => jan15 20 := "World" ]

[<Fact>]
let ``given an empty temporaries expect an empty temporaries``()=
    Given []
    |> When Temporality.merge
    |> Then shouldBeEmpty

[<Fact>]
let ``given unmergeable temporaries expect same temporaries``()=
    let temporaries = 
        [ jan15 01 => jan15 02 := "Hello"
          jan15 03 => jan15 05 := "Hello"
          jan15 06 => jan15 10 := "World"
          jan15 11 => jan15 20 := "World" ]

    Given temporaries
    |> When Temporality.merge
    |> Then shouldEqual temporaries
        
[<Fact>]
let ``given temporaries with overlap on startDate expect a merged temporary``()=
    Given
        [ jan15 01 => jan15 02 := "Hello"
          jan15 01 => jan15 05 := "Hello"
          jan15 06 => jan15 10 := "World" ]
    |> When Temporality.merge
    |> Then shouldEqual
        [ jan15 01 => jan15 05 := "Hello"
          jan15 06 => jan15 10 := "World" ]
        
[<Fact>]
let ``given temporaries with overlap on endDate expect a merged temporary``()=
    Given
        [ jan15 01 => jan15 03 := "Hello"
          jan15 02 => jan15 05 := "Hello"
          jan15 06 => jan15 10 := "World" ]
    |> When Temporality.merge
    |> Then shouldEqual
        [ jan15 01 => jan15 05 := "Hello"
          jan15 06 => jan15 10 := "World" ]
        
