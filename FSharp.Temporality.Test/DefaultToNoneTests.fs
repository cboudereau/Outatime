module DefaultToNoneTests

open Temporality
open Bdd
open Xunit

let begin' = DateTime.MinValue
let end' = DateTime.MaxValue
let jan15 n = (DateTime(2015,1,n))
let ``I want to default missings with None`` = Temporality.defaultToNone >> Seq.toList

let ``I want option contiguous temporary`` temporaries = Temporality.contiguous temporaries |> Seq.toList

[<Fact>]
let ``given temporaries expect a contiguous temporaries``()=
    When ``I want option contiguous temporary``
    |> With 
        [ jan15 01 => jan15 02 := "Hello"
          jan15 03 => jan15 05 := "Hello"
          jan15 05 => jan15 10 := "World"
          jan15 10 => jan15 20 := "World" ]
    |> Expect
        [ jan15 01 => jan15 02 := Some "Hello"
          jan15 02 => jan15 03 := None
          jan15 03 => jan15 05 := Some "Hello"
          jan15 05 => jan15 10 := Some "World"
          jan15 10 => jan15 20 := Some "World" ]

[<Fact>]
let ``given contiguous temporary expect a merged temporary``()=
    When ``I want to default missings with None``
    |> With 
        [ jan15 01 => jan15 02 := "Hello"
          jan15 02 => jan15 05 := "Hello"
          jan15 05 => jan15 10 := "World"
          jan15 10 => jan15 20 := "World" ]
    |> Expect
        [ begin' => jan15 01 := None
          jan15 01 => jan15 02 := Some "Hello"
          jan15 02 => jan15 05 := Some "Hello"
          jan15 05 => jan15 10 := Some "World"
          jan15 10 => jan15 20 := Some "World"
          jan15 20 => end' := None ]

[<Fact>]
let ``given full period with value when default to none expect same period with some value``() =
    When ``I want to default missings with None``
    |> With 
        [ begin' => end' := "Hello" ]
    |> Expect
        [ begin' => end' := Some "Hello" ]

[<Fact>]
let ``given overlaped temporaries when default to none expect option overlaped temporary``()=   
    When ``I want to default missings with None``
    |> With 
        [ jan15 01 => jan15 02 := "Hello"
          jan15 01 => jan15 05 := "Hello"
          jan15 05 => jan15 10 := "World"
          jan15 10 => jan15 20 := "World" ]
    |> Expect
        [ begin' => jan15 01 := None
          jan15 01 => jan15 02 := Some "Hello"
          jan15 01 => jan15 05 := Some "Hello"
          jan15 05 => jan15 10 := Some "World"
          jan15 10 => jan15 20 := Some "World"
          jan15 20 => end' := None ]
