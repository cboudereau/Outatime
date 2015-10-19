module SortTests

open Bdd
open Xunit
open Outatime

let jan15 n = DateTime(2015, 1, n)

[<Fact>]
let ``temporary display period and value``()=
    let ``I want to display temporary`` temporary = temporary.ToString()

    When ``I want to display temporary``
    |> With (jan15 1 => jan15 2 := "Hello")
    |> Expect "[2015/01/01; 2015/01/02[ = Hello"

[<Fact>]
let ``when sort temporaries expect temporaries sorted by start date``()=
    let ``I want to sort temporaries`` = Outatime.sort >> Seq.toList
    
    When ``I want to sort temporaries``
    |> With
        [ jan15 10 => jan15 20 := "Hello"
          jan15 1 => jan15 2 := "Hello" ] 
    |> Expect
        [ jan15 1 => jan15 2 := "Hello"
          jan15 10 => jan15 20 := "Hello" ] 

    When ``I want to sort temporaries``
    |> With 
        [ jan15 1 => jan15 2 := "Hello"
          jan15 10 => jan15 20 := "Hello" ] 
    |> Expect
        [ jan15 1 => jan15 2 := "Hello"
          jan15 10 => jan15 20 := "Hello" ] 

    When ``I want to sort temporaries``
    |> With []
    |> Expect []

