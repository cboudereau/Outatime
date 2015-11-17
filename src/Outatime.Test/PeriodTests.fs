module PeriodTests

open Bdd
open Xunit
open Outatime

let jan15 n = DateTime(2015, 1, n)

[<Fact>]
let ``TimeSpan composition test``()=
    When TimeSpan.forEver + TimeSpan.forNever |> Expect TimeSpan.forEver
    When TimeSpan.forEver - TimeSpan.forNever |> Expect TimeSpan.forEver
    When TimeSpan.forNever - TimeSpan.forNever |> Expect TimeSpan.forNever
    When TimeSpan.forNever + TimeSpan.forNever |> Expect TimeSpan.forNever

    When DateTime.MaxValue - DateTime.MinValue |> Expect TimeSpan.forEver

[<Fact>]
let ``timespan facilities tests``()=
    When TimeSpan.forNever
    |> Expect TimeSpan.Zero

    When TimeSpan.forOneDay
    |> Expect (TimeSpan.FromDays(1.))

    When TimeSpan.forNDays 5
    |> Expect (TimeSpan.FromDays(5.))

[<Fact>]
let ``when display period expect math representation``()=
    let period = { StartDate =  jan15 1; EndDate=jan15 2 }

    let ``display a period`` period = period.ToString()

    When ``display a period``
    |> With period
    |> Expect "[2015/01/01; 2015/01/02["

[<Fact>]
let ``infinite period must be between DateTime.MinValue and DateTime.MaxValue``()=
    
    When infinite
    |> Expect { StartDate=DateTime.MinValue; EndDate=DateTime.MaxValue }

[<Fact>]
let ``given 2 unsorted periods expect sorted periods``()=
    let p1 = jan15 20 => jan15 21
    let p2 = jan15 1 => jan15 2

    let its p1 p2 = (p1, p2)

    let ``I want to sort periods`` = sortP its

    When ``I want to sort periods``
    |> With p1
    |> And p2
    |> Expect (p2,p1)

    When ``I want to sort periods``
    |> With p2
    |> And p1
    |> Expect (p2,p1)
