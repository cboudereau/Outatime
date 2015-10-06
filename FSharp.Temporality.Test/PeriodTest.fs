module PeriodTest

open Temporality
open FsUnit
open Xunit

let jan15 day = DateTime(2015, 01, day)

[<Fact>]
let ``TimeSpan composition test``()=
    TimeSpan.forEver + TimeSpan.forNever |> should equal TimeSpan.forEver
    TimeSpan.forEver - TimeSpan.forNever |> should equal TimeSpan.forEver
    TimeSpan.forNever - TimeSpan.forNever |> should equal TimeSpan.forNever
    TimeSpan.forNever + TimeSpan.forNever |> should equal TimeSpan.forNever

    DateTime.MaxValue - DateTime.MinValue |> should equal TimeSpan.forEver

[<Fact>]
let ``Period should be display with math interval notation for half open interval``()=
    let p = jan15 1 => jan15 2
    
    p.ToString() |> should equal "[01/01/2015 00:00:00, 01/02/2015 00:00:00)" 

    let emptyPeriod = jan15 1 => jan15 1
    emptyPeriod.ToString() |> should equal "Empty 01/01/2015 00:00:00"

[<Fact>]
let ``Temporary should be displayed same as Period``()=
    let t = Period.infinite := "Hello"
    t.ToString() |> should equal @"Infinite : ""Hello"""