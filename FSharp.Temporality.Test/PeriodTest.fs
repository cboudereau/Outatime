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
    let p = { StartDate = jan15 1; EndDate = jan15 2 }
    
    p.ToString() |> should equal "[1/1/2015 0:00:00, 2/1/2015 0:00:00)" 

    Period.Always.ToString() |> should equal "Always"
    Period.Never.ToString() |> should equal "Never"

[<Fact>]
let ``Temporary should be displayed same as Period``()=
    let t = { Period = Period.Always; Value = "Hello" }

    t.ToString() |> should equal @"Always : ""Hello"""

