module TemporalTest

open Temporality
open Xunit
open FsCheck
open FsUnit.Xunit

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

[<Fact>]
let ``when value are equal on intersect periods should merge``() =
    [] |> Temporal.toTemporal |> Temporal.merge |> should equal ([] |> Temporal.toTemporal)
    
    let actual = 
        [ { Period = Period.from (jan15 1) (TimeSpan.forNDays 5); Value = "Hello" }
          { Period = Period.from (jan15 11) (TimeSpan.forNDays 5); Value = "Hello" }
          { Period = Period.from (jan15 06) (TimeSpan.forNDays 5); Value = "World" }
          { Period = Period.from (jan15 21) (TimeSpan.forNDays 5); Value = "Hello" }
          { Period = Period.from (jan15 16) (TimeSpan.forNDays 5); Value = "Hello" }
          { Period = Period.from (jan15 26) (TimeSpan.forNDays 5); Value = "Hello" } ] 
        |> Temporal.toTemporal |> Temporal.merge

    let expected = 
        [ { Period = Period.from (jan15 01) (TimeSpan.forNDays 5); Value = "Hello" }
          { Period = Period.from (jan15 06) (TimeSpan.forNDays 5); Value = "World" }
          { Period = Period.from (jan15 11) (TimeSpan.forNDays 20); Value = "Hello" } ]
        |> Temporal.toTemporal

    actual |> should equal expected

    let actual = 
        [ { Period = { StartDate = jan15 01; EndDate = jan15 30 }; Value = "Hello"}
          { Period = { StartDate = jan15 05; EndDate = jan15 25 }; Value = "Hello"} ]
        |> Temporal.toTemporal 
        |> Temporal.merge

    let expected = 
        [ { Period = { StartDate = jan15 01; EndDate = jan15 30 }; Value = "Hello"} ]
        |> Temporal.toTemporal

    actual |> should equal expected