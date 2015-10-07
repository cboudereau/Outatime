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