module TemporalZipTest

open Xunit
open FsUnit.Xunit
open Temporality

let jan15 day = DateTime(2015, 01, day)

[<Fact>]
let ``poc zip temporal``()= 
    let temporal1 = 
        [ { Period = { StartDate = jan15 1; EndDate = jan15 2 }; Value = "hello" }
          { Period = { StartDate = jan15 2; EndDate = jan15 5 }; Value = "world" } ]
        |> Temporal.toTemporal

    let temporal2 = 
        [ { Period = { StartDate = jan15 1; EndDate = jan15 3 }; Value = true }
          { Period = { StartDate = jan15 3; EndDate = jan15 5 }; Value = false } ]
        |> Temporal.toTemporal

    let expected = 
        [ { Period = { StartDate = jan15 1; EndDate = jan15 2 }; Value = ("hello", true) }
          { Period = { StartDate = jan15 2; EndDate = jan15 3 }; Value = ("world", true) }
          { Period = { StartDate = jan15 3; EndDate = jan15 5 }; Value = ("world", false) } ]
        |> Temporal.toTemporal

    (temporal1,temporal2) 
    |> Temporal.zip
    |> should equal expected