module TemporalViewProperties

open FsCheck.Xunit
open Temporality

[<Arbitrary(typeof<TestData.RandomStringTemporal>)>]
module ViewProperties = 
    [<Property>]
    let ``StartDate of view should be always greater or equal the given viewed startDate period`` period (temporaries : string Temporary list) = 
        let windowedTemporal = temporaries |> Temporality.clamp period
        windowedTemporal |> Seq.forall (fun t -> t.period.startDate >= period.startDate)
    
    [<Property>]
    let ``EndDate of view should be always less or equal to the given viewed end date period`` () period (temporaries : string Temporary list) = 
        let windowedTemporal = temporaries |> Temporality.clamp period
        windowedTemporal |> Seq.forall (fun t -> t.period.endDate <= period.endDate)
