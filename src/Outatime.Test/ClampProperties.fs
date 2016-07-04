module TemporalViewProperties

open FsCheck.Xunit
open Outatime

[<Arbitrary(typeof<TestData.RandomStringTemporal>)>]
module ClampProperties = 
    [<Property>]
    let ``StartDate of view should be always greater or equal the given viewed startDate period`` period (temporaries : string Temporary list) = 
        let windowedTemporal = temporaries |> Outatime.build |> Outatime.clamp period |> Outatime.toList
        windowedTemporal |> Seq.forall (fun t -> t.Period.StartDate >= period.StartDate)
    
    [<Property>]
    let ``EndDate of view should be always less or equal to the given viewed end date period`` () period (temporaries : string Temporary list) = 
        let windowedTemporal = temporaries |> Outatime.build |> Outatime.clamp period |> Outatime.toList
        windowedTemporal |> Seq.forall (fun t -> t.Period.EndDate <= period.EndDate)
