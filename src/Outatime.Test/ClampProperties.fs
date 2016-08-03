module TemporalViewProperties

open FsCheck.Xunit
open Outatime

[<Arbitrary(typeof<TestData.RandomStringTemporal>)>]
module ClampProperties = 
    [<Property>]
    let ``StartDate of view should be always greater or equal the given viewed startDate period`` period (temporaries : IntervalValued<DateTime, string> list) = 
        let windowedTemporal = temporaries |> Outatime.build |> Outatime.clamp period |> Outatime.toList
        windowedTemporal |> Seq.forall (fun t -> t.Interval.Start >= period.Start)
    
    [<Property>]
    let ``EndDate of view should be always less or equal to the given viewed end date period`` () period (temporaries : IntervalValued<DateTime, string> list) = 
        let windowedTemporal = temporaries |> Outatime.build |> Outatime.clamp period |> Outatime.toList
        windowedTemporal |> Seq.forall (fun t -> t.Interval.End <= period.End)
