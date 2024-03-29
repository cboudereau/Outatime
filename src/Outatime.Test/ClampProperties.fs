﻿module TemporalViewProperties

open FsCheck.Xunit
open Outatime

module ClampProperties = 
    [<Property(Arbitrary=[| typeof<TestData.RandomStringTemporal> |])>]
    let ``StartDate of view should be always greater or equal the given viewed startDate period`` period (temporaries : Temporary<string> list) = 
        let windowedTemporal = temporaries |> Outatime.build |> Outatime.clamp period |> Outatime.toList
        windowedTemporal |> Seq.forall (fun t -> t.Interval.Start >= period.Start)
    
    [<Property(Arbitrary=[| typeof<TestData.RandomStringTemporal> |])>]
    let ``EndDate of view should be always less or equal to the given viewed end date period`` () period (temporaries : Temporary<string> list) = 
        let windowedTemporal = temporaries |> Outatime.build |> Outatime.clamp period |> Outatime.toList
        windowedTemporal |> Seq.forall (fun t -> t.Interval.End <= period.End)
