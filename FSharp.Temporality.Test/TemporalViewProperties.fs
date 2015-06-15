module TemporalViewProperties

open FsCheck.Xunit
open Temporality

[<Arbitrary(typeof<TestData.RandomStringTemporal>)>]
module ViewProperties = 
    [<Property>]
    let ``StartDate of view should be always greater or equal the given viewed startDate period`` period (temporal : Temporal<string>) = 
        let getPeriod _ = period
        let windowedTemporal = temporal |> Temporal.view getPeriod
        windowedTemporal.Values |> Seq.forall (fun t -> t.Period.StartDate >= period.StartDate)
    
    [<Property>]
    let ``EndDate of view should be always less or equal to the given viewed end date period`` () period (temporal : Temporal<string>) = 
        let windowedTemporal = temporal |> Temporal.clamp period
        windowedTemporal.Values |> Seq.forall (fun t -> t.Period.EndDate <= period.EndDate)
