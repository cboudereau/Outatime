module IntersectProperties

open FsCheck
open FsCheck.Xunit

open Temporality

[<Arbitrary(typeof<TestData.TwoPeriodsArb>)>]
module IntersectProperties =
    let getLargestPeriod p1 p2 = 
        let minStartDate = min p1.StartDate p2.StartDate
        let maxEndDate = max p1.EndDate p2.EndDate
        { StartDate = minStartDate
          EndDate = maxEndDate }

    [<Property>]
    let ``largest period intersect with all period`` (p1, p2) = 
        let largestPeriod = getLargestPeriod p1 p2
        match (Period.intersect largestPeriod p1), (Period.intersect largestPeriod p2) with
        | Some a1, Some a2 -> a1 = p1 && a2 = p2
        | _, _ when p2.StartDate > p1.EndDate -> true
        | _, _ -> false

    [<Property>]
    let ``Never never union with period`` (p1,p2) = 
        match (Period.intersect Period.Never p1), (Period.intersect p2 Period.Never), Period.intersect Period.Never Period.Never with
        | None, None, None -> true
        | _ -> false

[<Arbitrary(typeof<TestData.ValidRepresentableTemporal>)>]
module SameValue =

    [<Property>]
    let ``temporal with same Hello value contains one temporary`` (temporal:Temporal<string>) =
        let mergedTemporal = temporal |> Temporal.merge

        if(temporal.Values |> Seq.length = 0) 
        then mergedTemporal.Values |> Seq.length = 0
        else mergedTemporal.Values |> Seq.length = 1