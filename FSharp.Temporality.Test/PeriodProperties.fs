module PeriodProperties

open FsCheck
open FsCheck.Xunit

open Temporality

let getLargestPeriod p1 p2 = 
    let minStartDate = min p1.StartDate p2.StartDate
    let maxEndDate = max p1.EndDate p2.EndDate
    { StartDate = minStartDate
      EndDate = maxEndDate }

[<Arbitrary(typeof<TestData.TwoRandomPeriods>)>]
module IntersectProperties =

    [<Property>]
    let ``max(p1,p2) ∩ p1 = p1, max (p1,p2) ∩ p2 = p2`` (p1, p2) = 
        let largestPeriod = getLargestPeriod p1 p2
        match (Period.intersect largestPeriod p1), (Period.intersect largestPeriod p2) with
        | Some a1, Some a2 -> a1 = p1 && a2 = p2
        | _ -> false
    
    [<Property>]
    let ``Always ∩ Always = Always`` () =
        Period.Always |> Period.intersect Period.Always = Some(Period.Always)

    [<Property>]
    let ``Always ∩ p = p`` (p1, p2) = 
        Period.Always |> Period.intersect p1 = Some(p1)
        && Period.Always |> Period.intersect p2 = Some(p2)

    [<Property>]
    let ``Never ∩ p = None`` (p1, p2) = 
        match (Period.intersect Period.Never p1), (Period.intersect p2 Period.Never), Period.intersect Period.Never Period.Never with
        | None, None, None -> true
        | _ -> false

    [<Property>]
    let ``p ∩ p = p`` (p1, p2) = 
        p1 |> Period.intersect p1 = Some(p1)
        && p2 |> Period.intersect p2 = Some(p2) 

[<Arbitrary(typeof<TestData.TwoRandomPeriods>)>]
module UnionProperties = 
    
    [<Property>]
    let ``Never ∪ Never = Never``() =
        Period.Never |> Period.union Period.Never = Some(Period.Never)

    [<Property>]
    let ``Always ∪ Always = Always``()=
        Period.Always |> Period.union Period.Always = Some(Period.Always)

    [<Property>]
    let ``Always ∪ P = Always`` (p1,p2) = 
        Period.Always |> Period.union p1 = Some(Period.Always)
        && Period.Always |> Period.union p2 = Some(Period.Always)

    [<Property>]
    let ``Never ∪ p = None`` (p1, p2) = 
        Period.Never |> Period.union p1 = None
        && Period.Never |> Period.union p2 = None

    [<Property>]
    let ``p ∪ p = p`` (p1, p2) = 
        p1 |> Period.union p1 = Some(p1)
        && p2 |> Period.union p2 = Some(p2)