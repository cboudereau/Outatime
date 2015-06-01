module PeriodProperties

open FsCheck
open FsCheck.Xunit

open Temporality

[<Arbitrary(typeof<TestData.TwoRandomPeriods>)>]
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
    let ``intersection between always is always`` () =
        Period.Always |> Period.intersect Period.Always = Some(Period.Always)

    [<Property>]
    let ``always, intersect with any period`` (p1,p2) = 
        Period.Always |> Period.intersect p1 = Some(p1)
        && Period.Always |> Period.intersect p2 = Some(p2)

    [<Property>]
    let ``Never never union with period`` (p1,p2) = 
        match (Period.intersect Period.Never p1), (Period.intersect p2 Period.Never), Period.intersect Period.Never Period.Never with
        | None, None, None -> true
        | _ -> false

[<Arbitrary(typeof<TestData.TwoRandomPeriods>)>]
module UnionProperties = 
    
    [<Property>]
    let ``union between never is never``() =
        Period.Never |> Period.union Period.Never = Some(Period.Never)

    [<Property>]
    let ``union between always is always``()=
        Period.Always |> Period.union Period.Always = Some(Period.Always)

    [<Property>]
    let ``union with always and any period is always`` (p1,p2) = 
        Period.Always |> Period.union p1 = Some(Period.Always)
        && Period.Always |> Period.union p2 = Some(Period.Always)

    [<Property>]
    let ``union with never and any period is none`` (p1, p2) = 
        Period.Never |> Period.union p1 = None
        && Period.Never |> Period.union p2 = None

//    [<Property>]
//    let ``when p1 is the largest period, p1 is the union``() p1 p2 =
//        let max = 
//            let minStart = min p1.StartDate p2.StartDate
//            let maxEnd = max p1.EndDate p2.EndDate
//            { StartDate = minStart; EndDate = maxEnd }
//
//        match p1 |> Period.union max, p2 |> Period.union max with
//        | Some(r1), Some(r2) when 