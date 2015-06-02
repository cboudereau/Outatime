module PeriodProperties

open FsCheck
open FsCheck.Xunit

open Temporality

[<Arbitrary(typeof<TestData.TwoRandomPeriods>)>]
module IntersectProperties =

    [<Property>]
    let ``Always ∩ Always = Always`` () =
        Period.Always |> Period.intersect Period.Always = Some(Period.Always)

    [<Property>]
    let ``Always ∩ p = p`` (p1, p2) = 
        Period.Always |> Period.intersect p1 = Some(p1)
        && Period.Always |> Period.intersect p2 = Some(p2)

    [<Property>]
    let ``Never ∩ p = None`` (p1, p2) = 
        Period.Never |> Period.intersect p1 = None
        && Period.Never |> Period.intersect p2 = None
    
    [<Property>]
    let ``Never ∩ Never = None`` () = 
        Period.Never |> Period.intersect Period.Never = None

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