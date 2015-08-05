module PeriodProperties

open FsCheck.Xunit

open Temporality

let jan15 d = DateTime(2015,1,d)
let empty = Period.Empty (jan15 1)

[<Arbitrary(typeof<TestData.RandomPeriod>)>]
module IntersectProperties =
    [<Property>]
    let ``Infinite ∩ Infinite = Infinite`` () =
        Period.Infinite |> Period.intersect Period.Infinite = Period.Infinite

    [<Property>]
    let ``Infinite ∩ p = p`` p = 
        Period.Infinite |> Period.intersect p = p

    [<Property>]
    let ``Empty ∩ p = Empty`` p = 
        match empty |> Period.intersect p with
        | Period.Empty _ -> true
        | _ -> false
    
    [<Property>]
    let ``Empty ∩ Empty = Empty`` () = 
        empty |> Period.intersect empty = empty

    [<Property>]
    let ``p ∩ p = p`` p = 
        p |> Period.intersect p = p

    [<Property>]
    let ``p1 ∩ p2 ∪ p1 = p1`` p1 p2 =
        Period.intersect p1 p2 |> Period.union p1 = p1

[<Arbitrary(typeof<TestData.RandomPeriod>)>]
module UnionProperties = 
    
    [<Property>]
    let ``empty ∪ empty = empty``() =
        empty |> Period.union empty = empty

    [<Property>]
    let ``Infinite ∪ Infinite = Infinite``()=
        Period.Infinite |> Period.union Period.Infinite = Period.Infinite

    [<Property>]
    let ``Infinite ∪ p = Infinite`` p = 
        Period.Infinite |> Period.union p = Period.Infinite

    [<Property>]
    let ``p ∪ p = p`` p = 
        p |> Period.union p = p

    [<Property>]
    let ``p1 ∪ p2 ∩ p1 = p1`` p1 p2 = 
        match Period.union p1 p2 with
        | Period.Empty u -> Period.intersect p1 p2 = u
        | u -> Period.intersect u p1 = p1