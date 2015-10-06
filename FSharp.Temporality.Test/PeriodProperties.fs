module PeriodProperties

open FsCheck.Xunit

open Temporality

let jan15 d = DateTime(2015,1,d)
let empty = jan15 1 => jan15 1

[<Arbitrary(typeof<TestData.RandomPeriod>)>]
module IntersectProperties =
    [<Property>]
    let ``Infinite ∩ Infinite = Infinite`` () =
        Period.infinite |> Period.intersect Period.infinite = Some Period.infinite

    [<Property>]
    let ``Infinite ∩ p = p`` p = 
        Period.infinite |> Period.intersect p = Some p

    [<Property>]
    let ``p ∩ p = p`` p = 
        p |> Period.intersect p = Some p

    [<Property>]
    let ``p1 ∩ p2 ∪ p1 = p1`` p1 p2 =
        (Period.intersect p1 p2).Value |> Period.union p1 = (Some p1)

[<Arbitrary(typeof<TestData.RandomPeriod>)>]
module UnionProperties = 
    
    [<Property>]
    let ``empty ∪ empty = empty``() =
        empty |> Period.union empty = Some empty

    [<Property>]
    let ``Infinite ∪ Infinite = Infinite``()=
        Period.infinite |> Period.union Period.infinite = Some Period.infinite

    [<Property>]
    let ``Infinite ∪ p = Infinite`` p = 
        Period.infinite |> Period.union p = Some Period.infinite

    [<Property>]
    let ``p ∪ p = p`` p = 
        p |> Period.union p = Some p

    [<Property>]
    let ``p1 ∪ p2 ∩ p1 = p1`` p1 p2 = 
        match Period.union p1 p2 with
        | Some u when u.startDate = u.endDate -> Period.intersect p1 p2 = Some u
        | Some u -> Period.intersect u p1 = Some p1
        | None -> false