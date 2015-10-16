module PeriodProperties

open FsCheck.Xunit

open Outatime

let jan15 d = DateTime(2015,1,d)
let empty = jan15 1 => jan15 1

[<Arbitrary(typeof<TestData.RandomPeriod>)>]
module IntersectProperties =
    [<Property>]
    let ``Infinite ∩ Infinite = Infinite`` () =
        Period.infinite |> Period.intersect Period.infinite = Some Period.infinite

    [<Property>]
    let ``Infinite ∩ p = p`` p = 
        
        match Period.infinite |> Period.intersect p with
        | None when Period.isEmpty p -> true
        | Some i when i = p -> true
        | _ -> false

    [<Property>]
    let ``p ∩ p = p`` p = 
        match p |> Period.intersect p with
        | None when Period.isEmpty p -> true
        | Some u when u = p -> true
        | _ -> false

[<Arbitrary(typeof<TestData.RandomPeriod>)>]
module UnionProperties = 
    
    [<Property>]
    let ``empty ∪ empty = None``() =
        empty |> Period.union empty = None

    [<Property>]
    let ``Infinite ∪ Infinite = Infinite``()=
        Period.infinite |> Period.union Period.infinite = Some Period.infinite

    [<Property>]
    let ``Infinite ∪ p = Infinite`` p = 
        Period.infinite |> Period.union p = Some Period.infinite

    [<Property>]
    let ``p ∪ p = p`` p = 
        match p |> Period.union p with
        | None when Period.isEmpty p -> true
        | Some u when u = p -> true
        | _ -> false