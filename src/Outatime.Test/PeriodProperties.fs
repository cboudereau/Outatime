module PeriodProperties

open FsCheck.Xunit

open Outatime

let jan15 d = DateTime(2015,1,d)
let empty = jan15 1 => jan15 1

[<Arbitrary(typeof<TestData.RandomPeriod>)>]
module IntersectProperties =
    [<Property>]
    let ``Infinite ∩ Infinite = Infinite`` () =
        infinite |> intersect infinite = Some infinite

    [<Property>]
    let ``Infinite ∩ p = p`` p = 
        
        match infinite |> intersect p with
        | None when isEmpty p -> true
        | Some i when i = p -> true
        | _ -> false

    [<Property>]
    let ``p ∩ p = p`` p = 
        match p |> intersect p with
        | None when isEmpty p -> true
        | Some u when u = p -> true
        | _ -> false

[<Arbitrary(typeof<TestData.RandomPeriod>)>]
module UnionProperties = 
    
    [<Property>]
    let ``empty ∪ empty = None``() =
        empty |> union empty = None

    [<Property>]
    let ``Infinite ∪ Infinite = Infinite``()=
        infinite |> union infinite = Some infinite

    [<Property>]
    let ``Infinite ∪ p = Infinite`` p = 
        infinite |> union p = Some infinite

    [<Property>]
    let ``p ∪ p = p`` p = 
        match p |> union p with
        | None when isEmpty p -> true
        | Some u when u = p -> true
        | _ -> false