module TemporalMergeProperties

open FsCheck
open FsCheck.Xunit

open Temporality
open FsUnit.Xunit

open Bdd

let jan15 n = (DateTime(2015,1,n))

[<Xunit.Fact>]
let ``simple merge test``()=
    Given 
        [ jan15 01 => jan15 02 := "Hello"
          jan15 02 => jan15 05 := "Hello"
          jan15 05 => jan15 10 := "World"
          jan15 10 => jan15 20 := "World" ]
    |> When Temporality.merge
    |> Then shouldEqual
        [ jan15 01 => jan15 05 := "Hello"
          jan15 05 => jan15 20 := "World" ]

[<Arbitrary(typeof<TestData.RandomStringTemporal>)>]
module Merge =
    
    [<Property>]
    let ``grouped temporary value can't be unioned`` (temporaries : string Temporary list) =
        let actual = (temporaries |> Temporality.merge)

        let groups = 
            actual
            |> Seq.groupBy(fun t -> t.value)
            |> Seq.toList

        let union (_, temporaries) = 

            let rec internalUnion temporaries = 
                match temporaries with
                | t1 :: t2 :: tail -> 
                    match Period.union t1.period t2.period, t1.value = t2.value with
                    | Some _, true -> true
                    | _ -> internalUnion (t2 :: tail)
                | [_] -> false
                | [] -> false
            
            internalUnion (temporaries |> Seq.toList)

        groups |> List.forall(not << union)

    [<Property>]
    let ``temporal with same Hello value is the union`` (temporaries:string Temporary list) =
        let mergedTemporal = temporaries |> Temporality.merge

        if(temporaries |> Seq.length = 0) 
        then mergedTemporal |> Seq.length = 0
        else 
            let twiceNotEqualValue group = 
                match group with
                | t1:Temporary<string> :: t2 :: tail -> 
                    t1.value <> t2.value || (t1.period |> Period.intersect t2.period |> Option.isNone)
                | [_] | [] -> true
            
            mergedTemporal 
            |> Seq.groupBy (fun t -> t.value)
            |> Seq.forall(fun (g,t) -> twiceNotEqualValue (t |> Seq.toList))