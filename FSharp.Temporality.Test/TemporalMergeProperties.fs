module TemporalMergeProperties

open FsCheck
open FsCheck.Xunit

open Temporality

[<Arbitrary(typeof<TestData.RandomTemporal>)>]
module Merge =
    
    [<Property>]
    let ``grouped temporary value can't be unioned`` (temporal : Temporal<string>) =
        let actual = (temporal |> Temporal.merge).Values 

        let groups = 
            actual
            |> Seq.groupBy(fun t -> t.Value)
            |> Seq.toList

        let union (_, temporaries) = 

            let rec internalUnion temporaries = 
                match temporaries with
                | t1 :: t2 :: tail -> 
                    match Temporary.union t1 t2 with
                    | Some _ -> true
                    | None -> internalUnion (t2 :: tail)
                | [t1] -> false
                | [] -> false
            
            internalUnion (temporaries |> Seq.toList)

        groups |> List.forall(not << union)

    [<Property>]
    let ``temporal with same Hello value is the union`` (temporal:Temporal<string>) =
        let mergedTemporal = temporal |> Temporal.merge

        if(temporal.Values |> Seq.length = 0) 
        then mergedTemporal.Values |> Seq.length = 0
        else 
            let twiceNotEqualValue group = 
                match group with
                | t1:Temporary<string> :: t2 :: tail -> 
                    t1.Value <> t2.Value || (t1.Period |> Period.intersect t2.Period |> Period.isEmpty)
                | [_] | [] -> true
            
            mergedTemporal.Values 
            |> Seq.groupBy (fun t -> t.Value)
            |> Seq.forall(fun (g,t) -> twiceNotEqualValue (t |> Seq.toList))