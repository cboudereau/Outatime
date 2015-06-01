module MergeProperties

open FsCheck
open FsCheck.Xunit

open Temporality

[<Arbitrary(typeof<TestData.NoOverlapTemporaries>)>]
module NoOverlapMerge =
    
    [<Property>]
    let ``grouped temporary value can't be unioned`` (temporaries : Temporary<string> seq) =
        let groups = 
            (temporaries 
            |> Temporal.toTemporal 
            |> Temporal.merge).Values 
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

[<Arbitrary(typeof<TestData.HelloValidRepresentableTemporal>)>]
module SameValue =

    [<Property>]
    let ``temporal with same Hello value is the union`` (temporal:Temporal<string>) =
        let mergedTemporal = temporal |> Temporal.merge

        if(temporal.Values |> Seq.length = 0) 
        then mergedTemporal.Values |> Seq.length = 0
        else mergedTemporal.Values |> Seq.length = 1