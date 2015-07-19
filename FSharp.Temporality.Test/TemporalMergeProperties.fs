module TemporalMergeProperties

open FsCheck
open FsCheck.Xunit

open Temporality
open FsUnit.Xunit

let Given v = v
let When f v = 
    v 
    |> Temporal.toTemporal 
    |> f 
    |> Temporal.temporaries

let Then check expected actual = 
    check (expected |> Temporal.toTemporal |> Temporal.temporaries) actual

let shouldEqual = should equal

[<Xunit.Fact>]
let ``simple merge test``()=
    Given 
        [ TimeSpan.forNDays 1 |> Period.from (DateTime(2015,1,1)) |> Temporary.create "Hello"
          TimeSpan.forNDays 3 |> Period.from (DateTime(2015,1,2)) |> Temporary.create "Hello"
          TimeSpan.forNDays 5 |> Period.from (DateTime(2015,1,5)) |> Temporary.create "World"
          TimeSpan.forNDays 10 |> Period.from (DateTime(2015,1,10)) |> Temporary.create "World" ]
    |> When Temporal.merge
    |> Then shouldEqual
        [ TimeSpan.forNDays 4 |> Period.from (DateTime(2015,1,1)) |> Temporary.create "Hello"
          TimeSpan.forNDays 10 |> Period.from (DateTime(2015,1,5)) |> Temporary.create "World" ]


[<Arbitrary(typeof<TestData.RandomStringTemporal>)>]
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