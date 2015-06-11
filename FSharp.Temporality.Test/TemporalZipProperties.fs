module TemporalZipProperties

open FsCheck.Xunit
open Temporality

[<Arbitrary(typeof<TestData.TupleRandomTemporal>)>]
module TestWithRandom =
    [<Property>]
    let ``Given (t1, t2) zipped then the values should intersect with any t1.Values and t2.Values``(t1:Temporal<string>, t2:Temporal<bool>) =

        let zipped = 
            (t1,t2)
            |> Temporal.zip
            |> Temporal.temporaries
        
        let intersectWith t1 t2 = 
            match Temporary.intersect t1 t2 with
            | Some _ -> true
            | None -> false
        
        let anyIntersect temporary temporal = 
            let intersectWithGivenTemporary = intersectWith temporary
            temporal
            |> Temporal.temporaries
            |> Seq.where(intersectWithGivenTemporary)
            |> Seq.length > 0

        let intersectWithGivenTemporals temporary = 
            let (s,b) = temporary.Value
            let toTemporary v = { Period = temporary.Period; Value = v }
            let anyIntersectWithTemporary t v = anyIntersect (v |> toTemporary) t
            anyIntersectWithTemporary t1 s
            && anyIntersectWithTemporary t2 b

        zipped |> Seq.forall(intersectWithGivenTemporals)