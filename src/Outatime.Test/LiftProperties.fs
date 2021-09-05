module LiftProperties

open Outatime
open FsCheck.Xunit

let jan15 d = DateTime(2015,1,1,0,0,0,System.DateTimeKind.Utc)

module LiftTemporal = 

    let splitPeriod = System.TimeSpan.FromDays(1000.)
    
    let inline build l = l |> Outatime.build |> Outatime.contiguous

    [<Property(Arbitrary=[| typeof<TestData.RandomStringTemporal> |])>]
    let ``lift2 of temporals and empty should always return empty temporals`` (t:Temporary<string> list) = 
        Outatime.lift2 (sprintf "x=%s/y=%s") (Outatime.build t) (Outatime.build []) |> Outatime.toList = []

    [<Property(Arbitrary=[| typeof<TestData.RandomStringTemporal> |])>]
    let ``lift2 of empty and temporals should always return empty temporals`` (t:Temporary<string> list) = 
        Outatime.lift2 (sprintf "x=%s/y=%s") (Outatime.build []) (Outatime.build t) |> Outatime.toList = []

    [<Property(Arbitrary=[| typeof<TestData.RandomStringTemporal> |])>]
    let ``contiguous lift2 should always return contiguous period`` (t1:Temporary<string> list) (t2:Temporary<string> list) = 
        let t1' = build t1
        let t2' = build t2 
        
        let r = Outatime.lift2 (sprintf "x=%A/y=%A") t1' t2'

        let actual = r |> Outatime.toList |> List.map(fun t -> t.Interval)
        let expected = r |> Outatime.contiguous |> Outatime.toList |> List.map(fun t -> t.Interval)

        actual = expected
        