#load "Outatime.fs"

open Outatime

type Price = Price of decimal

let jan15 d = System.DateTime(2015, 1, d, 0,0,0,System.DateTimeKind.Utc)

let actual = 
    [ ("R1", [ jan15 1 => jan15  2 := Price 100m
               jan15 3 => jan15 20 := Price 120m ])
      ("R2", [ jan15 2 => jan15  5 := Price 80m 
               jan15 7 => jan15 21 := Price 75m]) ] |> Map.ofList

let expected = 
    [ jan15  1 => jan15  2 := ([ ("R1", Price 100m) ] |> Map.ofList)

      jan15  3 => jan15  5 := ([ ("R1", Price 120m)
                                 ("R2", Price  80m) ] |> Map.ofList)

      jan15  5 => jan15  7 := ([ ("R1", Price 120m) ] |> Map.ofList)

      jan15  7 => jan15 20 := ([ ("R1", Price 120m)
                                 ("R2", Price  75m) ] |> Map.ofList)

      jan15 20 => jan15 21 := ([ ("R2", Price  75m) ] |> Map.ofList) ]

//First transform string * (price temporary list) into (string * price) temporary list

    
//take the (string * price) temporary list and make a list on each temporary intersection
let toMap map = 
    let transpose map = 
        map
        |> Map.toSeq
        |> Seq.collect (fun (k, temps) -> temps |> Seq.map(fun t -> t.Period := (k, t.Value)) |> Outatime.sort)
    
    let minStart t = t |> Seq.map(fun ts -> ts.Period.StartDate) |> Seq.min
    let maxEnd t = t |> Seq.map(fun ts -> ts.Period.EndDate) |> Seq.max
    let aggregate state t =
        seq { 
            match state |> Outatime.clamp t.Period with
            | i when i |> Seq.isEmpty -> 
                yield! state
                yield t.Period := (t.Value |> Seq.singleton)
            | i -> 
                let mins = state |> minStart
                let maxs = state |> maxEnd
                
                if t.Period.StartDate < mins then yield t.Period.StartDate => mins := (t.Value |> Seq.singleton)

                yield! state |> Outatime.clamp (infinite.StartDate => (i |> minStart))

                yield! i |> Seq.map(fun ti -> ti.Period := seq { yield! ti.Value; yield t.Value })

                yield! state |> Outatime.clamp (i |> maxEnd => infinite.EndDate)

                if t.Period.EndDate > maxs then yield maxs => t.Period.EndDate := (t.Value |> Seq.singleton)
         }
    
    map
    |> transpose
    |> Seq.fold aggregate Seq.empty
    |> Seq.map(fun t -> t.Period := (t.Value |> Map.ofSeq))

let result = actual |> toMap |> Seq.toList

result |> Seq.iter (printfn "%A")

result = expected
