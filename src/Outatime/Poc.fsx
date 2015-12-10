#load "Partial.fs"
#load "Partials.fs"
#load "Outatime.fs"

open Outatime

type Price = Price of decimal

let jan15 d = System.DateTime(2015, 1, d, 0,0,0,System.DateTimeKind.Utc)

let actual = 
    [ ("R1", [ jan15 1 => jan15  2 := Price 100m
               jan15 3 => jan15  4 := Price 120m
               jan15 4 => jan15 20 := Price 120m ])
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

let result = actual |> Outatime.traverse |> Seq.toList

result |> Seq.iter (printfn "%A")

result = expected
