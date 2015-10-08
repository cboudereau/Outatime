module TestData

open FsCheck
open Temporality

let getPeriod (d1, d2) = 
    let minDate = min d1 d2
    let maxDate = max d1 d2
    minDate => maxDate

type RandomPeriod = 
    static member Gen() = 
        let beginingPeriod = 
            Arb.generate<DateTime>
            |> Gen.map (fun d -> getPeriod(DateTime.MinValue, d))

        let endingPeriod = 
            Arb.generate<DateTime>
            |> Gen.map (fun d -> getPeriod(d, DateTime.MaxValue))

        let randomPeriod = 
            Arb.generate<DateTime>
            |> Gen.two
            |> Gen.map (getPeriod)
        
        let emptyPeriod = 
            Arb.generate<DateTime>
            |> Gen.map(fun d -> getPeriod(d,d))

        [ (3, randomPeriod)
          (1, emptyPeriod)
          (1, beginingPeriod)
          (1, endingPeriod) ]
        |> Gen.frequency 
        
    static member Values() = 
        RandomPeriod.Gen()
        |> Arb.fromGen

let toTemporaries l = 
    let rec internalToTemporaries s l = 
        seq { 
            match l with
            | (value, duration) :: tail -> 
                yield  s => s + duration := value
                yield! internalToTemporaries (s + duration) tail
            | [] -> yield! []
        }
    internalToTemporaries DateTime.MinValue l
    |> Seq.toList

type RandomStringTemporal = 
    static member Gen() =
        let emptyTemporal = 
            [ gen { return [] } ]
            |> Gen.oneof
        
        let overlapHelloTemporal = 
            Gen.map(fun p -> p := "Hello") (RandomPeriod.Gen())
            |> Gen.listOf
            |> Gen.map(fun temporaries -> temporaries)
        
        let noOverlapTemporal = 
            let daysGen = Gen.choose(0, 1000)
            
            [ 1, gen { return "Hello" }
              2, gen { return "World" } ]
            |> Gen.frequency 
            |> Gen.map2 (fun days value -> (value, TimeSpan.FromDays(float days))) daysGen
            |> Gen.listOf
            |> Gen.map (toTemporaries)

        [ emptyTemporal
          overlapHelloTemporal
          noOverlapTemporal ]
        |> Gen.oneof
    static member Values() = RandomStringTemporal.Gen() |> Arb.fromGen