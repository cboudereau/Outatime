module BoundedPeriodTest

open Xunit
open FsCheck
open Temporality

let ``Check for some valid periods that:`` fs = 
    let validPeriod = function 
    | Inclusive p -> p.startDate <= p.endDate 
    | Exclusive p -> p.startDate < p.endDate
    
    let arb = 
        Arb.generate<BoundedPeriod>
        |> Gen.suchThat validPeriod 
        |> Arb.fromGen
    
    fs
    |> Seq.iter(fun f -> Check.QuickThrowOnFailure(Prop.forAll arb f))

let ``Check for some invalid periods that throw``<'a when 'a :> exn> fs = 
    let invalidPeriod = function
    | Inclusive period -> period.startDate > period.endDate
    | Exclusive period -> period.startDate >= period.endDate
    
    let arb = 
        Arb.generate<BoundedPeriod>
        |> Gen.suchThat invalidPeriod 
        |> Arb.fromGen
    
    let expectedBoundedPeriodException f boundedPeriod = 
        try
            f boundedPeriod |> ignore
            false
        with
        | :? 'a as ex -> true
        | _ -> false

    fs
    |> Seq.iter(fun f -> Check.QuickThrowOnFailure(Prop.forAll arb <| expectedBoundedPeriodException f))

[<Fact>]
let ``convert inclusive period to exclusive period``() = 
    let ``start date never change`` boundedPeriod = 
        (boundedPeriod |> BoundedPeriod.ToExclusive).startDate = (boundedPeriod |> BoundedPeriod.ToInclusive).startDate
    
    let ``exclusive end date must be greater than inclusive end date`` boundedPeriod =
        (boundedPeriod |> BoundedPeriod.ToExclusive).endDate > (boundedPeriod |> BoundedPeriod.ToInclusive).endDate
    
    let ``same periods, same bound are equals`` boundedPeriod = 
        match boundedPeriod with
        | Inclusive ip -> boundedPeriod |> BoundedPeriod.ToInclusive = ip        
        | Exclusive ep -> boundedPeriod |> BoundedPeriod.ToExclusive = ep        

    ``Check for some valid periods that:``
        [ ``start date never change``
          ``exclusive end date must be greater than inclusive end date``
          ``same periods, same bound are equals`` ]
        
[<Fact>]
let ``startDate must be greater than endDate for BoundedPeriod``()=
    [ BoundedPeriod.ToInclusive 
      BoundedPeriod.ToExclusive ]
    |> ``Check for some invalid periods that throw``<BoundedPeriodException> 