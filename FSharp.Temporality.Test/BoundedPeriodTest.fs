module BoundedPeriodTest

open Xunit
open FsCheck
open Temporality

let ``Check for all valid period that:`` fs = 
    let validPeriod = function 
    | Inclusive p -> p.startDate <= p.endDate 
    | Exclusive p -> p.startDate < p.endDate
    
    let arb = 
        Arb.generate<BoundedPeriod>
        |> Gen.suchThat validPeriod 
        |> Arb.fromGen
    
    fs
    |> Seq.iter(fun f -> Check.QuickThrowOnFailure(Prop.forAll arb f))

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

    ``Check for all valid period that:``
        [ ``start date never change``
          ``exclusive end date must be greater than inclusive end date``
          ``same periods, same bound are equals`` ]

[<Fact>]
let ``startDate must be greater than endDate for BoundedPeriod``()=
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
        | :? BoundedPeriodException as ex -> true
        | _ -> false

    let ``inclusive case`` boundedPeriod = expectedBoundedPeriodException BoundedPeriod.ToInclusive  boundedPeriod
    let ``exclusive case`` boundedPeriod = boundedPeriod |> BoundedPeriod.ToExclusive
        
    Check.QuickThrowOnFailure(Prop.forAll arb ``inclusive case``)