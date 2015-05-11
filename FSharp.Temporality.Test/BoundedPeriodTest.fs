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
        (boundedPeriod |> BoundedPeriod.ToDailyExclusive).startDate = (boundedPeriod |> BoundedPeriod.ToInclusive).startDate
    
    let ``exclusive end date must be greater than inclusive end date`` boundedPeriod =
        (boundedPeriod |> BoundedPeriod.ToDailyExclusive).endDate > (boundedPeriod |> BoundedPeriod.ToInclusive).endDate
    
    let ``same periods, same bound are equals`` boundedPeriod = 
        match boundedPeriod with
        | Inclusive ip -> boundedPeriod |> BoundedPeriod.ToInclusive = ip        
        | Exclusive ep -> boundedPeriod |> BoundedPeriod.ToDailyExclusive = ep        

    ``Check for some valid periods that:``
        [ ``start date never change``
          ``exclusive end date must be greater than inclusive end date``
          ``same periods, same bound are equals`` ]
        
[<Fact>]
let ``startDate must be greater than endDate for BoundedPeriod``()=
    [ BoundedPeriod.ToInclusive 
      BoundedPeriod.ToDailyExclusive ]
    |> ``Check for some invalid periods that throw``<BoundedPeriodException> 

[<Fact>]
let ``discover order on timespan``()=
    let jan15 day = System.DateTime(2015, 01, day)

    let from10To12 = jan15 12 - jan15 10
    let from8To10 = jan15 10 - jan15 8
    let from8To15 = jan15 15 - jan15 8

    Assert.Equal(from10To12, from8To10)
    Assert.True((from10To12 = from8To10))