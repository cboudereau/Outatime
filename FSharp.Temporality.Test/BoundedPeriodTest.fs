module BoundedPeriodTest

open Xunit
open FsCheck
open Temporality

let validPeriod = 
    function 
    | Inclusive p -> p.startDate <= p.endDate
    | Exclusive p -> p.startDate < p.endDate

let ArbValidPeriod = 
    Arb.generate<BoundedPeriod>
    |> Gen.suchThat validPeriod
    |> Arb.fromGen

let ``Check for all valid period that:`` fs = 
    fs
    |> Seq.iter(fun f -> Check.QuickThrowOnFailure(Prop.forAll ArbValidPeriod f))

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