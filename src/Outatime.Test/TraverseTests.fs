module TraverseTests

open Outatime
open Xunit
open Bdd

type RoomCode = RoomCode of string

type Opening = Opened | Closed

type Availability = 
    | Opened of int
    | Closed

type RateCode = RateCode of string
type Price = Price of decimal

type Rate = 
    { rateCode : RateCode
      prices : Price Temporaries }

type Room = 
    { roomCode : RoomCode
      availabilities : Availability Temporaries
      rates : Rate seq }

let jan15 d = System.DateTime(2015, 1, d, 0, 0, 0, System.DateTimeKind.Utc)


//Here it is the simplest repartition, in addition, add closed and max sales on rates...
module Repartition = 
    let fullStock avail _ _ = avail
    let rateLevel avail n i = 
        match avail with
        | Closed -> Closed
        | Opened a -> 
            let (stock, remain) = a / n, (a % n)
            stock + (max 0 (remain - i)) |> Opened

module Partner = 
    type PartnerRate = 
        { rateCode : RateCode
          roomCode : RoomCode
          price : Price
          availability : Availability }
    
    let transpose availRepartition roomO pricesO = 
        match roomO, pricesO with
        | Some (roomCode, availability), Some prices ->
            let numberOfRate = prices |> Seq.length
            prices 
            |> Map.toSeq 
            |> Seq.mapi(fun i (c, p) -> { rateCode=c; roomCode=roomCode; price=p; availability= i |> availRepartition availability numberOfRate }) 
            |> Some
        | _ -> None

    let toRequest t = 
        match t.Value with
        | None -> sprintf "%O => No request" t.Period |> Seq.singleton
        | Some values -> 
            let toR value = sprintf "%O => %A, %A = %A/%A" t.Period value.roomCode value.rateCode value.availability value.price
            values |> Seq.map toR

let transposeRoom repartition room = 
    let transposeRate rates = 
        rates 
        |> Seq.map(fun r -> r.rateCode, r.prices) 
        |> Map.ofSeq 
        |> Outatime.traverse
    
    let roomWithRoomCode = (room.availabilities |> Seq.map(fun a -> a.Period := (room.roomCode, a.Value)))

    Partner.transpose repartition
    <!>  roomWithRoomCode
    <*?> (room.rates |> transposeRate)

let single = 
    { roomCode = RoomCode "SGL"
      availabilities = [ jan15 1 => jan15 10 := Opened 10 ]
      rates = 
        [ { rateCode= RateCode "RO"; prices= [ jan15 1 => jan15 10 := Price 120m ] } 
          { rateCode= RateCode "BB"; prices= [ jan15 1 => jan15 10 := Price 135m ] }] }

let double = 
    { roomCode = RoomCode "DBL"
      availabilities = [ jan15 1 => jan15 8 := Opened 5 ]
      rates = 
        [ { rateCode= RateCode "RO"; prices= [ jan15 1 => jan15 8 := Price 240m ] } 
          { rateCode= RateCode "BB"; prices= [ jan15 1 => jan15 8 := Price 270m ] }] }

[<Fact>]
let ``tranpose avp model to partner model with rate level repartition`` ()=

    When 
        [ single
          double ]
        |> Seq.collect (transposeRoom Repartition.rateLevel)
        |> Seq.collect Partner.toRequest
        |> Seq.toList
    |> Expect 
        [ @"[2015/01/01; 2015/01/10[ => RoomCode ""SGL"", RateCode ""BB"" = Opened 5/Price 135M"
          @"[2015/01/01; 2015/01/10[ => RoomCode ""SGL"", RateCode ""RO"" = Opened 5/Price 120M"
          @"[2015/01/01; 2015/01/08[ => RoomCode ""DBL"", RateCode ""BB"" = Opened 3/Price 270M"
          @"[2015/01/01; 2015/01/08[ => RoomCode ""DBL"", RateCode ""RO"" = Opened 2/Price 240M" ]
    