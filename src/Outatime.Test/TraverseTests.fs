module TraverseTests

open Outatime
open Xunit
open Bdd

type RoomCode = RoomCode of string

type Opening = Opened | Closed

type Availability = 
    | Opened of int
    | Closed

type RateCode = 
    | RateCode of string
    | AllRate

type Price = Price of decimal

type Rate = 
    { rateCode : RateCode
      prices : Price Temporaries }

type Room = 
    { roomCode : RoomCode
      availabilities : Availability Temporaries
      rates : Rate seq }

let jan15 d = System.DateTime(2015, 1, d, 0, 0, 0, System.DateTimeKind.Utc)

module Repartition = 
    let fullStock avail _ _ = avail
    
    //Here it is the simplest repartition, in addition, add closed and max sales on rates... (there is a bug, find it!)
    let rateLevel avail n i = 
        match avail with
        | Closed -> Closed
        | Opened a -> 
            let (stock, remain) = a / n, (a % n)
            stock + (max 0 (remain - i)) |> Opened

module Partner = 
    type Allotment = Allotment of int
    
    type PartnerRate = 
        | Opened of RoomCode * RateCode * Price * Allotment
        | Closed of RoomCode * RateCode

    let transpose availRepartition roomO pricesO = 
        match roomO, pricesO with
        | Some (roomCode, availability), Some prices ->
            let numberOfRate = prices |> Seq.length
            prices 
            |> Map.toSeq 
            |> Seq.mapi(fun i (c, p) -> 
                match i |> availRepartition availability numberOfRate with
                | Availability.Closed -> Closed (roomCode, c)
                | Availability.Opened allot -> Opened (roomCode, c, p, (Allotment allot)))
            |> Some
        | Some (roomCode, _), None -> Closed (roomCode, RateCode.AllRate) |> Seq.singleton |> Some
        | _ -> None

    let toRequest t = 
        match t.Value with
        | None -> sprintf "%O => No request" t.Period |> Seq.singleton
        | Some values -> 
            let toR = function
                | Closed (roomCode, rateCode) ->
                    sprintf "%O => %A, %A = Closed" t.Period roomCode rateCode 
                | Opened (roomCode, rateCode, price, allot) ->
                    sprintf "%O => %A, %A = Opened %A/%A" t.Period roomCode rateCode allot price
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
      availabilities = 
        [ jan15  1 => jan15 10 := Opened 10 
          jan15 10 => jan15 25 := Closed
          jan15 27 => jan15 30 := Opened 20 ]
      rates = 
        [ { rateCode= RateCode "RO"
            prices= 
                [ jan15  1 => jan15 10 := Price 120m 
                  jan15 10 => jan15 28 := Price 115m ] } 
          { rateCode= RateCode "BB"; prices= [ jan15 1 => jan15 15 := Price 135m ] }] }

let double = 
    { roomCode = RoomCode "DBL"
      availabilities = 
        [ jan15 1 => jan15  8 := Opened 5
          jan15 8 => jan15 23 := Opened 7 ]
      rates = 
        [ { rateCode= RateCode "RO"; prices= [ jan15 1 => jan15 25 := Price 240m ] } 
          { rateCode= RateCode "BB"; prices= [ jan15 1 => jan15 25 := Price 270m ] }] }

[<Fact>]
let ``tranpose avp model to partner model with rate level repartition`` ()=
    let r = 
        [ single
          double ]
        |> Seq.collect (transposeRoom Repartition.rateLevel)

    When 
        r
        |> Seq.collect Partner.toRequest
        |> Seq.toList
    |> Expect 
        [ @"[2015/01/01; 2015/01/10[ => RoomCode ""SGL"", RateCode ""BB"" = Opened Allotment 5/Price 135M"
          @"[2015/01/01; 2015/01/10[ => RoomCode ""SGL"", RateCode ""RO"" = Opened Allotment 5/Price 120M"
          @"[2015/01/10; 2015/01/15[ => RoomCode ""SGL"", RateCode ""BB"" = Closed"
          @"[2015/01/10; 2015/01/15[ => RoomCode ""SGL"", RateCode ""RO"" = Closed"
          @"[2015/01/15; 2015/01/25[ => RoomCode ""SGL"", RateCode ""RO"" = Closed"
          @"[2015/01/25; 2015/01/27[ => No request"
          @"[2015/01/27; 2015/01/28[ => RoomCode ""SGL"", RateCode ""RO"" = Opened Allotment 20/Price 115M"
          @"[2015/01/28; 2015/01/30[ => RoomCode ""SGL"", AllRate = Closed"
          @"[2015/01/01; 2015/01/08[ => RoomCode ""DBL"", RateCode ""BB"" = Opened Allotment 3/Price 270M"
          @"[2015/01/01; 2015/01/08[ => RoomCode ""DBL"", RateCode ""RO"" = Opened Allotment 2/Price 240M"
          @"[2015/01/08; 2015/01/23[ => RoomCode ""DBL"", RateCode ""BB"" = Opened Allotment 4/Price 270M"
          @"[2015/01/08; 2015/01/23[ => RoomCode ""DBL"", RateCode ""RO"" = Opened Allotment 3/Price 240M"
          @"[2015/01/23; 2015/01/25[ => No request" ]
    