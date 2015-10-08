module ApplicativeFunctorTests

open Bdd
open Xunit
open Temporality

let jan15 d = DateTime(2015, 1, d)

type Opening = Opened | Closed
type Departure = Departure of Opening
type Availability = Availability of int
type Price = Price of decimal

type Rate = 
    { departure: Departure
      availability: Availability
      price: Price }

type RateAvailability = 
    | Closed
    | Opened of Rate

[<Fact>]
let ``given multiple temporaries, when apply a function on this expect applied function on any intersection``()=
    let ``project temporaries to rate availability domain`` openingO departureO availabilityO priceO = 
        match openingO, departureO, availabilityO, priceO with
        | Some (Opening.Opened), Some d, Some a, Some p -> RateAvailability.Opened { departure=d; availability=a; price=p }
        | Some (Opening.Closed), _, _, _ | _ -> RateAvailability.Closed

    let ``transform temporaries into request`` temporaries = 
        let request temporary = 
            match temporary.value with
            | Closed -> sprintf "%O = Closed" temporary.period
            | Opened rate -> 
                let (Availability a) = rate.availability
                let (Price p) = rate.price
                let d = 
                    let (Departure departure) = rate.departure
                    match departure with
                    | Opening.Closed -> "closed to departure"
                    | Opening.Opened -> "opened to departure"

                sprintf "%O = Opened with %i of availibility at %f price and %s" temporary.period a p d
            
        temporaries 
        |> Seq.map request
        |> Seq.toList

    When 
        ``project temporaries to rate availability domain``
        <!> [ jan15 4  => jan15 5  := Opening.Opened
              jan15 5  => jan15 20 := Opening.Closed ]
        <*> [ jan15 2  => jan15 15 := Departure Opening.Opened
              jan15 15 => jan15 16 := Departure Opening.Closed
              jan15 16 => jan15 18 := Departure Opening.Opened
              jan15 18 => jan15 23 := Departure Opening.Closed ]
        <*> [ jan15 1  => jan15 22 := Availability 10 ]
        <*> [ jan15 1  => jan15 22 := Price 120m ]
        |> ``transform temporaries into request``
    |> Expect 
        [ "[0001/01/01; 2015/01/01[ = Closed"
          "[2015/01/01; 2015/01/02[ = Closed"
          "[2015/01/02; 2015/01/04[ = Closed"
          "[2015/01/04; 2015/01/05[ = Opened with 10 of availibility at 120.000000 price and opened to departure"
          "[2015/01/05; 2015/01/15[ = Closed"
          "[2015/01/15; 2015/01/16[ = Closed"
          "[2015/01/16; 2015/01/18[ = Closed"
          "[2015/01/18; 2015/01/20[ = Closed"
          "[2015/01/20; 2015/01/22[ = Closed"
          "[2015/01/22; 2015/01/23[ = Closed"
          "[2015/01/23; 9999/12/31[ = Closed" ]