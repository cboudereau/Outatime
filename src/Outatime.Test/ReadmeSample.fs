module ReadmeSample

open Outatime
open Xunit

//Simple BDD functions
let When f = f
let With v f = f v
let For = With
let And = With
let Then check expected = check expected

let shouldEqual<'a> (expected:'a) (actual:'a) = Assert.Equal(expected, actual)

let Expect<'a> = Then shouldEqual<'a>

let shouldBeEmpty actual = Assert.Equal(0, actual |> Seq.length)

let jan15 d = DateTime(2015, 1, d)

//A domain sample
type Opening = Opened | Closed
type Departure = OpenedToDeparture | ClosedToDeparture
type Availability = Availability of int
type Price = Price of decimal

type Rate = 
    { Departure: Departure
      Availability: Availability
      Price: Price }

type RateAvailability = 
    | Closed
    | Opened of Rate

let ``transform temporaries to rate availability domain`` openingO departureO availabilityO priceO = 
    match openingO, departureO, availabilityO, priceO with
    | Some opening, Some departure, Some availability, Some price -> 
        match opening with
        | Opening.Opened ->
            RateAvailability.Opened 
                { Departure=departure; Availability=availability; Price=price }
            |> Some
        | Opening.Closed -> Some RateAvailability.Closed
    | _ -> None

let ``transform temporaries into request`` temporaries = 
    let request t = 
        match t.Value with
        | None -> 
            sprintf "%O = No Request (May be put a state monad here for not contiguous case)" t.Period
        | Some Closed -> 
            sprintf "%O = Closed" t.Period
        | Some (Opened rate) -> 
            let (Availability a) = rate.Availability
            let (Price p) = rate.Price
            let d = 
                match rate.Departure with
                | ClosedToDeparture -> "closed to departure"
                | OpenedToDeparture -> "opened to departure"

            sprintf "%O = Opened with %i of availibility at %.2f price and %s" t.Period a p d
        
    temporaries 
    |> Seq.map request
    |> Seq.toList

[<Fact>]
let ``given temporaries with no intersections or empty periods expect the largest period with none value``()=
    When
        ``transform temporaries to rate availability domain``
        <!> [ jan15 4  => jan15 4  := Opening.Opened
              jan15 5  => jan15 5 := Opening.Closed ]

        <*> [ jan15 1 => jan15 16 := OpenedToDeparture
              jan15 2  => jan15 2 := OpenedToDeparture ]

        <*> [ jan15 1  => jan15 1 := Availability 10 ]

        <*?> [ jan15 3  => jan15 3 := Price 120m ]
        |> ``transform temporaries into request``
    |> Expect ["[2015/01/01; 2015/01/16[ = No Request (May be put a state monad here for not contiguous case)"]
    

[<Fact>]
let ``given multiple temporaries, when apply a function on this temporaries then expect applied function on any intersection``()=

    When
        ``transform temporaries to rate availability domain``
        <!> [ jan15 4  => jan15 5  := Opening.Opened
              jan15 5  => jan15 20 := Opening.Closed ]

        <*> [ jan15 2  => jan15 15 := OpenedToDeparture
              jan15 16 => jan15 18 := OpenedToDeparture
              jan15 18 => jan15 23 := ClosedToDeparture ]

        <*> [ jan15 1  => jan15 22 := Availability 10 ]

        <*?> [ jan15 1  => jan15 22 := Price 120m ]
        |> ``transform temporaries into request``
    |> Expect 
        [ "[2015/01/01; 2015/01/04[ = No Request (May be put a state monad here for not contiguous case)"
          "[2015/01/04; 2015/01/05[ = Opened with 10 of availibility at 120.00 price and opened to departure"
          "[2015/01/05; 2015/01/15[ = Closed"
          "[2015/01/15; 2015/01/16[ = No Request (May be put a state monad here for not contiguous case)"
          "[2015/01/16; 2015/01/20[ = Closed"
          "[2015/01/20; 2015/01/23[ = No Request (May be put a state monad here for not contiguous case)" ]

