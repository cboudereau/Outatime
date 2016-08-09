module ApplicativeFunctorTests

open Bdd
open Xunit
open Outatime

let jan15 d = DateTime(2015, 1, d)

let (<!>) f x = x |> Outatime.build |> Outatime.contiguous |> Outatime.lift f
let (<*>) f x = x |> Outatime.build |> Outatime.contiguous |> Outatime.apply f

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

let ``transform temporaries into request`` temporal = 
    let folder state p v = 
        seq {
            let toString (date:DateTime) = date.ToString("yyyy/MM/dd")
            let start = p |> start |> toString
            let enD = p |> enD |> toString

            yield! state
            match v with
            | Closed -> yield sprintf "[%s; %s[ = Closed" start enD
            | Opened rate -> 
                let (Availability availability) = rate.Availability
                let (Price price) = rate.Price
                let departure = 
                    match rate.Departure with
                    | ClosedToDeparture -> "closed to departure"
                    | OpenedToDeparture -> "opened to departure"
                yield sprintf "[%s; %s[ = Opened with %i of availibility at %.2f price and %s" start enD availability price departure }
        
    temporal
    |> Outatime.merge
    |> Outatime.ofOption
    |> Outatime.fold folder Seq.empty
    |> Seq.toList

[<Fact>]
let ``given empty temporaries expect empty temporaries``()=
    When
        ``transform temporaries to rate availability domain``
        <!> [ ]

        <*> [ ]

        <*> [ ]

        <*> [ ]
        |> ``transform temporaries into request``
    |> Expect [ ]
   
[<Fact>]
let ``given partial map empty temporaries expect temporaries``()=
    When
        ``transform temporaries to rate availability domain``
        <!> [ ]

        <*> [ jan15 2  => jan15 15 := OpenedToDeparture
              jan15 16 => jan15 18 := OpenedToDeparture
              jan15 18 => jan15 23 := ClosedToDeparture ]

        <*> [ jan15 1  => jan15 22 := Availability 10 ]

        <*> [ jan15 1  => jan15 22 := Price 120m ]
        |> ``transform temporaries into request``
    |> Expect [ ]

[<Fact>]
let ``given partial applied empty temporaries expect temporaries``()=
    When
        ``transform temporaries to rate availability domain``
        <!> [ jan15 4  => jan15 5  := Opening.Opened
              jan15 5  => jan15 20 := Opening.Closed ]

        <*> [ ]

        <*> [ jan15 1  => jan15 22 := Availability 10 ]

        <*> [ jan15 1  => jan15 23 := Price 120m ]
        |> ``transform temporaries into request``
    |> Expect [ ]

    When
        ``transform temporaries to rate availability domain``
        <!> [ jan15 4  => jan15 5  := Opening.Opened
              jan15 5  => jan15 20 := Opening.Closed ]

        <*> [ jan15 2  => jan15 15 := OpenedToDeparture
              jan15 16 => jan15 18 := OpenedToDeparture
              jan15 18 => jan15 23 := ClosedToDeparture ]

        <*> [ ]

        <*> [ jan15 1  => jan15 22 := Price 120m ]
        |> ``transform temporaries into request``
    |> Expect [ ]

[<Fact>]
let ``given temporaries with empty periods expect the largest period with none value``()=
    When
        ``transform temporaries to rate availability domain``
        <!> [ jan15 4  => jan15 4  := Opening.Opened
              jan15 5  => jan15 5 := Opening.Closed ]

        <*> [ jan15 1 => jan15 16 := OpenedToDeparture
              jan15 2  => jan15 2 := OpenedToDeparture ]

        <*> [ jan15 1  => jan15 1 := Availability 10 ]

        <*> [ jan15 3  => jan15 3 := Price 120m ]
        |> ``transform temporaries into request``
    |> Expect [ ]
    

[<Fact>]
let ``given multiple temporaries without intersection, when apply a function on this temporaries then expect none value with the largest period``()=

    When
        ``transform temporaries to rate availability domain``
        <!> [ jan15 4  => jan15 5  := Opening.Opened
              jan15 5  => jan15 20 := Opening.Closed ]

        <*> [ jan15 20  => jan15 25 := OpenedToDeparture ]

        <*> [ jan15 26  => jan15 27 := Availability 10 ]

        <*> [ jan15 29  => jan15 30 := Price 120m ]
        |> ``transform temporaries into request``
    |> Expect 
        [  ]

    When
        ``transform temporaries to rate availability domain``
        <!> [ jan15 29  => jan15 30  := Opening.Opened ]

        <*> [ jan15 20  => jan15 25 := OpenedToDeparture ]

        <*> [ jan15 26  => jan15 27 := Availability 10 ]

        <*> [ jan15 4  => jan15 5 := Price 120m ]
        |> ``transform temporaries into request``
    |> Expect 
        [ ]

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

        <*> [ jan15 1  => jan15 22 := Price 120m ]
        |> ``transform temporaries into request``
    |> Expect 
        [ "[2015/01/04; 2015/01/05[ = Opened with 10 of availibility at 120.00 price and opened to departure"
          "[2015/01/05; 2015/01/15[ = Closed"
          "[2015/01/16; 2015/01/20[ = Closed" ]