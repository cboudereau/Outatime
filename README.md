# Outatime [![Build status](https://ci.appveyor.com/api/projects/status/v3f2gj9602e82ia4?svg=true)](https://ci.appveyor.com/project/cboudereau/outatime) [![NuGet Status](http://img.shields.io/nuget/v/Outatime.svg?style=flat)](https://www.nuget.org/packages/Outatime/)

![Outatime] (https://raw.githubusercontent.com/cboudereau/Outatime/master/intro.jpg)

=====

## Intro
When you have a lots of values changing over the time in a planning, it become difficult to deal with it day per day. The solution is to group the same values and merge period.
One way is having the temporary (type containing value on a period) everywhere but: you have the domain complexity everywhere and if you have to compose another domain you will have more wrapped types.

To simplify and compose functionalities, Outatime provides an applicative functor approach. 
Instead of having the wrapped type everywhere, a function containing all values as parameter is binded to values changing over the time.

## How it works
By using merge function, intersection period, and finding the largest period for missing values, a function is applied each time you give the temporaries. 
Each time, the value is the function applied (given a function with n parameter, expect a function with n-1 parameter).

## Samples
```fsharp
module ReadmeSample

open Outatime
open Xunit

let (<!>) f x = x |> Outatime.build |> Outatime.contiguous |> Outatime.map f
let (<*>) f x = x |> Outatime.build |> Outatime.contiguous |> Outatime.apply f

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

let ``transform temporaries to rate availability domain`` _ openingO departureO availabilityO priceO = 
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
    let request t = 
            let toString (date:DateTime) = date.ToString("yyyy/MM/dd")
            let start = t |> period |> start |> toString
            let enD = t |> period |> enD |> toString

            match t.Value with
            | Closed -> sprintf "[%s; %s[ = Closed" start enD
            | Opened rate -> 
                let (Availability a) = rate.Availability
                let (Price p) = rate.Price
                let d = 
                    match rate.Departure with
                    | ClosedToDeparture -> "closed to departure"
                    | OpenedToDeparture -> "opened to departure"

                sprintf "[%s; %s[ = Opened with %i of availibility at %.2f price and %s" start enD a p d
        
    temporal
    |> Outatime.merge
    |> Outatime.ofOption
    |> Outatime.toList 
    |> List.map request

[<Fact>]
let ``given temporaries with no intersections or empty periods expect the largest period with none value``()=
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
```
## Documentation
A complete documentation can be found here : http://cboudereau.github.io/Outatime/
