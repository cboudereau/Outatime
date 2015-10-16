# Outatime [![Build status](https://ci.appveyor.com/api/projects/status/ejj6vrx6x69aojey?svg=true)](https://ci.appveyor.com/project/cboudereau/outatime)

When you have to deal with price, restrictions or all kind of values changing over the time, it becomes difficult to reduce the amount of data of your API If you don't have a strategy to merge the values.

## Maths at Rescue! What are the common value over the time to avoid data repetition
Outatime has differents principles in order to have the correct design and algorithms :

## Applicative functor at the rescue but it is promise it is not difficult it is an evidence!
```fsharp
module ApplicativeFunctorTests

open Bdd
open Xunit
open Outatime

let jan15 d = DateTime(2015, 1, d)

type Opening = Opened | Closed
type Departure = OpenedToDeparture | ClosedToDeparture
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
let ``given multiple temporaries, when apply a function on this temporaries then expect applied function on any intersection``()=
    let ``transform temporaries to rate availability domain`` opening departure availability price = 
        match opening with
        | (Opening.Opened) -> 
            RateAvailability.Opened 
                { departure=departure; availability=availability; price=price }
        | (Opening.Closed) -> 
            RateAvailability.Closed

    let ``transform temporaries into request`` temporaries = 
        let request t = 
            match t.value with
            | None -> 
                sprintf "%O = No Request (May be put a state monad here)" t.period
            | Some Closed -> 
                sprintf "%O = Closed" t.period
            | Some (Opened rate) -> 
                let (Availability a) = rate.availability
                let (Price p) = rate.price
                let d = 
                    match rate.departure with
                    | ClosedToDeparture -> "closed to departure"
                    | OpenedToDeparture -> "opened to departure"

                sprintf "%O = Opened with %i of availibility at %.2f price and %s" t.period a p d
            
        temporaries 
        |> Seq.map request
        |> Seq.toList

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
        [ "[2015/01/01; 2015/01/04[ = No Request (May be put a state monad here)"
          "[2015/01/04; 2015/01/05[ = Opened with 10 of availibility at 120.00 price and opened to departure"
          "[2015/01/05; 2015/01/15[ = Closed"
          "[2015/01/15; 2015/01/16[ = No Request (May be put a state monad here)"
          "[2015/01/16; 2015/01/18[ = Closed"
          "[2015/01/18; 2015/01/20[ = Closed"
          "[2015/01/20; 2015/01/23[ = No Request (May be put a state monad here)" ]
```

### Period 
Based on Mathematics Interval applied to Date with a Start Date and an End Date. The Interval is Half Open, then the period from 01/01 to 02/01 has one day duration.

### Temporary
Is the value at a Period

## Features
### Merge
When value are equals and period intersects, then the temporary is merged and number of temporaries on a temporal decrease. Eg Given a value "toto" on Given periods 01/01 -> 02/01; 02/01 -> 03/01, then the corresponding merge is "toto" on Given periods 01/01 -> 03/01. Sample : https://github.com/cboudereau/Outatime/blob/master/Outatime.Test/TemporalMergeProperties.fs

```fsharp
open Outatime
open Bdd
open Xunit

let jan15 n = (DateTime(2015,1,n))
let ``I want to merge temporaries`` = Outatime.merge >> Seq.toList

[<Fact>]
let ``given contiguous temporary expect a merged temporary``()=
    When ``I want to merge temporaries`` 
    |> With 
        [ jan15 01 => jan15 02 := "Hello"
          jan15 02 => jan15 05 := "Hello"
          jan15 05 => jan15 10 := "World"
          jan15 10 => jan15 20 := "World" ]
    |> Expect
        [ jan15 01 => jan15 05 := "Hello"
          jan15 05 => jan15 20 := "World" ]
```

### Split
When need to crop to a period..

```fsharp
open Outatime
open Xunit
open Bdd

let jan15 n = (DateTime(2015,1,n))
let days n = TimeSpan.FromDays(float n)

let ``I want to split temporaries`` days temporaries = Outatime.split days temporaries |> Seq.toList
let ``five days`` = days 5

[<Fact>]
let ``given temporaries for a large period when split for n days expect temporary with n day max period length``()=
    When ``I want to split temporaries`` 
    |> For ``five days``
    |> With [ jan15 01 => jan15 11 := "HelloWorld" ]
    |> Expect
        [ jan15 01 => jan15 06 := "HelloWorld"
          jan15 06 => jan15 11 := "HelloWorld" ]
```
	
### Clamp
Given a period and get the corresponding temporaries.

## QA
### Writen with property based testing driven dev
Outatime is fully written with property based testing and FSCheck https://github.com/fscheck/FsCheck
All properties are based on Interval Mathematics principles : https://github.com/cboudereau/Outatime/blob/master/Outatime.Test/PeriodProperties.fs
