# FSharp.Temporality [![Build status](https://ci.appveyor.com/api/projects/status/ejj6vrx6x69aojey?svg=true)](https://ci.appveyor.com/project/cboudereau/fsharp-temporality)

When you have to deal with price, restrictions or all kind of values changing over the time, it becomes difficult to reduce the amount of data of your API If you don't have a strategy to merge the values.

## Maths at Rescue! What are the common value over the time to avoid data repetition
FSharp.Temporality has differents principles in order to have the correct design and algorithms : 
Period based on Mathematics Interval applied to Date with a Start Date and an End Date. The Interval is Half Open, then the period from 01/01 to 02/01 has one day duration.
Temporary : Is the value at a Period
Temporal : Is the scope of temporaries and the unit of work of all algorithm. Temporals represent all the values over all periods.
To build a temporal : just create a Temporary list and then call the Temporal.toTemporal
 
## Features
### Merge
When value are equals and period intersects, then the temporary is merged and number of temporaries on a temporal decrease. Eg Given a value "toto" on Given periods 01/01 -> 02/01; 02/01 -> 03/01, then the corresponding merge is "toto" on Given periods 01/01 -> 03/01. Sample : https://github.com/cboudereau/FSharp.Temporality/blob/master/FSharp.Temporality.Test/TemporalMergeProperties.fs

	```FSharp
	open Temporality
	open FsUnit.Xunit

	let Given v = v
	let When f v = 
		v 
		|> Temporal.toTemporal 
		|> f 
		|> Temporal.temporaries

	let Then check expected actual = 
		check (expected |> Temporal.toTemporal |> Temporal.temporaries) actual

	let shouldEqual = should equal

	let jan15 n = (DateTime(2015,1,n))

	[<Xunit.Fact>]
	let ``simple merge test``()=
		Given 
			[ Period.from (jan15 01) (jan15 02) |> Temporary.create "Hello"
			  Period.from (jan15 02) (jan15 05) |> Temporary.create "Hello"
			  Period.from (jan15 05) (jan15 10) |> Temporary.create "World"
			  Period.from (jan15 10) (jan15 20) |> Temporary.create "World" ]
		|> When Temporal.merge
		|> Then shouldEqual
			[ Period.from (jan15 01) (jan15 05) |> Temporary.create "Hello"
			  Period.from (jan15 05) (jan15 20) |> Temporary.create "World" ]
	```

### Split
When need to crop to a period.. Sample : https://github.com/cboudereau/FSharp.Temporality/blob/master/FSharp.Temporality.Test/TemporalSplitProperties.fs

	```FSharp
		
	open FsUnit.Xunit
	open Temporality

	let Given v = v
	let When f v = 
		v 
		|> Temporal.toTemporal 
		|> f 
		|> Temporal.temporaries

	let Then check expected actual = 
		check (expected |> Temporal.toTemporal |> Temporal.temporaries) actual

	let shouldEqual = should equal

	let jan15 n = (DateTime(2015,1,n))

	[<Xunit.Fact>]
	let ``simple split test``()=
		Given 
			[ Period.from (jan15 01) (jan15 11) |> Temporary.create "HelloWorld" ]
		|> When (TimeSpan.FromDays(5.) |> Temporal.split)
		|> Then shouldEqual
			[ Period.from (jan15 01) (jan15 06) |> Temporary.create "HelloWorld"
			  Period.from (jan15 06) (jan15 11) |> Temporary.create "HelloWorld" ]
	```
	
### View
Given a period and get the corresponding temporaries. period = f(value). https://github.com/cboudereau/FSharp.Temporality/blob/master/FSharp.Temporality.Test/TemporalViewProperties.fs

## QA
### Writen with property based testing driven dev
FSharp.Temporality is fully written with property based testing and FSCheck https://github.com/fscheck/FsCheck
All properties are based on Interval Mathematics principles : https://github.com/cboudereau/FSharp.Temporality/blob/master/FSharp.Temporality.Test/PeriodProperties.fs
