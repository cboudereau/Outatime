# FSharp.Temporality [![Build status](https://ci.appveyor.com/api/projects/status/ejj6vrx6x69aojey?svg=true)](https://ci.appveyor.com/project/cboudereau/fsharp-temporality)

When you have to deal with price, restrictions or all kind of value changing accross time, it become difficult to reduce the amount of data of your API if you don't have a strategy to merge the values.

FSharp.Temporality has differents principles in order to have the correct design and algorithm : 
 > Period based on Mathematics Interval with a Start Date and an End Date. The Interval is Half Open, then the period from 01/01 to 02/01 has one day duration.
 > Temporary : Is the value at a Period
 > Temporal : Is the scope of temporaries and the unit of work of all algorithm. Temporals represent all the values accross all periods.
 > To build a temporal : just create a Temporary list and then call the Temporal.toTemporal
 
Features : 
 > merge : when value are equals and period intersects, then the temporary is merged and number of temporaries on a temporal decrease. Eg Given a value "toto" on Given periods 01/01 -> 02/01; 02/01 -> 03/01, then the corresponding merge is "toto" on Given periods 01/01 -> 03/01.
 > split : when need to crop to a period. period = f(value)
 > view : Given a period and get the corresponding temporaries
