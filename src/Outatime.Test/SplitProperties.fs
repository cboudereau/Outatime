module SplitProperties

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

open FsCheck.Xunit
    
[<Arbitrary(typeof<TestData.RandomStringTemporal>)>]
module SplitTemporaries = 

    let splitPeriod = System.TimeSpan.FromDays(1000.)

    [<Property>]
    let ``check that all period are less than split period`` (temporaries:string Temporary list) = 
        temporaries
        |> Outatime.split splitPeriod
        |> Seq.forall(fun v -> v.Period |> duration <= splitPeriod)
