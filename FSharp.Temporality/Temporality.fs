module Temporality

type DateTime = System.DateTime

type TimeSpan = System.TimeSpan

type Period = 
    { StartDate : DateTime
      EndDate : DateTime }
    with
        member this.DayLength = (this.EndDate - this.StartDate)
        override this.ToString() = sprintf "[%A;%A[" this.StartDate this.EndDate

let Always = { StartDate = DateTime.MinValue; EndDate = DateTime.MaxValue }

let forNDays n = TimeSpan.FromDays(float n)

let forOneDay = forNDays 1

type Temporary<'a when 'a : equality and 'a : comparison> = 
    { Period : Period
      Value : 'a }
    with
        override this.ToString() = sprintf "%A (%A)" this.Period this.Value

type Temporal<'a when 'a : equality and 'a : comparison> = 
    { Values : Temporary<'a> seq }
    with
        override this.ToString() = sprintf "%A" this.Values

let toTemporal temporaries = { Values = temporaries |> Seq.toList }

let split length temporal = 
    let rec internalSplit temporary = 
        seq{ 

            if(temporary.Period.DayLength <= length) then yield temporary
            else
                let next = temporary.Period.StartDate + length
                yield {temporary with Period = {temporary.Period with EndDate = next}}
                yield! internalSplit { temporary with Period = {temporary.Period with StartDate = next} }
            }
    temporal.Values
    |> Seq.collect internalSplit
    |> toTemporal
