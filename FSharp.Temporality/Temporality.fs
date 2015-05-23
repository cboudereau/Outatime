module Temporality

type DateTime = System.DateTime

type TimeSpan = System.TimeSpan

type Period = 
    { StartDate : DateTime
      Duration : TimeSpan }
    member this.EndDate = (this.StartDate + this.Duration)
    
    static member private order first second = 
        if first.StartDate <= second.StartDate then (first, second)
        else (second, first)

    static member Intersect first second = 
        let (f, s) = Period.order first second
        match f.EndDate >= s.StartDate with
        | true -> 
            let startDate = max s.StartDate f.StartDate
            let endDate = min s.EndDate f.EndDate
            Some { StartDate = startDate
                   Duration = endDate - startDate }
        | false -> None

    static member Union first second = 
        let (f, s) = Period.order first second
        let startDate = min s.StartDate f.StartDate
        let endDate = max s.EndDate f.EndDate

        match Period.Intersect f s with
        | Some _ -> Some { StartDate = startDate; Duration = endDate - startDate }
        | None -> None
        
    override this.ToString() = sprintf "[%A;%A)" this.StartDate this.EndDate

let forNDays n = TimeSpan.FromDays(float n)
let forOneDay = forNDays 1
let forNever = forNDays 0

let Always = 
    { StartDate = DateTime.MinValue
      Duration = TimeSpan.MaxValue }

let Never = 
    { StartDate = DateTime.MinValue
      Duration = forNever }

type Temporary<'a when 'a : equality and 'a : comparison> = 
    { Period : Period
      Value : 'a }
    override this.ToString() = sprintf "%A (%A)" this.Period this.Value

    static member Intersect first second = 
        match first.Value = second.Value, first.Period |> Period.Intersect second.Period with
        | true, Some p -> Some { first with Period = p}
        | _ -> None

    static member Union first second = 
        match first.Value = second.Value, Period.Union first.Period second.Period with
        | true, Some p -> Some { first with Period = p }
        | _ -> None

type Temporal<'a when 'a : equality and 'a : comparison> = 
    { Values : Temporary<'a> list }
    override this.ToString() = sprintf "%A" this.Values

let toTemporal temporaries = 
    { Values = 
        temporaries 
        |> Seq.sortBy(fun t -> t.Period.StartDate)
        |> Seq.toList }

let split length temporal = 
    let rec internalSplit temporary = 
        seq { 
            if (temporary.Period.Duration <= length) then yield temporary
            else 
                yield { temporary with Period = { temporary.Period with Duration = length } }
                yield! internalSplit { temporary with Period = { temporary.Period with Duration = length } }
        }
    temporal.Values
    |> Seq.collect internalSplit
    |> toTemporal

let merge temporal = 
    let rec internalMerge (temporaries:Temporary<_> list) = 
        seq {
            match temporaries with
            | t1 :: t2 :: tail -> 
                let union = Temporary<_>.Union t1 t2

                if(union.IsSome) then yield! internalMerge (union.Value :: tail)
                else 
                    yield t1
                    yield! internalMerge (t2 :: tail)
            | [t1] -> yield t1
            | [] -> yield! []
        }
        
    internalMerge temporal.Values
    |> toTemporal