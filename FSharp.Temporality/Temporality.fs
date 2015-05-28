module Temporality

type DateTime = System.DateTime

type TimeSpan = System.TimeSpan

let forNDays n = TimeSpan.FromDays(float n)
let forOneDay = forNDays 1
let forNever = forNDays 0
let forEver = TimeSpan.MaxValue

type Period = 
    { StartDate : DateTime
      Duration : TimeSpan }
    member this.EndDate = (this.StartDate + this.Duration)
    override this.ToString() = 
        match this with
        | p when p.StartDate = DateTime.MinValue && p.Duration = forEver -> sprintf "always"
        | p when p.StartDate = DateTime.MinValue && p.Duration = forNever -> sprintf "never"
        | p -> sprintf "[%A, %A)" p.StartDate p.EndDate

let Always = 
    { StartDate = DateTime.MinValue
      Duration = forEver }

let Never = 
    { StartDate = DateTime.MinValue
      Duration = forNever }

module Interval = 
    let private order first second = 
        if first.StartDate <= second.StartDate then (first, second)
        else (second, first)
    
    [<CompiledName("Intersect")>]
    let intersect first second = 
        let (f, s) = order first second
        match f.EndDate >= s.StartDate with
        | true -> 
            let startDate = max s.StartDate f.StartDate
            let endDate = min s.EndDate f.EndDate
            Some { StartDate = startDate
                   Duration = endDate - startDate }
        | false -> None
    
    [<CompiledName("Union")>]
    let union first second = 
        let (f, s) = order first second
        let startDate = min s.StartDate f.StartDate
        let endDate = max s.EndDate f.EndDate
        match intersect f s with
        | Some _ -> 
            Some { StartDate = startDate
                   Duration = endDate - startDate }
        | None -> None

type Temporary<'a when 'a : equality> = 
    { Period : Period
      Value : 'a }
    override this.ToString() = sprintf "%O : %A" this.Period this.Value

module Temporary = 
    [<CompiledName("Intersect")>]
    let intersect first second = 
        match first.Value = second.Value, first.Period |> Interval.intersect second.Period with
        | true, Some p -> Some { first with Period = p }
        | _ -> None
    
    [<CompiledName("Union")>]
    let union first second = 
        match first.Value = second.Value, Interval.union first.Period second.Period with
        | true, Some p -> Some { first with Period = p }
        | _ -> None

type Temporal<'a when 'a : equality> = Temporary<'a> list

module Temporal = 
    let toTemporal temporaries = 
        temporaries
        |> Seq.sortBy (fun t -> t.Period.StartDate)
        |> Seq.toList
    
    let split length temporal = 
        let rec internalSplit temporary = 
            seq { 
                if (temporary.Period.Duration <= length) then yield temporary
                else 
                    yield { temporary with Period = { temporary.Period with Duration = length } }
                    yield! internalSplit { temporary with Period = { temporary.Period with Duration = length } }
            }
        temporal
        |> Seq.collect internalSplit
        |> toTemporal
    
    let merge temporal = 
        let rec internalMerge temporaries = 
            seq { 
                match temporaries with
                | t1 :: t2 :: tail -> 
                    let union = Temporary.union t1 t2
                    if (union.IsSome) then yield! internalMerge (union.Value :: tail)
                    else 
                        yield t1
                        yield! internalMerge (t2 :: tail)
                | [ t1 ] -> yield t1
                | [] -> yield! []
            }
        internalMerge temporal |> toTemporal
