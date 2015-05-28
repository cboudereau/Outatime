module Temporality

type DateTime = System.DateTime

type TimeSpan = System.TimeSpan

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TimeSpan = 
    [<CompiledName("ForNDays")>]
    let forNDays n = TimeSpan.FromDays(float n)
    [<CompiledName("ForOneDay")>]
    let forOneDay = forNDays 1
    [<CompiledName("ForNever")>]
    let forNever = TimeSpan.Zero
    [<CompiledName("ForEver")>]
    let forEver = DateTime.MaxValue - DateTime.MinValue

type Period = 
    { StartDate : DateTime
      EndDate : DateTime }
    member this.Duration = (this.EndDate - this.StartDate)
    static member Always = { StartDate = DateTime.MinValue; EndDate = DateTime.MaxValue }
    static member Never = { StartDate = DateTime.MinValue; EndDate = DateTime.MinValue }

    override this.ToString() = 
        match this with
        | p when p = Period.Always -> sprintf "Always"
        | p when p = Period.Never -> sprintf "Never"
        | p -> sprintf "[%A, %A)" p.StartDate p.EndDate

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Period = 
    let private sort first second = 
        if first.StartDate <= second.StartDate then (first, second)
        else (second, first)
    
    [<CompiledName("From")>]
    let from startDate duration = { StartDate = startDate; EndDate = startDate + duration }

    [<CompiledName("Intersect")>]
    let intersect first second = 
        let (f, s) = sort first second
        match f.EndDate >= s.StartDate with
        | true -> 
            let startDate = max s.StartDate f.StartDate
            let endDate = min s.EndDate f.EndDate
            Some { StartDate = startDate
                   EndDate = endDate }
        | false -> None
    
    [<CompiledName("Union")>]
    let union first second = 
        let (f, s) = sort first second
        let startDate = min s.StartDate f.StartDate
        let endDate = max s.EndDate f.EndDate
        match intersect f s with
        | Some _ -> 
            Some { StartDate = startDate
                   EndDate = endDate }
        | None -> None

type Temporary<'a when 'a : equality> = 
    { Period : Period
      Value : 'a }
    override this.ToString() = sprintf "%O : %A" this.Period this.Value

module Temporary = 
    [<CompiledName("Intersect")>]
    let intersect first second = 
        match first.Value = second.Value, first.Period |> Period.intersect second.Period with
        | true, Some p -> Some { first with Period = p }
        | _ -> None
    
    [<CompiledName("Union")>]
    let union first second = 
        match first.Value = second.Value, Period.union first.Period second.Period with
        | true, Some p -> Some { first with Period = p }
        | _ -> None

type Temporal<'a when 'a : equality> = Temporary<'a> list

module Temporal = 
    let toTemporal temporaries = 
        temporaries
        |> Seq.sortBy (fun t -> t.Period.StartDate)
        |> Seq.toList
    
    let view period temporal = 
        temporal 
        |> List.filter(fun t -> (t.Period |> Period.intersect period).IsSome)

    let split length temporal = 
        let rec internalSplit temporary = 
            seq { 
                if (temporary.Period.Duration <= length) then yield temporary
                else 
                    let next = temporary.Period.StartDate + length
                    yield { temporary with Period = { temporary.Period with EndDate = next } }
                    yield! internalSplit { temporary with Period = { temporary.Period with StartDate = next } }
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
