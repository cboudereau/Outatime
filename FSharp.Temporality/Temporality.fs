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
    static member Infinite = { StartDate = DateTime.MinValue; EndDate = DateTime.MaxValue }
    static member Empty d = { StartDate = d; EndDate = d }
    static member isInfinite (p:Period) = p = Period.Infinite
    static member isEmpty (p:Period) = p.Duration = TimeSpan.forNever

    override this.ToString() = 
        let datef (d:DateTime) = d.ToString(System.Globalization.CultureInfo.InvariantCulture)
        match this with
        | p when Period.isInfinite p -> sprintf "Infinite"
        | p when Period.isEmpty p -> sprintf "Empty %s" (datef p.StartDate)
        | p -> sprintf "[%s, %s)" (datef p.StartDate) (datef p.EndDate)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Period = 
    let (|Infinite|_|) p = if Period.isInfinite p then Some p else None
    let (|Empty|_|) p = if Period.isEmpty p then Some p else None
    
    let private sort first second = 
        if first.StartDate <= second.StartDate then (first, second)
        else (second, first)
    
    [<CompiledName("From")>]
    let from startDate duration = { StartDate = startDate; EndDate = startDate + duration }

    let private maybeContiguous p1 p2 = 
        let (f,s) = sort p1 p2
        if f.EndDate >= s.StartDate 
        then Some (f,s)
        else None

    [<CompiledName("Intersect")>]
    let intersect first second = 
        match maybeContiguous first second with
        | None -> Period.Empty first.EndDate
        | Some(s, f) -> 
            let startDate = max s.StartDate f.StartDate
            let endDate = min s.EndDate f.EndDate
            { StartDate = startDate
              EndDate = endDate }
    
    [<CompiledName("Union")>]
    let union first second = 
        match maybeContiguous first second with
        | None -> Period.Empty first.EndDate
        | Some (s, f) -> 
            let startDate = min s.StartDate f.StartDate
            let endDate = max s.EndDate f.EndDate
            { StartDate = startDate
              EndDate = endDate }

type Temporary<'a> = 
    { Period : Period
      Value : 'a }
    override this.ToString() = sprintf "%O : %A" this.Period this.Value

module Temporary = 
    [<CompiledName("Create")>]
    let create value period = { Period = period; Value = value }
    
    [<CompiledName("Intersect")>]
    let intersect first second = 
        match first.Value = second.Value, first.Period |> Period.intersect second.Period with
        | _, Period.Empty _ | false, _ -> None
        | true, p -> Some { first with Period = p }
    
    [<CompiledName("Union")>]
    let union first second = 
        match first.Value = second.Value, Period.union first.Period second.Period with
        | _, Period.Empty _ | false, _ -> None
        | true, p -> Some { first with Period = p }

type Temporal<'a when 'a : equality> = 
    { Values: Temporary<'a> list }

module Temporal = 
    [<CompiledName("ToTemporal")>]
    let toTemporal temporaries = 
        let sortedTemporaries = 
            temporaries
            |> Seq.sortBy (fun t -> t.Period.StartDate)
            |> Seq.filter (fun t -> t.Period |> Period.isEmpty |> not)
            |> Seq.toList
        { Values = sortedTemporaries }
    
    let temporaries temporal = temporal.Values

    [<CompiledName("View")>]
    let view<'a when 'a : equality> (period:'a -> Period) (temporal:Temporal<'a>) = 
        temporal.Values
        |> Seq.map(fun t -> { t with Period = t.Period |> Period.intersect (period t.Value) })
        |> toTemporal

    [<CompiledName("Clamp")>]
    let clamp<'a when 'a : equality> period temporal = view<'a> (fun _ -> period) temporal

    [<CompiledName("Split")>]
    let split length temporal = 
        let rec internalSplit temporary = 
            seq { 
                if (temporary.Period.Duration <= length) then yield temporary
                else 
                    let next = temporary.Period.StartDate + length
                    yield { temporary with Period = { temporary.Period with EndDate = next } }
                    yield! internalSplit { temporary with Period = { temporary.Period with StartDate = next } }
            }
        temporal.Values
        |> Seq.collect internalSplit
        |> toTemporal
    
    [<CompiledName("Merge")>]
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
        internalMerge temporal.Values |> toTemporal

    [<CompiledName("Zip")>]
    let zip (t1, t2) = 
        t1.Values
        |> Seq.collect(
            fun first -> 
                let clampFirst second = { Period = second.Period; Value = (first.Value, second.Value) }
                t2 |> clamp first.Period
                |> temporaries
                |> Seq.map(clampFirst))
        |> toTemporal