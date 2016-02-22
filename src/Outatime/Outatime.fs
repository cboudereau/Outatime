module Outatime

type DateTime = System.DateTime

type TimeSpan = System.TimeSpan

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TimeSpan = 
    let forNDays n = TimeSpan.FromDays(float n)
    let forOneDay = forNDays 1
    let forNever = TimeSpan.Zero
    let forEver = DateTime.MaxValue - DateTime.MinValue

type Period = 
    { StartDate : DateTime
      EndDate : DateTime }
    override this.ToString() = 
        let toString (date:DateTime) = date.ToString("yyyy/MM/dd")
        sprintf "[%s; %s[" (this.StartDate |> toString) (this.EndDate |> toString)

let infinite = { StartDate=DateTime.MinValue; EndDate=DateTime.MaxValue }
let duration p = p.EndDate - p.StartDate

let sortP f p1 p2 = 
        if p1.StartDate <= p2.StartDate then f p1 p2
        else f p2 p1

let isEmpty p = p.StartDate = p.EndDate

let intersect p1 p2 = 
    let intersect p1 p2 =
        let i =
            { StartDate = max p1.StartDate p2.StartDate
              EndDate = min p1.EndDate p2.EndDate }

        match i |> isEmpty, p1.EndDate >= p2.StartDate with
        | true, _ | _, false -> None
        | _ -> Some i
     
    sortP intersect p1 p2

let union p1 p2 = 
    let union p1 p2 = 
        let u =
            { StartDate = min p1.StartDate p2.StartDate
              EndDate = max p1.EndDate p2.EndDate }
        
        match u |> isEmpty, p1.EndDate >= p2.StartDate with
        | true, _ | _, false -> None
        | _ -> Some u
    
    sortP union p1 p2

type Temporary<'a> = 
    { Period : Period
      Value : 'a }
    override this.ToString() = sprintf "%O = %O" this.Period this.Value

type Temporaries<'a> = Temporary<'a> seq

let (=>) startDate endDate = 
    { StartDate = startDate
      EndDate = endDate }

let (:=) period value = { Period=period; Value=value }

let sort temporaries = temporaries |> Seq.sortBy (fun t -> t.Period.StartDate, t.Period.EndDate)
let option temporaries = 
    let option t = t.Period := Some t.Value
    temporaries |> Seq.map option

let clamp period temporaries = 
    
    let clamp state temporary = 
        match intersect period temporary.Period with
        | Some i -> seq { yield! state; yield { Period=i; Value=temporary.Value } }
        | None -> state

    temporaries |> Seq.fold clamp Seq.empty

let split length temporaries = 
    let rec split t = 
        seq{
            if t.Period |> duration <= length then yield t
            else
                let next = t.Period.StartDate + length
                yield { t with Period = { t.Period with EndDate = next } }
                yield! split { t with Period = { t.Period with StartDate = next } }
        }
    temporaries
    |> Seq.collect split

let contiguousO temporaries = 
    let it i = i

    let folder state current = 
        let defaulted = 
            match state with
            | None -> current |> Seq.singleton
            | Some previous -> 
                match intersect previous.Period current.Period  with
                | Some _ -> seq { yield current }
                | None -> 
                    seq{
                        let period = { StartDate=previous.Period.EndDate; EndDate=current.Period.StartDate }
                        if isEmpty period |> not then yield period := None
                        yield current
                    }
        defaulted, Some current
    temporaries
    |> Seq.mapFold folder None
    |> fst
    |> Seq.collect it

let contiguous temporaries = temporaries |> option |> contiguousO 

let defaultToNoneO period temporaries = 
    let foreverO temporaries = 
        match temporaries |> Seq.toList with
        | [] -> { Period={ StartDate = period.StartDate; EndDate=period.EndDate}; Value=None } |> Seq.singleton
        | temporaries ->
            seq{
                let head = temporaries |> Seq.head
                let last = temporaries |> Seq.last

                if head.Period.StartDate <> period.StartDate 
                then yield { Period={ StartDate=period.StartDate; EndDate=head.Period.StartDate }; Value=None }
                yield! temporaries
                if last.Period.EndDate <> period.EndDate
                then yield { Period={ StartDate=last.Period.EndDate; EndDate=period.EndDate }; Value=None }
            }

    temporaries |> contiguousO |> foreverO

let defaultToNone period = option >> defaultToNoneO period

let (|Always|_|) p = if p = infinite then Some p else None

let (|Equals|_|) p1 p2 = if p1 = p2 then Some p1 else None

let (|Exists|Empty|) p = 
    if p |> isEmpty then Empty
    else Exists p

let (|Intersect|_|) p1 p2 =
    let intersect p1 p2 = 
        let i =
            { StartDate = max p1.StartDate p2.StartDate
              EndDate = min p1.EndDate p2.EndDate }
        match i, p1.EndDate >= p2.StartDate with
        | Exists _, true -> Some i
        | _ -> None
    sortP intersect p1 p2

let (|Union|_|) p1 p2 =
    let union p1 p2 = 
        let u =
            { StartDate = min p1.StartDate p2.StartDate
              EndDate = max p1.EndDate p2.EndDate }

        match u |> isEmpty, p1.EndDate >= p2.StartDate with
        | false, true -> Some u
        | _ -> None
    sortP union p1 p2

let (|Merged|_|) t1 t2 = 
    match t1.Value = t2.Value, t1.Period with
    | true, Union t2.Period p -> Some { Period=p; Value=t1.Value }
    | _ -> None

let merge temporaries = 
    let enumerator = (temporaries:#seq<_>).GetEnumerator()

    let rec merge previous = 
        seq {
            match enumerator.MoveNext(), previous with
            | true, None -> yield! enumerator.Current |> Some |> merge
            | true, Some t1 -> 
                let t2 = enumerator.Current
                match t2 with
                | Merged t1 u -> yield! u |> Some |> merge
                | _ -> yield t1; yield! t2 |> Some |> merge
            | false, last -> 
                enumerator.Dispose()
                match last with
                | Some t -> yield t;
                | None -> yield! Seq.empty }
    merge None

let private liftf f temporaries = temporaries |> Seq.map (fun t -> Partial.liftf (f t.Period) t.Value)

let map f temporaries = 
    let liftedf period v = period := (f v)
    
    temporaries
    |> sort
    |> merge
    |> defaultToNone infinite
    |> liftf liftedf

let apply tfs tvs = 
    
    let defaultedv = tvs |> sort |> merge |> defaultToNone infinite |> Seq.toList
    
    let unliftp t = 
        let p t = t.Period
        t |> Partial.unlift |> p
    
    let liftc period tf v = period := (tf.Value v)

    let combinef tf = 
        let combinev tv = Partial.combine (liftc tv.Period) tf tv.Value

        defaultedv 
        |> clamp (unliftp tf)
        |> Seq.map combinev

    tfs |> Seq.collect combinef

let applyf tfs tvs = apply tfs tvs |> Partials.trim |> Partials.unlift |> merge

let traverse map = 
    let transpose map = 
        map
        |> Map.toSeq
        |> Seq.collect (fun (k, temporaries) -> temporaries |> Seq.map(fun t -> t.Period := (k, t.Value)) |> sort)
    
    let minStart t = t |> Seq.map(fun ts -> ts.Period.StartDate) |> Seq.min
    let maxEnd t = t |> Seq.map(fun ts -> ts.Period.EndDate) |> Seq.max
    let aggregate state t =
        seq { 
            match state |> clamp t.Period with
            | i when i |> Seq.isEmpty -> 
                yield! state
                yield t.Period := (t.Value |> Seq.singleton)
            | i -> 
                let mins = state |> minStart
                let maxs = state |> maxEnd
                
                if t.Period.StartDate < mins then yield t.Period.StartDate => mins := (t.Value |> Seq.singleton)

                yield! state |> clamp (infinite.StartDate => (i |> minStart))

                yield! i |> Seq.map(fun ti -> ti.Period := seq { yield! ti.Value; yield t.Value })

                yield! state |> clamp (i |> maxEnd => infinite.EndDate)

                if t.Period.EndDate > maxs then yield maxs => t.Period.EndDate := (t.Value |> Seq.singleton)
         }
    
    map
    |> transpose
    |> Seq.fold aggregate Seq.empty
    |> Seq.map(fun t -> t.Period := (t.Value |> Map.ofSeq))
    |> merge

let (<!>) = map
let (<*>) = apply
let (<*?>) = applyf