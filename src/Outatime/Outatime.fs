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
    { startDate : DateTime
      endDate : DateTime }
    override this.ToString() = 
        let toString (date:DateTime) = date.ToString("yyyy/MM/dd")
        sprintf "[%s; %s[" (this.startDate |> toString) (this.endDate |> toString)

let infinite = { startDate=DateTime.MinValue; endDate=DateTime.MaxValue }
let duration p = p.endDate - p.startDate

let sortP f p1 p2 = 
        if p1.startDate <= p2.startDate then f p1 p2
        else f p2 p1

let isEmpty p = p.startDate = p.endDate

let intersect p1 p2 = 
    let intersect p1 p2 =
        let i =
            { startDate = max p1.startDate p2.startDate
              endDate = min p1.endDate p2.endDate }

        match i |> isEmpty, p1.endDate >= p2.startDate with
        | true, _ | _, false -> None
        | _ -> Some i
     
    sortP intersect p1 p2

let union p1 p2 = 
    let union p1 p2 = 
        let u =
            { startDate = min p1.startDate p2.startDate
              endDate = max p1.endDate p2.endDate }
        
        match u |> isEmpty, p1.endDate >= p2.startDate with
        | true, _ | _, false -> None
        | _ -> Some u
    
    sortP union p1 p2

type Temporary<'a> = 
    { period : Period
      value : 'a }
    override this.ToString() = sprintf "%O = %O" this.period this.value

let (=>) startDate endDate = 
    { startDate = startDate
      endDate = endDate }

let (:=) period value = { period=period; value=value }

let sort temporaries = temporaries |> Seq.sortBy (fun t -> t.period.startDate, t.period.endDate)
let option temporaries = 
    let option t = { period=t.period; value = Some t.value } 
    temporaries |> Seq.map option

let clamp period temporaries = 
    
    let clamp state temporary = 
        match intersect period temporary.period with
        | Some i -> seq { yield! state; yield { period=i; value=temporary.value } }
        | None -> state

    temporaries |> Seq.fold clamp Seq.empty

let split length temporaries = 
    let rec split t = 
        seq{
            if t.period |> duration <= length then yield t
            else
                let next = t.period.startDate + length
                yield { t with period = { t.period with endDate = next } }
                yield! split { t with period = { t.period with startDate = next } }
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
                match intersect previous.period current.period  with
                | Some _ -> seq { yield current }
                | None -> 
                    seq{
                        let period = { startDate=previous.period.endDate; endDate=current.period.startDate }
                        if isEmpty period |> not then yield { period=period;value=None }
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
        | [] -> { period={ startDate = period.startDate; endDate=period.endDate}; value=None } |> Seq.singleton
        | temporaries ->
            seq{
                let head = temporaries |> Seq.head
                let last = temporaries |> Seq.last

                if head.period.startDate <> period.startDate 
                then yield { period={ startDate=period.startDate; endDate=head.period.startDate }; value=None }
                yield! temporaries
                if last.period.endDate <> period.endDate
                then yield { period={ startDate=last.period.endDate; endDate=period.endDate }; value=None }
            }

    temporaries |> contiguousO |> foreverO

let defaultToNone period = option >> defaultToNoneO period

let merge temporaries = 

    let union t1 t2 = 
        match t1.value = t2.value, union t1.period t2.period with
        | false, _ 
        | _, None -> None
        | true, Some p -> Some { period=p; value=t1.value }

    let rec merge temporaries = 
        seq{
            match temporaries with
            | t1::t2::tail ->
                match union t1 t2 with
                | Some u -> yield! merge (u::tail)
                | None -> yield t1; yield! merge (t2::tail)
            | [] -> yield! Seq.empty
            | [t] -> yield t
        }
    temporaries |> Seq.toList |> merge

let map f temporaries = 
    temporaries 
    |> sort
    |> merge
    |> Seq.map(fun t -> t.period := f t.value)
    |> contiguous

let apply tfs tvs = 
    let sortedv = tvs |> sort |> merge |> Seq.toList
    let defaultedv = sortedv |> defaultToNone infinite
    let apply tf = 
        let intersect state tv = 
            match intersect tf.period tv.period, tf.value, tv.value with
            | Some i, Some f, Some v -> seq { yield! state; yield i := Some (f v) }
            | _ -> state
                
        defaultedv |> Seq.fold intersect Seq.empty

    let applied = tfs |> defaultToNoneO infinite |> Seq.collect apply
    
    let allPeriods = 
        let p t = t.period
        seq { yield! sortedv |> Seq.map p; yield! tfs |> Seq.map p }
    
    let largestPeriod periods = 
        if periods |> Seq.isEmpty then None
        else Some (periods |> Seq.map (fun p -> p.startDate) |> Seq.min => (periods |> Seq.map(fun p -> p.endDate) |> Seq.max))

    match largestPeriod allPeriods, applied |> Seq.isEmpty with
    | Some p, false -> applied |> defaultToNoneO p
    | Some p, true -> seq { yield p := None }
    | _ -> applied

let (<!>) = map
let (<*>) = apply
