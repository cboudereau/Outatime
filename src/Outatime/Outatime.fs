module Outatime

module Partial = 
    type Partial<'a> = 
        | Applied of 'a
        | Defaulted of 'a
    
    let unlift = function
        | Applied v
        | Defaulted v -> v 

    let liftf f = function
        | Some v -> Partial.Applied (v |> Some |> f) 
        | None -> Partial.Defaulted (None |> f)
    
    let combine f first second = 
        match first, second with
        | Partial.Applied f', Some s'
        | Partial.Defaulted f', Some s' -> Partial.Applied (f f' (Some s'))
        | Partial.Applied f', None -> Partial.Applied (f f' None)
        | Partial.Defaulted f', None -> Partial.Defaulted (f f' None)

module Partials = 
    let unlift partials = partials |> Seq.map Partial.unlift    
    let ltrim partials = 
        let ltrim state p = 
            seq {
                match state |> Seq.isEmpty, p with
                | true, Partial.Applied a -> yield Partial.Applied a
                | true, Partial.Defaulted _ -> yield! state
                | _, i -> yield! state; yield i }
        partials |> Seq.fold ltrim Seq.empty

    let rtrim partials = 
        let rtrim p state = 
            seq {
                match state |> Seq.isEmpty, p with
                | true, Partial.Applied a -> yield Partial.Applied a
                | true, Partial.Defaulted _ -> yield! Seq.empty
                | _, i -> yield i; yield! state }
        Seq.empty |> Seq.foldBack rtrim partials

    let trim partials = partials |> ltrim |> rtrim

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

let merge temporaries = 

    let union t1 t2 = 
        match t1.Value = t2.Value, union t1.Period t2.Period with
        | false, _ 
        | _, None -> None
        | true, Some p -> Some { Period=p; Value=t1.Value }

    let rec merge temporaries = 
        seq{
            match temporaries with
            | t1::t2::tail ->
                match union t1 t2 with
                | Some u -> yield! merge (u::tail)
                | None -> yield t1; yield! merge (t2::tail)
            | [t] -> yield t
            | [] -> yield! Seq.empty
        }
    temporaries |> Seq.toList |> merge

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

let (<!>) = map
let (<*>) = apply
let (<*?>) = applyf