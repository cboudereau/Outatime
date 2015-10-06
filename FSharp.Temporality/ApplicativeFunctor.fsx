type TimeSpan = System.TimeSpan

type DateTime = System.DateTime

type Period = 
    { startDate : DateTime
      endDate : DateTime }
    override this.ToString() = 
        let toString (date:DateTime) = date.ToString("yyyy/MM/dd")
        sprintf "[%s; %s[" (this.startDate |> toString) (this.endDate |> toString)
    
    static member infinite = { startDate=DateTime.MinValue; endDate=DateTime.MaxValue }
    static member duration p = p.endDate - p.startDate

    static member sort f p1 p2 = 
        if p1.startDate <= p2.startDate then f p1 p2
        else f p2 p1

    static member intersect p1 p2 = 
        let intersect p1 p2 =
            let i =                 
                { startDate = max p1.startDate p2.startDate
                  endDate = min p1.endDate p2.endDate }

            match i.startDate = i.endDate, p1.endDate >= p2.startDate with
            | true, _ | _, false -> None
            | _ -> Some i       
         
        Period.sort intersect p1 p2

    static member union p1 p2 = 
        let union p1 p2 = 
            if p1.endDate >= p2.startDate
            then 
                { startDate = min p1.startDate p2.startDate
                  endDate = max p1.endDate p2.endDate }
                |> Some
            else None
        Period.sort union p1 p2

type Temporary<'a> = 
    { period : Period
      value : 'a }
    override this.ToString() = sprintf "%O = %O" this.period this.value

let (=>) startDate endDate = 
    { startDate = startDate
      endDate = endDate }

let (:=) period value = { period=period; value=value }

let sort temporaries = temporaries |> Seq.sortBy (fun t -> t.period.startDate)
let option t = { period=t.period; value = Some t.value }

let clamp period temporaries = 
    
    let clamp state temporary = 
        match Period.intersect period temporary.period with
        | Some i -> seq { yield! state; yield { period=i; value=temporary.value } }
        | None -> state

    temporaries |> Seq.fold clamp Seq.empty

let split length temporaries = 
    let rec split t = 
        seq{
            if t.period |> Period.duration <= length then yield t
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
                match Period.intersect previous.period current.period  with
                | Some _ -> seq { yield current }
                | None -> 
                    seq{
                        yield { period={startDate=previous.period.endDate; endDate=current.period.startDate};value=None }
                        yield current
                    }
        defaulted, Some current
    temporaries
    |> Seq.mapFold folder None
    |> fst
    |> Seq.collect it

let contiguous temporaries = temporaries |> Seq.map option |> contiguousO 

let foreverO temporaries = 
    match temporaries |> Seq.toList with
    | [] -> { period={ startDate = DateTime.MinValue; endDate=DateTime.MaxValue}; value=None } |> Seq.singleton
    | temporaries ->
        seq{
            let head = temporaries |> Seq.head
            let last = temporaries |> Seq.last

            if head.period.startDate <> DateTime.MinValue 
            then yield { period={ startDate=DateTime.MinValue; endDate=head.period.startDate }; value=None }
            yield! temporaries
            if last.period.endDate <> DateTime.MaxValue
            then yield { period={ startDate=last.period.endDate; endDate=DateTime.MaxValue }; value=None }
        }

let forever temporaries = temporaries |> Seq.map option |> foreverO

let defaultToNoneO temporaries = temporaries |> contiguousO |> foreverO

let defaultToNone temporaries = temporaries |> Seq.map option |> defaultToNoneO

let merge temporaries = 

    let union t1 t2 = 
        match t1.value = t2.value, Period.union t1.period t2.period with
        | false, _ | _, None -> None
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
    |> defaultToNone 
    |> Seq.map(fun t -> t.period := f t.value)

let apply tfs tvs = 
    let sortedv = tvs |> sort |> merge |> defaultToNone

    let apply tf = 
        let intersect tv = 
            match Period.intersect tf.period tv.period with
            | Some i -> {period=i; value = tf.value tv.value} |> Seq.singleton
            | _ -> Seq.empty

        sortedv |> Seq.collect intersect

    tfs |> Seq.collect apply
        
let (<!>) = map
let (<*>) = apply

//Test
let utcDate y m d = DateTime(y, m, d, 0, 0, 0, System.DateTimeKind.Utc)
let d2015 = utcDate 2015
let jan15 = d2015 1
let feb15 = d2015 2

let print source = source |> Seq.iter (printfn "%O")

//split
[ jan15 10 => jan15 22 := "Hello" ]
|> split (TimeSpan.FromDays(5.))
|> print

let availability close closeToDeparture price = (close, closeToDeparture, price)

let contiguousSample = 
    availability
    <!> [ jan15 2 => jan15 5 := false; jan15 5 => jan15 20 := true ]
    <*> [ jan15 2 => jan15 19 := false; jan15 1 => jan15 2 := true ]
    <*> [ jan15 2 => jan15 22 := 120m ]

contiguousSample |> print

let mergedSample = 
    availability
    <!> [ jan15 2 => jan15 5 := false
          jan15 5 => jan15 7 := true
          jan15 7 => jan15 20 := true ]
    <*> [ jan15 2 => jan15 19 := false; jan15 1 => jan15 2 := true ]
    <*> [ jan15 2 => jan15 22 := 120m ]

mergedSample |> print

//Smoke test
contiguousSample |> Seq.toList = (mergedSample |> Seq.toList)

//only map
let price p = p
price
<!> [ jan15 4 => jan15 5 := 150m; jan15 5 => jan15 20 := 165m ] 
|> print

//map and apply
let actual2 = 
    availability
    <!> [ jan15 4 => jan15 5 := false; jan15 5 => jan15 20 := true ]
    <*> [ jan15 2 => jan15 15 := false
          jan15 15 => jan15 16 := true
          jan15 16 => jan15 18 := false
          jan15 18 => jan15 23 := true ]
    <*> [ jan15 1 => jan15 22 := 120m ]
actual2 |> print

actual2 
|> clamp (jan15 3 => jan15 7)
|> print
