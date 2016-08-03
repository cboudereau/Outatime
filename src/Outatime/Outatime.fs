module Outatime

type DateTime = System.DateTime

type TimeSpan = System.TimeSpan
    
type Interval<'t> = 
    { Start : 't
      End : 't }
    override this.ToString() = sprintf "[%O; %O[" this.Start this.End

type IntervalValued<'k, 'v> = 
    { Interval : Interval<'k>
      Value : 'v }
    override this.ToString() = sprintf "%O = %O" this.Interval this.Value

type IntervalValuedSet<'k, 'v> = private IntervalValuedSet of IntervalValued<'k, 'v> seq

let always = { Start=DateTime.MinValue; End=DateTime.MaxValue }

let ret x = 
    { Interval = always
      Value = x }
    |> Seq.singleton
    |> IntervalValuedSet

let (=>) s e = 
    { Start = s
      End = e }

let (:=) p v = 
    { Interval = p
      Value = v }

let lift f (IntervalValuedSet x) = x |> Seq.map(fun i -> i.Interval := f i.Value) |> IntervalValuedSet

let lift2 f (IntervalValuedSet x) (IntervalValuedSet y) = 
    seq {
        use xe = x.GetEnumerator()
        use ye = y.GetEnumerator()

        if(xe.MoveNext() && ye.MoveNext()) then
            let rec next () = 
                seq { 
                    if xe.Current.Interval.Start < ye.Current.Interval.End then
                        let start = max ye.Current.Interval.Start xe.Current.Interval.Start
                        let enD = min ye.Current.Interval.End xe.Current.Interval.End
                        if start < enD then yield (start => enD := (f xe.Current.Value ye.Current.Value))
                    let n = if ye.Current.Interval.End < xe.Current.Interval.End then ye.MoveNext() else xe.MoveNext()
                    if n then yield! next () }
            yield! next ()
    } |> IntervalValuedSet

let apply f x = lift2 (fun f x -> f x) f x

let map f x = apply (ret f) x

let private contiguousT zero f (IntervalValuedSet temporaries) = 
    seq { 
        use e = temporaries.GetEnumerator()
        if e.MoveNext() then 
            if e.Current.Interval.Start <> always.Start then 
                yield always.Start => e.Current.Interval.Start := None
            yield e.Current.Interval := Some e.Current.Value
            let rec next previous = 
                seq { 
                    if e.MoveNext() then 
                        if e.Current.Interval.Start > previous.Interval.End then 
                            yield previous.Interval.End => e.Current.Interval.Start := None
                        yield e.Current.Interval := f e.Current.Value
                        yield! next e.Current
                    elif previous.Interval.End <> always.End then 
                        yield previous.Interval.End => always.End := zero }
            yield! next e.Current
        else yield always := None }
    |> IntervalValuedSet

let merge (IntervalValuedSet t) =
    seq {
        use e = t.GetEnumerator()

        if e.MoveNext() then
            let rec merge x =     
                seq { 
                    if e.MoveNext() then
                        let y = e.Current
                        if y.Interval.Start <= x.Interval.End && y.Value = x.Value then
                            let start = min y.Interval.Start x.Interval.Start
                            let enD = max y.Interval.End x.Interval.End
                            if enD > start then yield! merge (start => enD := x.Value)
                            else yield x; yield! merge y
                        else yield x; yield! merge y
                    else yield x }
            yield! merge e.Current } |> IntervalValuedSet

let contiguous temporal = temporal |> contiguousT None Some
let build temporaries = 
    let sort temporaries = temporaries |> Seq.sortBy (fun t -> t.Interval.Start)
    let removeEmpty temporaries = temporaries |> Seq.filter(fun t -> t.Interval.Start < t.Interval.End)
    let check temporaries = temporaries |> Seq.map(fun t -> if t.Interval.End < t.Interval.Start then failwithf "invalid period %O" t.Interval else t)
    temporaries |> removeEmpty |> check |> sort |> IntervalValuedSet

let toList (IntervalValuedSet temporaries) = temporaries |> Seq.toList

let ofOption (IntervalValuedSet temporaries) = 
    seq {
        for t in temporaries do 
            if t.Value |> Option.isSome then yield t.Interval := t.Value.Value } |> IntervalValuedSet

let ofMap temporals = 
    Map.fold 
        <| fun state k t -> lift2 (fun m v -> m |> Map.add k v) state t
        <| ret Map.empty
        <| temporals

let fold folder state (IntervalValuedSet temporaries) = 
    let f state t = folder state t.Interval t.Value 
    Seq.fold f state temporaries

let clamp period (IntervalValuedSet temporaries) = 
    temporaries
    |> Seq.collect(fun t -> 
        let start = max period.Start t.Interval.Start
        let enD = min period.End t.Interval.End
        if enD > start then 
            (start => enD := t.Value) |> Seq.singleton
        else Seq.empty)
    |> IntervalValuedSet

let inline split length intervalValuedSet = 
    let inline duration p = p.End - p.Start

    let rec splitI t = 
        seq {
            if t.Interval |> duration <= length then yield t
            else
                let next = t.Interval.Start + length
                yield { t with Interval = { t.Interval with End = next } }
                yield! splitI { t with Interval = { t.Interval with Start = next } } 
        }
    intervalValuedSet
    |> toList
    |> Seq.collect splitI
    |> build

let (<!>) = map
let (<*>) = apply