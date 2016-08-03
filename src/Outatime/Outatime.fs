module Outatime

type DateTime = System.DateTime

type TimeSpan = System.TimeSpan

open SetTheory

type Period = Interval<DateTime>
type Temporary<'v> = IntervalValued<DateTime, 'v>    
type Temporal<'v> = IntervalValuedSet<DateTime, 'v>

let (=>) (x:DateTime) (y:DateTime) : Period = (=>) x y
let (:=) (p:Period) v : Temporary<'v> = (:=) p v

let build (temporaries : 'v Temporary seq) = build temporaries
let clamp period (temporal:Temporal<_>) = clamp period temporal
let toList (temporal:Temporal<_>) : _ Temporary list = toList temporal
let merge (temporal:Temporal<_>) : Temporal<_> = merge temporal
let fold (folder:'a -> Period -> 'c -> 'a) state (temporal:Temporal<_>) = fold folder state temporal
let split length (temporal:Temporal<_>) : Temporal<_> = split length temporal
let contiguous (temporal:Temporal<_>) : Temporal<_ option>= contiguous temporal
let lift2 f (x:Temporal<_>) (y:Temporal<_>) : Temporal<_> = lift2 f x y
let map f (x:Temporal<_>) : Temporal<_> = map f x
let apply (f:Temporal<_>) (x:Temporal<_>) : Temporal<_> = apply f x
let ofOption (x:Temporal<_ option>) : Temporal<_> = ofOption x
let ofMap (x:Map<_,Temporal<_>>) : Temporal<Map<_,_>> = ofMap x
let lift f (x:Temporal<_>) : Temporal<_> =  lift f x

let (<!>) f (x:Temporal<_>) : Temporal<_> = (<!>) f x
let (<*>) (f:Temporal<_>) (x:Temporal<_>) : Temporal<_> = (<*>) f x

let period t = t.Interval
let start p = p.Start
let enD p = p.End
let duration (p:Period) = p.End - p.Start