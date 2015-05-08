module Temporality

type DateTime = System.DateTime

type Period = 
    { startDate: DateTime 
      endDate: DateTime }

type Bound = 
    | Inclusive
    | Exclusive

type BoundedPeriod = 
    | Inclusive of Period
    | Exclusive of Period

type Temporary<'a> =
    { period: BoundedPeriod 
      value: 'a }

