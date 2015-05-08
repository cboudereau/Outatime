module Temporality

type DateTime = System.DateTime

type PeriodException(message) = 
    inherit exn(message)

type Period = 
    { startDate : DateTime
      endDate : DateTime }

type BoundedPeriod = 
    | Inclusive of Period
    | Exclusive of Period
    
    static member ToInclusive(boundedPeriod) = 
        let validPeriod = BoundedPeriod.PreCondition boundedPeriod
        match validPeriod with
        | Inclusive period -> period
        | Exclusive period -> { period with endDate = period.endDate.AddDays(-1.) }
    
    static member ToExclusive(boundedPeriod) = 
        let validPeriod = BoundedPeriod.PreCondition boundedPeriod
        match validPeriod with
        | Exclusive period -> period
        | Inclusive period -> { period with endDate = period.endDate.AddDays(1.) }
    
    static member private PreCondition(boundedPeriod) = 
        match boundedPeriod with
        | Inclusive period -> 
            if period.startDate >= period.endDate then 
                raise (PeriodException(sprintf "%A start must be greater than %A" period.startDate period.endDate))
            else boundedPeriod
        | Exclusive period -> 
            if period.startDate > period.endDate then 
                raise (PeriodException(sprintf "%A start must be greater than %A" period.startDate period.endDate))
            else boundedPeriod

type Temporary<'a when 'a : equality and 'a : comparison> = 
    { period : BoundedPeriod
      value : 'a }
