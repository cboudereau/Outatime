module Temporality

type DateTime = System.DateTime

type TimeSpan = System.TimeSpan

type BoundedPeriodException(message) = inherit exn(message)

type Period = 
    { startDate : DateTime
      endDate : DateTime }
    with
        override this.ToString() = sprintf "%A" this

let forNDays n = TimeSpan.FromDays(float n)

let forOneDay = forNDays 1

type BoundedPeriod = 
    | Inclusive of Period
    | Exclusive of Period
    
    override this.ToString() = sprintf "%A" this

    static member ToInclusive boundedPeriod = 
        let validPeriod = BoundedPeriod.PreCondition boundedPeriod
        match validPeriod with
        | Inclusive period -> period
        | Exclusive period -> { period with endDate = period.endDate.AddDays(-1.) }
    
    static member ToExclusive offset boundedPeriod = 
        let validPeriod = BoundedPeriod.PreCondition boundedPeriod
        match validPeriod with
        | Exclusive period -> period
        | Inclusive period -> { period with endDate = period.endDate + offset }
    
    static member ToDailyExclusive = BoundedPeriod.ToExclusive forOneDay

    static member private PreCondition(boundedPeriod) = 
        let raiseBoundedPeriodException period = 
            raise (BoundedPeriodException(sprintf "%A start must be greater than %A" period.startDate period.endDate))
        
        match boundedPeriod with
        | Inclusive period -> 
            if period.startDate >= period.endDate then 
                raiseBoundedPeriodException period
            else boundedPeriod
        | Exclusive period -> 
            if period.startDate > period.endDate then 
                raiseBoundedPeriodException period
            else boundedPeriod

type Temporary<'a when 'a : equality and 'a : comparison> = 
    { period : BoundedPeriod
      value : 'a }
    with
        override this.ToString() = sprintf "%A" this
