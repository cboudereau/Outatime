module Partial

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


