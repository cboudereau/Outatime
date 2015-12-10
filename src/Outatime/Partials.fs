module Partials

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
