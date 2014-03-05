let zlozenie f g x = f (g x)
let (<.>) = zlozenie
let is_some x = match x with None -> false | Some _ -> true