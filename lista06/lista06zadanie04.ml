let inr cont text = fun n -> cont (text ^ (string_of_int n))
let flt cont text = fun f -> cont (text ^ (string_of_float f))
let str cont text = fun s -> cont (text ^ s)
let eol cont text = cont (text ^ "\n")
let lit s cont text = cont (text ^ s)

let (++) cont1 cont2 = fun cont text -> cont1 (cont2 cont) text
let id x = x
let sprintf f = f id ""

let check1 = "Ala ma 2 koty." = sprintf (lit "Ala ma " ++ inr ++ lit " kot" ++ str ++ lit ".") 2 "y"
