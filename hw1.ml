let subset a b = 
    (* for all x in a, checks if x is in b*)
    List.for_all(fun x -> List.mem x b) a

let equal_sets a b = 
    subset a b && subset b a

let rec set_union a b = 
    match a with 
    | [] -> b 
    | x::xs -> 
        if List.mem x b then 
            set_union xs b
        else 
            x::set_union xs b

let rec set_all_union a = 
    match a with 
    | [] -> []
    | x::xs -> set_union x (set_all_union xs)
    
let rec computed_fixed_point eq f x = 
    let test = f x in 
    if eq test x then x 
    else computed_fixed_point eq f test

(* helper function that applys function f, p times*)
let rec apply_p f p x = 
    if p = 1 then (f x)
    else apply_p f (p-1) (f x)

let rec computed_periodic_point eq f p x = 
    if p = 0 then x
    else if p = 1 then (f x)
    else if (eq (apply_p f p x) x ) then x 
    else computed_periodic_point eq f p (f x)

let rec whileseq s p x = 
    if not (p x) then []
    else x::whileseq s p (s x)

type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal

let is_terminal symbol = 
    match symbol with 
    | T _ -> true
    | N _ -> false  
    
let filter_blind_alleys g = 



    

