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
    let test = f x in  (* Call f first to get the new value *)
    if eq test x then x 
    else computed_fixed_point eq f test

(* test changes *)