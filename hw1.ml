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

(* this checks if a single symbol is terminal*)
let is_terminal symbol = 
    match symbol with 
    | T _ -> true
    | N _ -> false  
(* rules that enter this are only the rhs of a rule
this checks if the entire rule is terminal
*)
let rec is_terminal_rule rule =  
    match rule with
    | [] -> true
    | x::xs -> 
        if is_terminal x 
        then is_terminal_rule xs
        else false
(* this isolates all the immediate terminal rules*)
let rec create_terminal_rule_list rules = 
    match rules with 
    | [] -> []
    | x::xs -> 
        if is_terminal_rule (snd x) then 
            x::create_terminal_rule_list xs
        else 
            create_terminal_rule_list xs
(* this isolates all rules with at least one nonterminal*)
let rec create_nonterminal_rule_list rules = 
    match rules with 
    | [] -> []
    | x::xs -> 
        if not (is_terminal_rule (snd x)) then 
            x::create_nonterminal_rule_list xs
        else 
            create_nonterminal_rule_list xs
(* this checks if a rule can be resolved to a terminal rule*)
let rec is_fully_terminal n_rules t_rules =
    match n_rules with
    | [] -> t_rules
    | rule::rest ->
        let lhs, rhs = rule in
        let can_resolve = List.for_all (fun symbol ->
            match symbol with
            | T _ -> true
            | N nonterminal -> 
                List.exists (fun (t_lhs, _) -> t_lhs = nonterminal) t_rules
        ) rhs in
        if can_resolve then
            is_fully_terminal rest (rule::t_rules)
        else
            is_fully_terminal rest t_rules
(*This orders the rules according to the original list of rules*)
let order_rules fully_terminal_rules rules = 
    List.filter (fun rule -> List.mem rule fully_terminal_rules) rules

let filter_blind_alleys g = 
    let (start, rules) = g in
    let t_rules = create_terminal_rule_list rules in
    let n_rules = create_nonterminal_rule_list rules in
    let fully_terminal_rules = is_fully_terminal n_rules t_rules in
    (start, (order_rules fully_terminal_rules rules))
