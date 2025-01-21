let subset_test0 = subset [] [1;2;3]
Printf.printf "subset_test0: %b\n" subset_test0;

let subset_test1 = subset [3;1;3] [1;2;3];
Printf.printf "subset_test1: %b\n" subset_test1; 

let subset_test2 = not (subset [1;3;7] [4;1;3]);
Printf.printf "subset_test2: %b\n" subset_test2;

let equal_sets_test0 = equal_sets [1;3] [3;1;3] in 
Printf.printf "equal_sets_test0: %b\n" equal_sets_test0;

let equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3]) in 
Printf.printf "equal_sets_test1: %b\n" equal_sets_test1;

let set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3] in 
Printf.printf "set_union_test0: %b\n" set_union_test0;

let set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3] in 
Printf.printf "set_union_test1: %b\n" set_union_test1;

let set_union_test2 = equal_sets (set_union [] []) [] in 
Printf.printf "set_union_test2: %b\n" set_union_test2;

let set_all_union_test0 = equal_sets (set_all_union []) [] in
Printf.printf "set_all_union_test0: %b\n" set_all_union_test0;

let set_all_union_test1 = equal_sets (set_all_union [[3;1;3]; [4]; [1;2;3]]) [1;2;3;4] in 
Printf.printf "set_all_union_test1: %b\n" set_all_union_test1;

let set_all_union_test2 = equal_sets (set_all_union [[5;2]; []; [5;2]; [3;5;7]]) [2;3;5;7] in 
Printf.printf "set_all_union_test2: %b\n" set_all_union_test2;

(* Test 0 *)
let computed_fixed_point_test0 =
    let result = computed_fixed_point (=) (fun x -> x / 2) 1000000000 in
    Printf.printf "Test 0: Expected 0, Got %d\n" result;
    result = 0
in
Printf.printf "Test 0 Passed: %b\n" computed_fixed_point_test0;

(* Test 1 *)
let computed_fixed_point_test1 =
    let result = computed_fixed_point (=) (fun x -> x *. 2.) 1. in
    Printf.printf "Test 1: Expected infinity, Got %f\n" result;
    result = infinity
in
Printf.printf "Test 1 Passed: %b\n" computed_fixed_point_test1;

(* Test 2 *)
let computed_fixed_point_test2 =
    let result = computed_fixed_point (=) sqrt 10. in
    Printf.printf "Test 2: Expected 1.0, Got %f\n" result;
    result = 1.
in
Printf.printf "Test 2 Passed: %b\n" computed_fixed_point_test2;

(* Test 3 *)
let computed_fixed_point_test3 =
    let result = computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
                                    (fun x -> x /. 2.)
                                    10. in
    Printf.printf "Test 3: Expected 1.25, Got %f\n" result;
    result = 1.25
in
Printf.printf "Test 3 Passed: %b\n" computed_fixed_point_test3;