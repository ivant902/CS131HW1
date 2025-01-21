(*let my_subset_test0 = subset [] [1;2;3]
let my_subset_test1 = subset [3;1;3] [1;2;3]
let my_subset_test2 = not (subset [1;3;7] [4;1;3])

let my_equal_sets_test0 = equal_sets [1;3] [3;1;3]
let my_equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3])

let my_set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [] []) []

let my_set_all_union_test0 =
  equal_sets (set_all_union []) []
let my_set_all_union_test1 =
  equal_sets (set_all_union [[3;1;3]; [4]; [1;2;3]]) [1;2;3;4]
let my_set_all_union_test2 =
  equal_sets (set_all_union [[5;2]; []; [5;2]; [3;5;7]]) [2;3;5;7]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let my_computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.
let my_computed_fixed_point_test3 =
  ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25)
*)
let rules = 
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]
let awksub_grammer = Num, rules
let my_filter_blind_alleys_test0 = 
    filter_blind_alleys awksub_grammer = awksub_grammer