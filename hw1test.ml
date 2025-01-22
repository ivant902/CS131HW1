(* Copyright 2006-2023 Paul Eggert.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  *)

let my_subset_test0 = subset [1] [1;2;3]
let my_subset_test1 = subset [3;1;3;2] [1;2;3]
let my_subset_test2 = not (subset [1;3;7;5] [4;1;7])

let my_equal_sets_test0 = equal_sets [5;10] [10;10;10;5]
let my_equal_sets_test1 = not (equal_sets [1;3;35] [3;1;34])

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

let my_computed_periodic_point_test0 =
    computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1
let my_computed_periodic_point_test1 =
    computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1.

let my_whileseq_test0 = 
    whileseq ((+) 3) ((>) 10) 1 = [1; 4; 7]
(* An example grammar for a small subset of Awk.  *)

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
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

let my_filter_blind_alleys_test0 =
  filter_blind_alleys (Expr,
      [Expr, [N Num];
       Expr, [N Lvalue];
       Expr, [N Expr; N Lvalue];
       Expr, [N Lvalue; N Expr];
       Expr, [N Expr; N Binop; N Expr];
       Lvalue, [N Lvalue; N Expr];
       Lvalue, [N Expr; N Lvalue];
       Lvalue, [N Incrop; N Lvalue];
       Lvalue, [N Lvalue; N Incrop];
       Incrop, [T"++"]; Incrop, [T"--"];
       Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
       Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]])
  = (Expr,
     [Expr, [N Num];
      Incrop, [T"++"]; Incrop, [T"--"];
      Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"];
      Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]])
