(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun all_except_option (_, []) = NONE
  | all_except_option (s, xs) =
    let val xs' = List.filter (fn x => x <> s) xs
    in if xs' = xs then NONE else SOME xs' end

fun get_substitutions1 (xs : string list list, s: string) =
  case xs of
       [] => []
     | x::xs => case all_except_option(s, x) of
                     NONE => get_substitutions1(xs, s)
                   | SOME y => y @ get_substitutions1(xs, s)

fun get_substitutions2 (xs: string list list, s: string) =
  let
    fun aux(x : string list list, s: string, acc) =
      case x of
           [] => acc
         | (x::xs) => case all_except_option(s,x) of
                          NONE => aux(xs, s, acc)
                        | SOME y => aux(xs, s, acc @ y)
  in
    aux(xs, s, [])
  end

type full_name = {  first : string,
                    middle: string,
                    last  : string }

(**)

