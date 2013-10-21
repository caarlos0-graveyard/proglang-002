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


fun similar_names (xs: string list list, name: full_name) =
  let
    val {first=f, last=l, middle=m} = name
    fun aux(xs: string list) =
      case xs of
           [] => []
         | x::xs' => {first=x, middle=m, last=l}::aux(xs')
  in
    name::(aux(get_substitutions1(xs, f)))
  end

(**)

fun card_color(c, _) =
  case c of
       (Clubs | Spades) => Black
     | _ => Red

fun card_value(_, x) =
  case x of
       Num(y) => y
     | Ace => 11
     | _ => 10

fun remove_card(xs: card list, x: card, e) =
  case xs of
       [] => raise e
     | x'::xs' => if (x' = x) then xs' else x::remove_card(xs', x, e)

fun all_same_color(xs: card list) =
  case xs of
       [] => true
     | x::[] => true
     | x::(ns::xs) => (card_color(x) = card_color(ns)) andalso
     all_same_color(ns::xs)

fun sum_cards(xs) =
  let
    fun aux(xs, acc) =
      case xs of
           [] => acc
         | x::xs' => aux(xs', acc + card_value(x))
  in
    aux(xs, 0)
  end

fun score(xs: card list, goal: int) =
  let
    val sum = sum_cards xs
    val pre = if (sum > goal) then 3 * (sum - goal) else (goal - sum)
  in
    if (all_same_color xs) then pre div 2 else pre
  end

fun officiate(cards: card list, moves: move list, goal: int) =
  let
    fun aux(cards: card list, held: card list, moves: move list, goal: int) =
      case (cards, held, moves, goal) of
           (_, _, [], _) => score(held, goal) (* empty moves list *)
         | ([], _, _, _) => score(held, goal) (* empty deck *)
         | (c::cs, _, m::ms, _) =>
             case m of (* other cases *)
               Discard d =>
                aux(c::cs, remove_card(held, d, IllegalMove), ms, goal)
             | Draw =>
                 case c::cs of
                      [] => score(held, goal)
                    | _ =>
                        let
                          val held' = c::held
                          val held_sum = sum_cards(held')
                        in
                          if (held_sum > goal)
                          then score(held', goal)
                          else aux(cs, held', ms, goal)
                        end
  in
    aux(cards, [], moves, goal)
  end

