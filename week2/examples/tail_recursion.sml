fun fact n =
  if n=0 then 1 else n*fact(n-1)

(* tail recursion here: *)
fun factt n =
  let fun aux(n,acc) =
    if n=0 then acc
    else aux(n-1,acc*n)
  in
    aux(n,1)
  end

val x = fact 3
val x' = factt 3

fun sum xs =
  case xs of
       [] => 0
     | x::xs' => x + sum xs'

fun sumt xs =
  let fun aux(xs, acc) =
    case xs of
         [] => acc
       | x::xs' => aux(xs', x+acc)
  in
    aux(xs, 0)
  end

val y = sum [1,2,3]
val y' = sumt [1,2,3]

fun rev xs =
  case xs of
       [] => []
     | x::xs' => (rev xs') @ [x]

fun revt xs =
  let fun aux(xs, acc) =
    case xs of
         [] => acc
       | x::xs' => aux(xs', x::acc)
  in aux (xs, [])
  end

val z = rev [1,2,3]
val z' = revt [1,2,3]

