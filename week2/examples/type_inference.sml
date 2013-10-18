fun sum_triple1 (x, y, z) = x + y + z
fun full_name1 {first=x, middle=y, last=z} = x ^ " " ^ y ^ " " ^ z

(* int * 'a * int -> int*)
fun partial_sum (x, y, z) = x + y
fun partial_name {first=x, middle=y, last=z} = x ^ " " z
