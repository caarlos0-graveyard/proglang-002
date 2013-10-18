exception ListLenghtMismatch

fun zip3 list_triple =
  case list_triple of
       ([],[],[]) => []
     | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
     | _ => raise ListLenghtMismatch

fun unzip3 lst =
  case lst of
       [] => ([],[],[])
     | (a,b,c)::tl => let val (l1,l2,l3) = unzip3 tl
                      in (a::l1,b::l2,c::l3)
                      end

val z = zip3([1,2,3],[4,5,6],[7,8,9]);
val u = unzip3 [(1,4,7), (2,5,8), (3,6,9)];

fun nondecreasing xs = (* int list -> bool *)
  case xs of
       [] => true
     | x::xs' => case xs' of
                      [] => true
                    | y::ys' => x <= y andalso nondecreasing xs'

fun nondecreasing2 xs =
  case xs of
       [] => true
     | _::[] => true
     | head::(neck::rest) => head <= neck
                             andalso nondecreasing (neck::rest)

datatype sgn P | N | Z

fun multsign (x, y) = (* int * int -> sgn *)
  let fun sign x if x=0 then Z else if x>0 then P else N
  in
    case (sign x1, sign x2) of
         (Z,_) => Z
       | (_,Z) => Z
       | (P,P) => P
       | (N,N) => P
       | _ => N
      (* | (N,P) => N
       | (P,N) => N *)
  end

fun len xs =
  case xs of
       [] => 0
     | _::xs' => 1 + len xs'

