(* return true if date1 is older than date2 *)
fun is_older(date1 : int*int*int, date2 : int*int*int) =
  let
    val yr1 = #1 date1
    val yr2 = #1 date2
    val mn1 = #2 date1
    val mn2 = #2 date2
    val dy1 = #3 date1
    val dy2 = #3 date2
  in
    if yr2 > yr1 then true
    else if yr2 < yr1 then false
    else if mn2 > mn1 then true
    else if mn2 < mn1 then false
    else if dy2 > dy1 then true
    else false
  end

(* return how many dates are from the given month *)
fun number_in_month(dates : (int*int*int) list, month : int) =
  let
    fun count(date : int*int*int) =
      if (#2 date) = month then 1
      else 0

    fun count_all(xs : (int*int*int) list, sum : int) =
      if null xs then sum
      else count_all((tl xs), sum + count(hd xs))
  in
    count_all(dates, 0)
  end

