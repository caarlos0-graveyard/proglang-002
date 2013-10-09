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
    fun in_month(date : int*int*int) =
      if (#2 date) = month then 1
      else 0
  in
    if null dates then 0
    else in_month(hd dates) + number_in_month((tl dates), month)
  end

fun number_in_months(dates : (int*int*int) list, months : int list) =
  if null months then 0
  else number_in_month(dates, (hd months)) + number_in_months(dates, (tl
  months))

fun dates_in_month(dates : (int*int*int) list, month : int) =
  let
    fun in_month(date : int*int*int) = (#2 date) = month
  in
    if null dates then []
    else if in_month(hd dates) then (hd dates) :: dates_in_month((tl dates), month)
    else dates_in_month((tl dates), month)
  end

fun dates_in_months(dates : (int*int*int) list, months : int list) =
  if null months then []
  else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))

fun get_nth(strs : string list, n : int) =
  let
    fun index'(strs : string list, i : int) =
      if null strs then ""
      else if n = i then hd strs
      else index'((tl strs), (i + 1))
  in
    index'(strs, 1)
  end

fun date_to_string(date : int*int*int) =
  let
    val months = ["January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, (#2 date)) ^ " " ^ Int.toString(#3 date) ^
      ", " ^ Int.toString(#1 date)
  end
