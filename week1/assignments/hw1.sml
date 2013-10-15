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

(* return how many dates are from the given months *)
fun number_in_months(dates : (int*int*int) list, months : int list) =
  if null months then 0
  else number_in_month(dates, (hd months)) + number_in_months(dates, (tl
  months))

(* return the dates that belong to the month *)
fun dates_in_month(dates : (int*int*int) list, month : int) =
  let
    fun in_month(date : int*int*int) = (#2 date) = month
  in
    if null dates then []
    else if in_month(hd dates) then (hd dates) :: dates_in_month((tl dates), month)
    else dates_in_month((tl dates), month)
  end

(* return the dates that belong in the given months *)
fun dates_in_months(dates : (int*int*int) list, months : int list) =
  if null months then []
  else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))

(* return the nth item of the given list - or empty *)
fun get_nth(strs : string list, n : int) =
  let
    fun index'(strs : string list, i : int) =
      if null strs then ""
      else if n = i then hd strs
      else index'((tl strs), (i + 1))
  in
    index'(strs, 1)
  end

(* toString for dates  *)
fun date_to_string(date : int*int*int) =
  let
    val months = ["January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, (#2 date)) ^ " " ^ Int.toString(#3 date) ^
      ", " ^ Int.toString(#1 date)
  end

(* return how many itens of the given list will sum less than the num passed *)
fun number_before_reaching_sum(num : int, xs : int list) =
  let
    fun sum(total : int, count : int, xss : int list) =
      if null xss then count
      else if (total + (hd xss)) >= num then count
      else sum(total + (hd xss), count + 1, (tl xss))
  in
    sum(0, 0, xs)
  end

(* returns the month of the given day of the year *)
fun what_month(day : int) =
  let
    val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days) + 1
  end

(* for the two given days of year, return the list of each months between them
* *)
fun month_range(day1 : int, day2 : int) =
  if day1 > day2 then []
  else
    let
      fun r(lo, hi) =
        if lo > hi then []
        else lo :: r(lo + 1, hi)

      fun d(xs) =
        if null xs then []
        else what_month(hd xs) :: d(tl xs)
    in
      d(r(day1, day2))
    end

(* return the oldest day in the days list *)
fun oldest(days : (int*int*int) list) =
  let
    fun comp(day : int*int*int, xs : (int*int*int) list) =
      if null xs then day
      else
        let
          val h = hd xs
          val t = tl xs
        in
          if is_older(day, h) then comp(day, t)
          else comp(h, t)
        end
  in
    if null days then NONE
    else SOME(comp(hd days, tl days))
  end

