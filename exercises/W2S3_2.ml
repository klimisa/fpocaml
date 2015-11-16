type date = { year : int; month : int; day : int; hour : int; minute : int };;

let the_origin_of_time = { year = 1; month = 1; day = 1; hour = 0; minute = 0 };;
    
let wellformed date =
  let { year = origin_year; month = origin_month; day = origin_day; hour = origin_hour; minute = origin_minute } = the_origin_of_time in
  let { year; month; day; hour; minute } = date in
  let 
    is_valid_year 	= (year >= origin_year) and
    is_valid_month 	= (month >= origin_month && month <=5) and
    is_valid_day 		= (day>= origin_day && day<= 4) and
    is_valid_hour		= (hour>= origin_hour && hour<= 2) and
    is_valid_minute = (minute>= origin_minute && minute<= 1)
  in 
  (is_valid_year && is_valid_month && is_valid_day && is_valid_hour && is_valid_minute);;

let next date =
  let { year = origin_year; month = origin_month; day = origin_day; hour = origin_hour; minute = origin_minute } = the_origin_of_time in
  let { year; month; day; hour; minute } = date in
  let 
    next_minute_date = { year; month; day; hour; minute = minute + 1 } and
    next_hour_date = 	{ year; month; day; hour = hour + 1; minute = origin_minute } and
    next_day_date = { year; month; day = day + 1; hour = origin_hour; minute = origin_minute } and
    next_month_date = { year; month = month + 1; day = origin_day; hour = origin_hour; minute = origin_minute } and
    next_year_date = { year = year + 1; month = origin_month; day = origin_day; hour = origin_hour; minute = origin_minute } 	in
  let next_date = 
    if wellformed next_minute_date then next_minute_date 
    else if wellformed next_hour_date then next_hour_date 
    else if wellformed next_day_date then next_day_date 
    else if wellformed next_month_date then next_month_date 
    else next_year_date
  in next_date;;

let of_int minutes =
		let rec convert_int_to_date datetime min = 
    if min >= 120 then
      convert_int_to_date { year = datetime.year + 1; month = datetime.month; day = datetime.day; hour = datetime.hour; minute = datetime.minute } (min - 120)
    else if min >= 24 && min < 120 then
      convert_int_to_date	{ year = datetime.year; month = datetime.month + 1; day = datetime.day; hour = datetime.hour; minute = datetime.minute } (min - 24)
    else if min >= 6 && min < 24 then
      convert_int_to_date	{ year = datetime.year; month = datetime.month; day = datetime.day + 1; hour = datetime.hour; minute = datetime.minute } (min - 6)			
    else if min >= 2 && min < 6 then
      convert_int_to_date	{ year = datetime.year; month = datetime.month; day = datetime.day; hour = datetime.hour + 1; minute = datetime.minute } (min - 2)
    else
      { year = datetime.year; month = datetime.month; day = datetime.day; hour = datetime.hour; minute = min  }
		in 	convert_int_to_date the_origin_of_time minutes;;
