-module(mm_date_common).
-export([adjust/2]).
leap_year(Year)->
	Y1 = Year / 100,
	Y2 = Year div 100,
	if 
		Y1 == Y2->
			Y3 = Year / 400,
			Y4 = Year div 400,
			Y3 == Y4;
		true ->
			Y3 = Year / 4,
			Y4 = Year div 4,
			Y3 == Y4
	end.
day_of_month(1)-> 31;
day_of_month(3)-> 31;
day_of_month(4)-> 30;
day_of_month(5)-> 31;
day_of_month(6)-> 30;
day_of_month(7)-> 31;
day_of_month(8)-> 31;
day_of_month(9)-> 30;
day_of_month(10)-> 31;
day_of_month(11)-> 30;
day_of_month(12)-> 31.

day_of_month(Year,Month)->
	if 
		Month == 2 ->
			case leap_year(Year) of 
				ture ->
					29;
				false ->
					28
			end;
		Month == 0 ->
			day_of_month(12);
		true->
			day_of_month(Month)
	end.

adjust({Year,Month,Day},{Hour,Min,Sec})->
	{DateTuple,TimeTuple} = adjust_date_time({Year,Month,Day},{Hour,Min,Sec}),
	DateTuple2 = adjust_date(DateTuple),
	{DateTuple2,TimeTuple}.

adjust_date({Year,Month,Day})->
	DayOfMonth = day_of_month(Year,Month),
	DayOfPrevMonth = day_of_month(Year,Month -1),
	if
		Day > DayOfMonth ->
			adjust_date({Year,Month + 1 ,  Day - DayOfMonth});
		DayOfMonth == 0 ->
			adjust_date({Year,Month -1 , DayOfPrevMonth});
		Month > 12 ->
			adjust_date({Year + 1, Month -12 ,Day});
		Month == 0 ->
			adjust_date({Year - 1 , 12, DayOfPrevMonth});
		true ->
			{Year,Month,Day}
	end.
adjust_date_time({Year,Month,Day},{Hour,Min,Sec})->
	if 
		Min > 60 ->
			adjust_date_time({Year,Month,Day},{Hour + 1,Min - 60,Sec});
		Min < 0 ->
			adjust_date_time({Year,Month,Day},{Hour - 1,Min + 60,Sec});
		Hour > 24 ->
			adjust_date_time({Year,Month,Day + 1},{Hour - 24,Min,Sec});
		Hour < 0 ->
			adjust_date_time({Year,Month,Day - 1},{Hour + 24,Min,Sec});
		true ->
			{{Year,Month,Day},{Hour,Min,Sec}}
	end.