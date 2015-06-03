-module(mm_date_asctime).
-export([parse/1]).
-export([format/0]).

-define(SEPARATOR_SPACE,$\s).
-define(SEPARATOR_COLON,$\:).
% day         =  "Mon"  / "Tue" /  "Wed"  / "Thu"
%             /  "Fri"  / "Sat" /  "Sun"
day_to_integer("Mon")-> 1;
day_to_integer("Tue")-> 2;
day_to_integer("Wed")-> 3;
day_to_integer("Thu")-> 4;
day_to_integer("Fri")-> 5;
day_to_integer("Sat")-> 6;
day_to_integer("Sun")-> 7.

integer_to_day(1)-> "Mon";
integer_to_day(2)-> "Tue";
integer_to_day(3)-> "Wed";
integer_to_day(4)-> "Thu";
integer_to_day(5)-> "Fri";
integer_to_day(6)-> "Sat";
integer_to_day(7)-> "Sun".

% month       =  "Jan"  /  "Feb" /  "Mar"  /  "Apr"
%             /  "May"  /  "Jun" /  "Jul"  /  "Aug"
%             /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"
month_to_integer("Jan")-> 1;
month_to_integer("Feb")-> 2;
month_to_integer("Mar")-> 3;
month_to_integer("Apr")-> 4;
month_to_integer("May")-> 5;
month_to_integer("Jun")-> 6;
month_to_integer("Jul")-> 7;
month_to_integer("Aug")-> 8;
month_to_integer("Sep")-> 9;
month_to_integer("Oct")-> 10;
month_to_integer("Nov")-> 11;
month_to_integer("Dec")-> 12.

integer_to_month(1)-> "Jan";
integer_to_month(2)-> "Feb";
integer_to_month(3)-> "Mar";
integer_to_month(4)-> "Apr";
integer_to_month(5)-> "May";
integer_to_month(6)-> "Jun";
integer_to_month(7)-> "Jul";
integer_to_month(8)-> "Aug";
integer_to_month(9)-> "Sep";
integer_to_month(10)-> "Oct";
integer_to_month(11)-> "Nov";
integer_to_month(12)-> "Dec".


parse_time(Time)->
    TimeTokens = string:tokens(Time,[?SEPARATOR_COLON]),
    [Hour,Min,Sec] = TimeTokens,
    HourInteger = erlang:list_to_integer(Hour),
    MinInteger = erlang:list_to_integer(Min),
    SecInteger = erlang:list_to_integer(Sec),
    {HourInteger,MinInteger,SecInteger}.

%Format Sun Nov  6 08:49:37 1994 
parse(DateTime)->
    Length = erlang:length(DateTime),
    DateTimeSub = string:substr(DateTime,5,Length - 4),
    Tokens = string:tokens(DateTimeSub,[?SEPARATOR_SPACE]),
    [Month,Date,Time,Year] = Tokens,
    TimeTuple = parse_time(Time),
    {{erlang:list_to_integer(Year),month_to_integer(Month),erlang:list_to_integer(Date)},
    TimeTuple}.

format() ->
    {{YYYY,MM,DD},{Hour,Min,Sec}} = calendar:universal_time(),
    DayOfWeek = calendar:day_of_the_week({YYYY,MM,DD}),
    lists:flatten(
      io_lib:format("~s ~3.s ~2.2w ~2.2.0w:~2.2.0w:~2.2.0w ~4.4.0w",
            [integer_to_day(DayOfWeek),integer_to_month(MM),DD,Hour,Min,Sec,YYYY])).
