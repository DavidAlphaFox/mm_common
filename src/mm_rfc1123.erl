-module(mm_rfc1123).
-export([parse_date/1,parse_time/1]).
% Date and Time Specification of RFC 1123
% BNF Syntax 

% date-time   =  [ day "," ] date time        ; dd mm yyyy
%                                             ; hh:mm:ss zzz
% day         =  "Mon"  / "Tue" /  "Wed"  / "Thu"
%             /  "Fri"  / "Sat" /  "Sun"

% date        =  1*2DIGIT month 2*4DIGIT        ; day month year
%                                               ; e.g. 20 Jun 1987

% month       =  "Jan"  /  "Feb" /  "Mar"  /  "Apr"
%             /  "May"  /  "Jun" /  "Jul"  /  "Aug"
%             /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"

% time        =  hour zone                      ; ANSI and Military

% hour        =  2DIGIT ":" 2DIGIT [":" 2DIGIT] ; 00:00:00 - 23:59:59
                                                 
% zone        =  "UT"  / "GMT"                  ; Universal Time
%                                               ; North American : UT
%             /  "EST" / "EDT"                  ;  Eastern:  - 5/ - 4
%             /  "CST" / "CDT"                  ;  Central:  - 6/ - 5
%             /  "MST" / "MDT"                  ;  Mountain: - 7/ - 6
%             /  "PST" / "PDT"                  ;  Pacific:  - 8/ - 7
%             /  1ALPHA                         ; Military: Z = UT;
%                                               ;  A:-1; (J not used)
%                                               ;  M:-12; N:+1; Y:+12
%             / ( ("+" / "-") 4DIGIT )          ; Local differential
%                                               ;  hours+min. (HHMM)

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
% zone        =  "UT"  / "GMT"                  ; Universal Time
%                                               ; North American : UT
%             /  "EST" / "EDT"                  ;  Eastern:  - 5/ - 4
%             /  "CST" / "CDT"                  ;  Central:  - 6/ - 5
%             /  "MST" / "MDT"                  ;  Mountain: - 7/ - 6
%             /  "PST" / "PDT"                  ;  Pacific:  - 8/ - 7
%             /  1ALPHA                         ; Military: Z = UT;
%                                               ;  A:-1; (J not used)
%                                               ;  M:-12; N:+1; Y:+12
%             / ( ("+" / "-") 4DIGIT )          ; Local differential
%                                               ;  hours+min. (HHMM)
zone_to_integer("UT")-> {0,0};
zone_to_integer("GMT")-> {0,0};
zone_to_integer("EST")-> {-5,0};
zone_to_integer("EDT")-> {-4,0};
zone_to_integer("CST")-> {-6,0};
zone_to_integer("CDT")-> {-5,0};
zone_to_integer("MST")-> {-7,0};
zone_to_integer("MDT")-> {-6,0};
zone_to_integer("PST")-> {-8,0};
zone_to_integer("PDT")-> {-7,0};
zone_to_integer("A")-> {-1,0};
zone_to_integer("M")-> {-12,0};
zone_to_integer("N")-> {1,0};
zone_to_integer("Y")-> {12,0};
zone_to_integer(Z)-> 
	Operator = string:substr(Z,1,1),
	Hour = string:substr(Z,2,2),
	Min = string:substr(Z,4,2),
	case Operator of
		"+" ->
			{erlang:list_to_integer(Hour),erlang:list_to_integer(Min)};
		"-"->
			{0 - erlang:list_to_integer(Hour),0 - erlang:list_to_integer(Min)}
	end.
% date        =  1*2DIGIT month 2*4DIGIT        ; day month year
%                                               ; e.g. 20 Jun 1987
parse_date(Date)->
	DateTokens = string:tokens(Date,[?SEPARATOR_SPACE]),
	[Day,Month,Year] = DateTokens,
	{erlang:list_to_intger(Year),month_to_integer(Month),erlang:list_to_intger(Day)}.
parse_time(Time)->
	TimeTokens = string:tokens(Time,[?SEPARATOR_SPACE,?SEPARATOR_COLON]),
	case erlang:length(TimeTokens) of
		3->
			[Hour,Min,Zone] = TimeTokens,
			HourInteger = erlang:list_to_integer(Hour),
			MinInteger = erlang:list_to_integer(Min),
			{HourDiff,MinDiff} = zone_to_integer(Zone),
			{HourInteger + HourDiff,MinInteger + MinDiff,0};
		4 ->
			[Hour,Min,Sec,Zone] = TimeTokens,
			HourInteger = erlang:list_to_integer(Hour),
			MinInteger = erlang:list_to_integer(Min),
			SecInteger = erlang:list_to_integer(Sec),
			{HourDiff,MinDiff} = zone_to_integer(Zone),
			{HourInteger + HourDiff,MinInteger + MinDiff,SecInteger}
	end.


