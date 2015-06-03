-module(mm_http_date).
-export([parse/1]).

parse(Date)->
	[_D,_A,_Y,DateType| _Rest] = Date,
	try
    	case DateType of
	    	$\, ->
		 		mm_rfc1123:parse(Date);
	     	$\  ->
	     		mm_ascii:parse(Date);
	     	_ ->
		 		mm_rfc1036:parse(Date)
		end
	catch 
		_Any:_Reason->
			bad_date
	end.