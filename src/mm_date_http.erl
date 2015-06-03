-module(mm_date_http).
-export([parse/1]).

parse(Date)->
    [_D,_A,_Y,DateType| _Rest] = Date,
    try
        case DateType of
            $\, ->
                mm_date_rfc1123:parse(Date);
            $\  ->
                mm_date_asctime:parse(Date);
            _ ->
                mm_date_rfc1036:parse(Date)
        end
    catch
        _Any:_Reason ->
            bad_date
    end.
