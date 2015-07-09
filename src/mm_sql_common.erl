-module(mm_sql_common).
-export([escape/1,interpose/2,operator_to_string/1]).
-export([slot_numbered/1,slot_question/1]).
-export([check_operator/1]).

-type field() :: string() | atom().
-type condition() ::{'and', [condition()]} | {'or', [condition()]} | {field(), term()}.
-type operator() :: '<' | '>' | '=' | '<=' | '>=' | '!=' | 'like'.
-type field_name() :: atom().
-type value() :: binary() | string() | number() | 'null' | 'not_null'.
-type expression() ::
    [expression()]
    | {'and', [expression()]}
    | {'or', [expression()]}
    | {'not', expression()}
    | terminal().

-type terminal() ::
    {field_name(), operator(), field_name()}
    | {field_name(), operator(), value()}
    | {field_name(), value()}.

-export_type([expression/0,field/0,condition/0]).


-spec escape(field()) -> string().
escape(Field) when is_atom(Field) ->
  escape(atom_to_list(Field));
escape(Field) when is_list(Field) ->
  lists:flatten(["`", Field, "`"]).

-spec interpose(term(), list()) -> list().
interpose(Sep, List) ->
  interpose(Sep, List, []).

-spec interpose(term(), list(), list()) -> list().
interpose(_Sep, [], Result) ->
  lists:reverse(Result);
interpose(Sep, [Item | []], Result) ->
  interpose(Sep, [], [Item | Result]);
interpose(Sep, [Item | Rest], Result) ->
  interpose(Sep, Rest, [Sep, Item | Result]).

-spec operator_to_string(atom())-> list().
operator_to_string('=<') -> "<=";
operator_to_string('/=') -> "!=";
operator_to_string('==') -> "=";
operator_to_string(Op) -> atom_to_list(Op).

-spec slot_question({'?', integer()}) -> string().
slot_question(_) -> " ? ".

-spec slot_numbered({'?', integer()}) -> iodata().
slot_numbered({_, N}) -> [" $", integer_to_list(N), " "].


-spec check_operator(operator()) -> ok.
check_operator('<') -> ok;
check_operator('=<') -> ok;
check_operator('>') -> ok;
check_operator('>=') -> ok;
check_operator('==') -> ok;
check_operator('/=') -> ok;
check_operator('like') -> ok;
check_operator(Op) -> throw({unknown_operator, Op}).
