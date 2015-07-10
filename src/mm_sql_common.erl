-module(mm_sql_common).
-export([single_quote/1,interpose/2]).
-export([slot_numbered/1,slot_question/1]).
-export([check_operator/1,operator_to_string/1]).

-export([values_conditions/1,where_clause/3,order_by_clause/2]).


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

-spec single_quote(field()) -> string().
single_quote(Field) when is_atom(Field) ->
  single_quote(atom_to_list(Field));
single_quote(Field) when is_list(Field) ->
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

-spec operator_to_string(atom())-> list().
operator_to_string('=<') -> "<=";
operator_to_string('/=') -> "!=";
operator_to_string('==') -> "=";
operator_to_string(Op) -> atom_to_list(Op).


-spec values_conditions(expression()) -> {[any()], expression()}.
values_conditions(Expr) ->
  {Values, CleanExprs, _} = values_conditions(Expr, {[], [], 1}),
  {lists:reverse(Values), lists:reverse(CleanExprs)}.

values_conditions(Exprs, Acc) when is_list(Exprs) ->
  lists:foldl(fun values_conditions/2, Acc, Exprs);

values_conditions({LogicalOp, Exprs}, {Values, CleanExprs, Count})
  when (LogicalOp == 'and')
       or (LogicalOp == 'or')
       or (LogicalOp == 'not') ->
  {NewValues, NewCleanExprs, NewCount} = values_conditions(Exprs, {Values, [], Count}),
  {NewValues,[{LogicalOp, lists:reverse(NewCleanExprs)} | CleanExprs],NewCount};
values_conditions({Name, Op, Value}, {Values, CleanExprs, Count}) when not is_atom(Value) ->
  check_operator(Op),
  {[Value | Values],[{Name, Op, {'?', Count}} | CleanExprs],Count + 1};
values_conditions({Name1, Op, Name2}, {Values, CleanExprs, Count}) when is_atom(Name2) ->
  check_operator(Op),
  {Values,[{Name1, Op, Name2} | CleanExprs],Count};
values_conditions({Name, Value}, {Values, CleanExprs, Count}) when Value =/= 'null', Value =/= 'not_null' ->
  {[Value | Values],[{Name, {'?', Count}} | CleanExprs],Count + 1};
values_conditions({Name, Value}, {Values, CleanExprs, Count}) ->
  {Values,[{Name, Value} | CleanExprs],Count};
values_conditions([], Acc) ->
  Acc;
values_conditions(Expr, _) ->
  throw({unsupported_expression, Expr}).

-spec where_clause(expression(), fun(), fun()) -> iodata().
where_clause([], _EscapeFun, _SlotFun) ->
  [];
where_clause(Exprs, EscapeFun, SlotFun) when is_list(Exprs) ->
  Clauses = [where_clause(Expr, EscapeFun, SlotFun) || Expr <- Exprs],
  ["(", interpose(" AND ", Clauses), ")"];
where_clause({'and', Exprs}, EscapeFun, SlotFun) ->
  where_clause(Exprs, EscapeFun, SlotFun);
where_clause({'or', Exprs}, EscapeFun, SlotFun) ->
  Clauses = [where_clause(Expr, EscapeFun, SlotFun) || Expr <- Exprs],
  ["(", interpose(" OR ", Clauses), ")"];
where_clause({'not', Expr}, EscapeFun, SlotFun) ->
  [" NOT ", "(", where_clause(Expr, EscapeFun, SlotFun), ")"];
where_clause({Name, Op, {'?', _} = Slot}, EscapeFun, SlotFun) ->
  [EscapeFun(Name), " ", operator_to_string(Op), SlotFun(Slot)];
where_clause({Name1, Op, Name2}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name1), " ", operator_to_string(Op), " ", single_quote(Name2)];
where_clause({Name,  {'?', _} = Slot}, EscapeFun, SlotFun) ->
  [EscapeFun(Name), " = ", SlotFun(Slot)];
where_clause({Name, 'null'}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name), " IS NULL "];
where_clause({Name, 'not_null'}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name), " IS NOT NULL "].

order_by_clause(SortFields, EscapeFun) ->
  ClauseFun = fun({Name, SortOrder}) ->
                  [EscapeFun(atom_to_list(Name)), " ", atom_to_list(SortOrder)]
              end,
  Clauses = lists:map(ClauseFun, SortFields),
  [" ORDER BY ", interpose(", ", Clauses)].
