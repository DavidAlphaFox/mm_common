%%% @doc Small and simple SQL builder.
%%%
%%% Copyright 2012 Inaka &lt;hello@inaka.net&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(mm_sql_builder).

-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([insert/2, update/3, delete/2]).
-export([select/7, count/4]).

-export([
         values_conditions/1,
         where_clause/1,
         where_clause/2,
         where_clause/3,
         order_by_clause/1,
         order_by_clause/2
        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns number of results, useful for pagination.
-spec count(
  string(), [mm_sql_common:field()],mm_sql_common:condition(), string()
) -> {iolist(), [term()]}.
count(TableName, SelectFields, Conditions, ExtraWhere) ->
  {_Select, Where, WValues} =
    form_select_query(SelectFields, Conditions, ExtraWhere),
  {
    ["SELECT COUNT(1) AS `count` FROM ", mm_sql_common:escape(TableName), " WHERE ", Where],
    WValues
  }.

%% @doc Generic select function.
-spec select(
  string(), [mm_sql_common:field()], mm_sql_common:condition(), string(), non_neg_integer(),
  non_neg_integer(), string()
) -> {iolist(), [term()]}.
select(TableName, SelectFields, Conditions, ExtraWhere, Page, PageSize, OrderBy) ->
  Paging =
    [ " LIMIT ", integer_to_list((Page-1) * PageSize), ", ",
      integer_to_list(PageSize)],
  {Select, Where, WValues} =
    form_select_query(SelectFields, Conditions, ExtraWhere),
  {
    ["SELECT ", Select,
     " FROM ", mm_sql_common:escape(TableName),
     " WHERE ", Where, " ", OrderBy, " ", Paging],
    WValues
  }.

%% @doc INSERT.
-spec insert(atom() | string(), proplists:proplist()) -> {iodata(), [term()]}.
insert(TableName, Proplist) ->
  {Fields, Values, Args} = lists:foldr(
    fun({K, V}, {Fs, Vs, Args}) ->
      {[mm_sql_common:escape(K)|Fs], [V|Vs], ["?"|Args]}
    end,
    {[], [], []},
    Proplist
  ),
  {
    [
     "INSERT INTO ", mm_sql_common:escape(TableName), " (", string:join(Fields, ", "), ") ",
     "VALUES (", string:join(Args, ", "), ")"
    ],
    Values
  }.

%% @doc UPDATE.
-spec update(
  atom() | string(), proplists:proplist(), mm_sql_common:condition()
) -> {iodata(), [term()], [term()]}.
update(TableName, UpdateFields, Conditions) ->
  {_Select, Where, WValues} = form_select_query([], Conditions, ""),
  {UFields, UValues} = lists:foldr(
    fun({K, V}, {Fs, Vs}) ->
      {[mm_sql_common:escape(K) ++ "=?"|Fs], [V|Vs]}
    end,
    {[], []},
    UpdateFields
  ),
  Update = string:join(UFields, ","),
  {["UPDATE ", mm_sql_common:escape(TableName), " SET ", Update, " WHERE ", Where],
   UValues,
   WValues
  }.

%% @doc DELETE.
-spec delete(string(), mm_sql_common:condition()) -> {iolist(), [term()]}.
delete(TableName, Conditions) ->
  {_Select, Where, WValues} = form_select_query([], Conditions, ""),
  {["DELETE FROM ", mm_sql_common:escape(TableName), " WHERE ", Where], WValues}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Query generator.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec form_select_query([mm_sql_common:field()], mm_sql_common:condition(), string()) ->
  {string(), string(), [string()]}.
form_select_query(SelectFields, Conditions, ExtraWhere) ->
  {Values, CleanConditions} = values_conditions(Conditions),
  WhereTmp = where_clause(CleanConditions),
  SFields = [mm_sql_common:escape(F) || F <- SelectFields],
  Where = case ExtraWhere of
    [] -> WhereTmp;
    ExtraWhere ->
      [WhereTmp, 
	   case WhereTmp of
		   [] -> " ";
		   _ -> " AND "
	   end, 
	   ExtraWhere]
  end,
  Select = string:join(SFields, ","),
  % SelectedFields, Where clause, and Where values
  {Select, Where, Values}.

-spec values_conditions(mm_sql_common:expression()) ->
  {[any()], mm_sql_common:expression()}.
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
  mm_sql_common:check_operator(Op),
  {[Value | Values],[{Name, Op, {'?', Count}} | CleanExprs],Count + 1};
values_conditions({Name1, Op, Name2}, {Values, CleanExprs, Count}) when is_atom(Name2) ->
  mm_sql_common:check_operator(Op),
  {Values,[{Name1, Op, Name2} | CleanExprs],Count};
values_conditions({Name, Value}, {Values, CleanExprs, Count}) when Value =/= 'null', Value =/= 'not_null' ->
  {[Value | Values],[{Name, {'?', Count}} | CleanExprs],Count + 1};
values_conditions({Name, Value}, {Values, CleanExprs, Count}) ->
  {Values,[{Name, Value} | CleanExprs],Count};
values_conditions([], Acc) ->
  Acc;
values_conditions(Expr, _) ->
  throw({unsupported_expression, Expr}).

-spec where_clause(mm_sql_common:expression()) -> iodata().
where_clause(Exprs) ->
  where_clause(Exprs, fun mm_sql_common:escape/1, fun mm_sql_common:slot_question/1).

-spec where_clause(mm_sql_common:expression(), fun()) -> iodata().
where_clause(Exprs, EscapeFun) ->
  where_clause(Exprs, EscapeFun, fun mm_sql_common:slot_question/1).

-spec where_clause(mm_sql_common:expression(), fun(), fun()) -> iodata().
where_clause([], _EscapeFun, _SlotFun) ->
  [];
where_clause(Exprs, EscapeFun, SlotFun) when is_list(Exprs) ->
  Clauses = [where_clause(Expr, EscapeFun, SlotFun) || Expr <- Exprs],
  ["(", mm_sql_common:interpose(" AND ", Clauses), ")"];
where_clause({'and', Exprs}, EscapeFun, SlotFun) ->
  where_clause(Exprs, EscapeFun, SlotFun);
where_clause({'or', Exprs}, EscapeFun, SlotFun) ->
  Clauses = [where_clause(Expr, EscapeFun, SlotFun) || Expr <- Exprs],
  ["(", mm_sql_common:interpose(" OR ", Clauses), ")"];
where_clause({'not', Expr}, EscapeFun, SlotFun) ->
  [" NOT ", "(", where_clause(Expr, EscapeFun, SlotFun), ")"];
where_clause({Name, Op, {'?', _} = Slot}, EscapeFun, SlotFun) ->
  [EscapeFun(Name), " ", mm_sql_common:operator_to_string(Op), SlotFun(Slot)];
where_clause({Name1, Op, Name2}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name1), " ", mm_sql_common:operator_to_string(Op), " ", mm_sql_common:escape(Name2)];
where_clause({Name,  {'?', _} = Slot}, EscapeFun, SlotFun) ->
  [EscapeFun(Name), " = ", SlotFun(Slot)];
where_clause({Name, 'null'}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name), " IS NULL "];
where_clause({Name, 'not_null'}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name), " IS NOT NULL "].


order_by_clause(SortFields) ->
  order_by_clause(SortFields, fun mm_sql_common:escape/1).

order_by_clause(SortFields, EscapeFun) ->
  ClauseFun = fun({Name, SortOrder}) ->
                  [EscapeFun(atom_to_list(Name)), " ", atom_to_list(SortOrder)]
              end,
  Clauses = lists:map(ClauseFun, SortFields),
  [" ORDER BY ", mm_sql_common:interpose(", ", Clauses)].
