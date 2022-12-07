%% @doc Mapping from records to XTDB documents
-module(xt_mapping).
-export([to_doc/2, to_rec/2, mapping/2, idmap/2, attributes/1, qlike/2]).
-include("xt_mapping.hrl").

%% @doc Convert Erlang record tuple to an XTDB document map using mapping
to_doc(Record, #mapping{fields = Fields}) ->
    lists:foldl(fun({Name,Field}, Doc) -> maps:put(Name, element(Field, Record), Doc);
                   ({_Name,_,Write}, Doc) -> Write(Record, Doc)
                end,
                #{}, Fields).

%% @doc Convert an XTDB document map (as gotten by pull) to an Erlang record tuple using mapping
to_rec(Doc, #mapping{empty = Empty, fields = Fields}) ->
    lists:foldl(fun({Name,Field}, Rec) -> setelement(Field, Rec, maps:get(Name, Doc, undefined));
                   ({_,Read,_}, Rec) -> Read(Doc,Rec)
                end,
                Empty, Fields).


%% @doc Create an ':xt/id' mapping for a Field as Key.
idmap(Field, Key) ->
    {':xt/id',

     %% Read function to set id field when creating record from doc
     fun(Doc,Rec) ->
             IdMap = maps:get(':xt/id', Doc),
             Id = maps:get(Key, IdMap),
             setelement(Field, Rec, Id)
     end,

     %% Write function to set doc :xt/id when creating doc from record
     fun(Rec,Doc) ->
             maps:put(':xt/id', #{ Key => element(Field, Rec) }, Doc)
     end}.

%% @doc Normal field mapping without any special conversion
field(Attr, Field) -> #field{attr = Attr, field = Field}.

%% @doc Field with special mapping to and from XTDB values.
%% ToXTDB is called to convert the fields value when sending to XTDB and
%% FromXTDB is called before setting values received form XTDB to a record.
field(Attr, Field, ToXTDB, FromXTDB) ->
    #field{attr = Attr, field = Field,
           to_xtdb = ToXTDB,
           from_xtdb = FromXTDB}.

%% @doc Create a field that is stored as a local date
local_date(Attr, Field) ->
    #field{attr = Attr, field = Field,
           to_xtdb = fun(undefined) -> undefined;
                        ({Year,Month,Day} -> {local_date, Year, Month, Day})
                        end,
           from_xtdb = fun(undefined) -> undefined;
                          ({local_date, Year, Month, Day}) -> {Year, Month, Day}
                       end}.

%% @doc Create a field that is stored as a date and time
local_datetime(Attr,Field) ->
    #field{attr = Attr, field = Field,
           to_xtdb = fun(undefined) -> undefined;
                        ({{Year,Month,Day},{Hour,Minute,Second}}) -> {local_datetime, Year,Month,Day,Hour,Minute,Second}
                     end,
           from_xtdb = fun(undefined) -> undefined;
                          ({local_datetime, Y, M, D, H, Mi, S})  ->
                                  {{Y,M,D}, {H,Mi,S}}
                       end}.





%% @doc Create a record mapping
mapping(EmptyRecordValue, FieldMappings) ->
    #mapping{empty = EmptyRecordValue,
             fields = FieldMappings}.

%% @doc Return all XTDB attributes specified in the mapping. Uses an empty record instance.
attributes(#mapping{empty=Empty} = M) ->
    maps:keys(to_doc(Empty, M)).


next_param(In) ->
    list_to_atom( "p" ++ integer_to_list(length(In)) ).

next_where(Where) ->
    list_to_atom( "w" ++ integer_to_list(length(Where)) ).

qlike_op(Op, Attr, Val, {Where, In}) ->
    NextParam = next_param(In),
    NextWhere = next_where(Where),
    {Where ++ [[qlike, Attr, NextWhere],
               [{Op, NextWhere, NextParam}]],
     [{NextParam, Val} | In]}.

qlike_eq(Attr,Val,{Where,In}) ->
    NextParam = next_param(In),
    {Where ++ [[qlike, Attr, NextParam]],
     [{NextParam, Val} | In]}.

qlike_textsearch(Attr, Term, {Where,In}) ->
    NextParam = next_param(In), %% prepare lucene search string
    {Where ++ [[{'text-search', Attr, NextParam}, [[qlike]]]],
     [{NextParam, Term} | In]}.

qlike_where(Attr, Val, WhereIn) ->
    case Val of
        {'<', Val1} -> qlike_op('<', Attr, Val1, WhereIn);
        {'<=', Val1} -> qlike_op('<', Attr, Val1, WhereIn);
        {'>', Val1} -> qlike_op('>', Attr, Val1, WhereIn);
        {'>=', Val1} -> qlike_op('>=', Attr, Val1, WhereIn);
        {'textsearch', Term} -> qlike_textsearch(Attr, Term, WhereIn);
        _ -> qlike_eq(Attr, Val, WhereIn)
    end.

%% @doc Search instances by providing a candidate record. Record values may be
%% direct values to match or tuples containing {op, Val} where op is one of
%% the supported operations:
%% - <, <=, >, >=  range predicate operators
%% - textsearch    search using Lucene index
qlike(Candidate, Mapping) ->
    {Where,In} = lists:foldl(fun({Name,Field}, WhereIn) ->
                                     Val = element(Field, Candidate),
                                     case Val of
                                         undefined -> WhereIn;
                                         _ -> qlike_where(Name, Val, WhereIn)
                                     end;
                                (_, WhereIn) -> WhereIn
                             end,
                             {[],[]},
                             Mapping#mapping.fields),
    {ok, Results} = xt:q([[pull,qlike,attributes(Mapping)]],
                         Where, In),
    [to_rec(R, Mapping) || [R] <- Results].
