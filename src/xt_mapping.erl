%% @doc Mapping from records to XTDB documents
-module(xt_mapping).
-export([%% Basic conversion to and from documents
         to_doc/2, to_rec/2,

         %% Mapping registry
         register/1, get/1,

         %% Functions for creating mappings
         mapping/2, idmap/2, embed/2, embed/3,
         field/2, field/4, local_date/2, local_datetime/2,
         required/1, static/2,
         link/4, link/5,

         %% Support for querying by records
         attributes/1,
         qlike/3, read_results/2]).
-include("xt_mapping.hrl").

%% Maybe run value through conversion function
-spec conv(undefined | function(), any()) -> any().
conv(undefined, Val) -> Val;
conv(Fun, Val) ->  Fun(Val).

-type rec_or_struct() :: tuple() | map().
-type field_ref() :: integer() | atom().

%% Abstract over Erlang record and Elixir struct differences:
%% - get_value/2  gets value (either from record or struct)
%% - set_value/3  sets value into record tuple or struct map
%% - undefined_value/1  determines wether to use undefined/nil as the "missing" value

get_value(#field{field=Field,key=undefined}, Record) -> element(Field, Record);
get_value(#field{field=undefined,key=Key}, Struct) -> maps:get(Key, Struct);
get_value(#embed{field=Field,key=undefined}, Record) -> element(Field, Record);
get_value(#embed{field=undefined,key=Key}, Struct) -> maps:get(Key, Struct);
get_value(Field, Record) when is_integer(Field) andalso is_tuple(Record) ->
    element(Field, Record);
get_value(Key, Struct) when is_atom(Key) andalso is_map(Struct) ->
    maps:get(Key, Struct).

set_value(#field{field=Field,key=undefined}, Val, Record) ->
    setelement(Field, Record, Val);
set_value(#field{field=undefined,key=Key}, Val, Struct) ->
    maps:put(Key, Val, Struct);
set_value(#embed{field=Field,key=undefined}, Val, Record) ->
    setelement(Field, Record, Val);
set_value(#embed{field=undefined,key=Key}, Val, Struct) ->
    maps:put(Key, Val, Struct);
set_value(#link{field=F,key=K}, Val, To) ->
    set_value({F,K}, Val, To);
set_value({Field,undefined}, Val, Record) -> set_value(Field, Val, Record);
set_value({undefined,Key}, Val, Struct) -> set_value(Key, Val, Struct);
set_value(Field, Val, Record) when is_integer(Field) andalso is_tuple(Record) ->
    setelement(Field, Record, Val);
set_value(Key, Val, Struct) when is_atom(Key) andalso is_map(Struct) ->
    maps:put(Key, Val, Struct).

%% Get undefined value, Erlang uses undefined atom in records
%% and Elixir structs have nil atom
undefined_value(#field{field=F,key=K}) ->
    if K == undefined -> undefined;
       F == undefined -> nil
    end;
undefined_value(#embed{field=F,key=K}) ->
    if K == undefined -> undefined;
       F == undefined -> nil
    end;
undefined_value(N) when is_integer(N) -> undefined;
undefined_value(K) when is_atom(K) -> nil.


%% @doc Convert Erlang record tuple or Elixir struct to an XTDB document map using mapping
-spec to_doc(rec_or_struct(), #mapping{}) -> #{atom() => any()}.
to_doc(Record, #mapping{fields = Fields}) ->
    lists:foldl(
      fun(#field{attr=Name,to_xtdb=Conv,required=Req}=F, Doc) ->
              Undef = undefined_value(F),
              case conv(Conv, get_value(F, Record)) of
                  Undef ->
                      if Req -> throw({required_field_missing, F});
                         true -> Doc
                      end;
                  Val -> maps:put(Name, Val, Doc)
              end;
         (#conversion{record_to_xtdb=Write}, Doc) -> Write(Record, Doc);
         (#embed{mapping=EmbedMapping}=Embed, Doc) ->
              Undef = undefined_value(Embed),
              case get_value(Embed, Record) of
                  Undef -> Doc;
                  E -> maps:merge(Doc, to_doc(E, EmbedMapping))
              end;
         (#static{attr=Attr,value=Val}, Doc) ->
              maps:put(Attr, Val, Doc)
      end,
      #{}, Fields).


%% @doc Convert an XTDB document map (as gotten by pull) to an Erlang record tuple or Elixir struct using mapping
-spec to_rec(#{atom() => any()}, #mapping{}) -> tuple().
to_rec(Doc, #mapping{empty = Empty, fields = Fields}) ->
    lists:foldl(
      fun(#field{attr=Name,from_xtdb=Conv}=F, Rec) ->
              set_value(F, conv(Conv, maps:get(Name, Doc, undefined_value(F))), Rec);

         (#conversion{xtdb_to_record=Read}, Rec) -> Read(Doc,Rec);
         (#embed{mapping=EmbedMapping}=Embed, Rec) ->
              case to_rec(Doc, EmbedMapping) of
                  %% Embedded had no values (same as empty), don't set it
                  None when None == EmbedMapping#mapping.empty -> Rec;

                  %% Embedded had some values
                  Val -> set_value(Embed, Val, Rec)
              end;
         (#link{to=To, attr = Attr}=Link, Rec) ->
              LinkMapping = xt_mapping:get(To),
              case maps:get(Attr, Doc, undefined) of
                  undefined -> Rec;
                  [Vals] -> set_value(Link, [to_rec(V, LinkMapping)
                                             || V <- Vals], Rec);
                  Val -> set_value(Link, to_rec(Val, LinkMapping), Rec)
              end;
         (#static{}, Rec) -> Rec

      end,
      Empty, Fields).


%% @doc Create an ':xt/id' mapping for a Field as Key.
-spec idmap(integer() | atom(), atom()) -> #conversion{}.
idmap(FieldOrKey, Key) ->
    #conversion{
       attrs = [':xt/id'],

       %% Read function to set id field when creating record from doc
       xtdb_to_record =
           fun(Doc,Rec) ->
                   case maps:get(':xt/id', Doc, undefined) of
                       undefined -> Rec;
                       IdMap when is_map(IdMap) ->
                           Id = maps:get(Key, IdMap),
                           set_value(FieldOrKey, Id, Rec);
                       Else ->
                           logger:warning("Encountered invalid :xt/id value, not a map: ~p", [Else]),
                           set_value(FieldOrKey, Else, Rec)
                   end
           end,
       %% Write function to set doc :xt/id when creating doc from record
       record_to_xtdb =
           fun(Rec,Doc) ->
                   Undef = undefined_value(FieldOrKey),
                   case get_value(FieldOrKey, Rec) of
                       Undef -> Doc;
                       Val -> maps:put(':xt/id', #{Key => Val}, Doc)
                   end
           end}.

%% @doc Normal field mapping without any special conversion
field(Attr, FieldOrKey) -> field(Attr, FieldOrKey, undefined, undefined).

%% @doc Field with special mapping to and from XTDB values.
%% ToXTDB is called to convert the fields value when sending to XTDB and
%% FromXTDB is called before setting values received form XTDB to a record.
field(Attr, FieldOrKey, ToXTDB, FromXTDB) ->
    %% Check that attribute name starts with ':' (clj keywords)
    AttrName = case atom_to_list(Attr) of
                   [$: | _] -> Attr;
                   Name -> list_to_atom([$: | Name])
               end,
    {Field, Key} = case FieldOrKey of
                       F when is_integer(F) -> {F, undefined};
                       K when is_atom(K) -> {undefined, K}
                   end,
    #field{attr = AttrName, field = Field, key = Key,
           to_xtdb = ToXTDB, from_xtdb = FromXTDB}.

%% @doc Create a field that is stored as a local date
local_date(Attr, FieldOrKey) ->
    Undef = undefined_value(FieldOrKey),
    field(Attr, FieldOrKey,
          fun(U) when U == Undef -> undefined;
             ({Year,Month,Day}) -> {local_date, Year, Month, Day}
          end,
          fun(undefined) -> Undef;
             ({local_date, Year, Month, Day}) -> {Year, Month, Day}
          end).

%% @doc Create a field that is stored as a date and time
local_datetime(Attr,FieldOrKey) ->
    Undef = undefined_value(FieldOrKey),
    field(Attr, FieldOrKey,
          fun(U) when U == Undef -> undefined;
             ({{Year,Month,Day},{Hour,Minute,Second}}) -> {local_datetime, Year,Month,Day,Hour,Minute,Second}
          end,
          fun(undefined) -> Undef;
             ({local_datetime, Y, M, D, H, Mi, S})  ->
                  {{Y,M,D}, {H,Mi,S}}
          end).

%% @doc Make field required. This affects queries, only documents that
%% have a value for this attribute will be considered. The value can be
%% anything (including nil).
required(Field) ->
    Field#field{required=true}.

%% @doc Create an attribute that has a static value. Every document
%% created will have this attribute added with the given value. Queries
%% will only return documents where this value is the correct value.
%% The value will not be part of the Erlang record.
%%
%% As document the database is untyped, this is useful if you need to
%% encode a "type" attribute for the documents if there are no required
%% fields that discriminate it from all other documents.
static(Attr, Value) ->
    #static{attr=Attr, value=Value}.

%% @doc Create a record mapping
mapping(EmptyRecordValue, FieldMappings) ->
    #mapping{empty = EmptyRecordValue,
             fields = FieldMappings}.


%% @doc Create a field mapping that embeds another records information in the same document.
%% This is convenient to not need shredding the documents, simply use the same document for
%% all attributes. You can optionally specify prefix to apply for the embedded fields (for
%% example when embedding multiple records of the same type, like shipping and billing address).
%%
%% The prefix, if any, is applied to the start of the name, so if we have a record mapping for address
%% that maps ':address/postal-code' and so on and we embed two records with shipping- and billing-
%% prefixes, we will get ':shipping-address/postal-code' and ':billing-address/postal-code' attributes
%% in the document.
-spec embed(#mapping{}, field_ref()) -> #embed{}.
-spec embed(atom(), #mapping{}, field_ref()) -> #embed{}.
embed(Mapping, FieldOrKey) ->
    {Field,Key} = case FieldOrKey of
                      F when is_integer(F) -> {F, undefined};
                      K when is_atom(K) -> {undefined, K}
                  end,
    #embed{mapping = Mapping, field = Field, key = Key}.
embed(Prefix, #mapping{fields=Fields}=Mapping, FieldOrKey) ->
    embed(Mapping#mapping{
            fields =
                [ prefix_name(Prefix,F) || F <- Fields ]}, FieldOrKey).

%% @doc Create a field mapping that links to another document or documents
-spec link(atom(), rec_or_struct(), field_ref(), many | one) -> #link{}.
link(Attr, To, FieldOrKey, Cardinality) ->
    link(Attr, To,FieldOrKey, Cardinality, false).
link(Attr, To, FieldOrKey, Cardinality, Owned) ->
    {Field,Key} = case FieldOrKey of
                      F when is_integer(F) -> {F,undefined};
                      K when is_atom(K) -> {undefined, K}
                  end,
    #link{attr = Attr,
          to = To,
          field = Field, key = Key,
          owned = Owned,
          cardinality = Cardinality}.


prefix_name(Prefix,#field{attr=Attr}=F) ->
    PrefixStr = atom_to_list(Prefix),
    PrefixedAttr = list_to_atom(
                     ":" ++ PrefixStr ++
                         string:sub_string(atom_to_list(Attr), 2)),
    F#field{attr=PrefixedAttr};
prefix_name(_,M) -> M.


type(Rec) when is_tuple(Rec) ->
    element(1,Rec);
type(#{ '__struct__' := Type }) ->
    Type.


%% @doc Register mapping in global registry.
register(#mapping{empty=Empty}=Mapping) ->
    persistent_term:put({xt_mapping,type(Empty)}, Mapping).

%% @doc Get a mapping from the global registry.
get(RecordType) ->
    persistent_term:get({xt_mapping,type(RecordType)}).

%% @doc Return all XTDB attributes specified in the mapping. Uses an empty record instance.
attributes(#mapping{fields = Fields}) ->
    lists:foldl(fun attributes/2, [],  Fields).

attributes(#field{attr=A}, Acc) -> [A | Acc];
attributes(#conversion{attrs=Attrs}, Acc) -> Attrs ++ Acc;
attributes(#embed{mapping=M}, Acc) -> attributes(M) ++ Acc;
attributes(#static{}, Acc) -> Acc;
attributes(#link{attr=Attr,to=To}, Acc) ->
    [#{Attr => attributes(xt_mapping:get(To))} | Acc].

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

qlike_req(Attr,{Where,In}) ->
    %% Add required filed, just [qlike :some-attr] without value
    {[[qlike, Attr] | Where], In}.

qlike_textsearch(Attr, Term, {Where,In}) ->
    NextParam = next_param(In), %% prepare lucene search string
    {Where ++ [[{'text-search', Attr, NextParam}, [[qlike]]]],
     [{NextParam, Term} | In]}.

qlike_between(Attr, Low, High, {Where, In}) ->
    NextWhere = next_where(Where),
    NextParam = next_param(In),
    LowParam = list_to_atom(atom_to_list(NextParam) ++ "lo"),
    HighParam = list_to_atom(atom_to_list(NextParam) ++ "hi"),
    {Where ++ [[qlike, Attr, NextWhere],
               [{'>=', NextWhere, LowParam}],
               [{'<=', NextWhere, HighParam}]],
     [{LowParam, Low}, {HighParam, High} | In]}.

qlike_in(Attr, Values, {Where, In}) ->
    NextParam = next_param(In),
    {Where ++ [[qlike, Attr, NextParam]],
     [{[NextParam, '...'], Values} | In]}.

qlike_where(Attr, Conv, Val, WhereIn) ->
    case Val of
        {'<', Val1} -> qlike_op('<', Attr, conv(Conv, Val1), WhereIn);
        {'<=', Val1} -> qlike_op('<', Attr, conv(Conv,Val1), WhereIn);
        {'>', Val1} -> qlike_op('>', Attr, conv(Conv, Val1), WhereIn);
        {'>=', Val1} -> qlike_op('>=', Attr, conv(Conv, Val1), WhereIn);
        {'between', Low, High} -> qlike_between(Attr, conv(Conv,Low), conv(Conv,High), WhereIn);
        {'textsearch', Term} -> qlike_textsearch(Attr, conv(Conv,Term), WhereIn);
        {in, Values} -> qlike_in(Attr, lists:map(fun(V) -> conv(Conv,V) end,
                                                 Values), WhereIn);
        _ -> qlike_eq(Attr, conv(Conv,Val), WhereIn)
    end.

where_in(WhereIn0, Candidate, Mapping) ->
    lists:foldl(
      fun(#field{attr=Name,to_xtdb=Conv,required=Req}=F, WhereIn) ->
              Val = get_value(F, Candidate),
              Undef = undefined_value(F),
              if Val == Undef ->
                      if Req -> qlike_req(Name,WhereIn);
                         true -> WhereIn
                      end;
                 true -> qlike_where(Name, Conv, Val, WhereIn)
              end;
         (#embed{mapping=EmbedMapping}=Embed, WhereIn) ->
              Val = get_value(Embed, Candidate),
              Undef = undefined_value(Embed),
              if Val == Undef -> WhereIn;
                 true -> where_in(WhereIn, Val, EmbedMapping)
              end;
         (#static{attr=Attr,value=Val}, WhereIn) ->
              qlike_eq(Attr, Val, WhereIn);
         (#conversion{record_to_xtdb = RecordToXtdb}, WhereIn) ->
              %% Run conversion on an empty map and add any attrs
              maps:fold(fun(Attr,Val,Acc) -> qlike_eq(Attr, Val, Acc) end,
                        WhereIn,
                        RecordToXtdb(Candidate, #{}));
         (_, WhereIn) -> WhereIn
      end,
      WhereIn0, Mapping#mapping.fields).

%% @doc Generate a query to search instances matching a candidate record.
%% Record values may be direct values to match or tuples containing {op, ...args}
%% where op is one of the supported operations:
%% <ul>
%%  <li><code>{'&lt;', Val}</code> less than</li>
%%  <li><code>{'&lt;=', Val}</code> less than or equals</li>
%%  <li><code>{'&gt;', Val}</code> greater than</li>
%%  <li><code>{'&gt;=', Val}</code> greater than or equals</li>
%%  <li><code>{between,Low,High}</code> between low (inclusive) and high (inclusive)</li>
%%  <li><code>{textsearch,Term}</code>  Lucene text search</li>
%%  <li><code>{in,[Val1,...,ValN]}</code>  value is in given list</li>
%% </ul>
%%
%% The comparison operators can be used on any field type including
%% numbers, strings, dates and so on. But for better text searching
%% it is better to the <code>textsearch</code> operator.
%% Returns a list containing options for `xt:q/1` query.
%%
%% Supports order_by option that uses the mapping. Other options
%% are passed through as is.
%%
%% @see xt_lucene
qlike(Candidate, Mapping, OptionList) ->
    Options1 = orddict:from_list(OptionList),
    {Where0,In} = where_in({[],[]}, Candidate, Mapping),
    Find0 = [[pull,qlike,
              case orddict:find(fetch, Options1) of
                  error -> attributes(Mapping);
                  {ok, Fetch} -> fetch(Mapping, Fetch, [':xt/id'])
              end]],
    {Find, Where, Options2} =
        case orddict:is_key(order_by, Options1) of
            true -> add_order(Mapping, Find0, Where0, Options1);
            false -> {Find0, Where0, Options1}
        end,
    lists:foldl(fun({Key, Value}, Opts) ->
                        orddict:store(Key, Value, Opts) end,
                Options2,
                [{find, Find},
                 {where, Where},
                 {in, In}]).

find_attr(#mapping{fields=Fields}, Field) ->
    find_attr(Fields, Field);
find_attr([#field{field = Field, attr = Attr}|_], Field) -> Attr;
find_attr([_|Fields], Field) -> find_attr(Fields, Field);
find_attr(_, Field) -> throw({no_attr_for_field, Field}).

%% FIXME: support embedded records in where
add_order(Mapping, Find0, Where0, Options) ->
    Order0 = orddict:fetch(order_by, Options),
    %%io:format("order0 ~p~n", [Order0]),
    {FindOut, WhereOut, Order} =
        lists:foldl(
          fun(I, {Find, Where, Order}) ->
                  {Field, Dir} = lists:nth(I, Order0),
                  Name = list_to_atom("_o"++integer_to_list(I)),
                  {Find ++ [Name],
                   Where ++ [[qlike, find_attr(Mapping, Field), Name]],
                   Order ++ [[Name, case Dir of
                                        asc -> ':asc';
                                        desc -> ':desc' end]]}
          end,
          {Find0, Where0, []},
         lists:seq(1, length(Order0))),
    OptionsOut = orddict:store(order_by, Order, Options),
    {FindOut, WhereOut, OptionsOut}.

read_results(Results, Mapping) ->
    [to_rec(hd(R), Mapping) || R <- Results].

mapping_for(Mapping, Field) ->
    case lists:search(fun(#field{field=F1}) when F1 == Field -> true;
                         (#embed{field=F1}) when F1 == Field -> true;
                         (_) -> false end,
                      Mapping#mapping.fields) of
        {value, Fm} -> Fm;
        false -> throw({no_field_mapping_for, Field, in, Mapping})
    end.

fetch(_, [], Acc) -> Acc;
fetch(M, [F|Fields], Acc) when is_integer(F) ->
    #field{attr=Attr} = mapping_for(M, F),
    fetch(M, Fields, [Attr | Acc]);
fetch(M, [{Embed,EmbedFields}|Fields], Acc) ->
    #embed{mapping = EmbedMapping} = mapping_for(M, Embed),
    fetch(M, Fields, fetch(EmbedMapping, EmbedFields, Acc)).
