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

         %% Support for querying by records
         attributes/1,
         qlike/2, read_results/2]).
-include("xt_mapping.hrl").

%% Maybe run value through conversion function
-spec conv(undefined | function(), any()) -> any().
conv(undefined, Val) -> Val;
conv(Fun, Val) ->  Fun(Val).

%% @doc Convert Erlang record tuple to an XTDB document map using mapping
-spec to_doc(tuple(), #mapping{}) -> #{atom() => any()}.
to_doc(Record, #mapping{fields = Fields}) ->
    lists:foldl(fun(#field{attr=Name,field=Field,to_xtdb=Conv,required=Req}, Doc) ->
                        case conv(Conv, element(Field, Record)) of
                            undefined ->
                                if Req -> throw({required_field_missing, Field, attr, Name});
                                   true -> Doc
                                end;
                            Val -> maps:put(Name, Val, Doc)
                        end;
                   (#conversion{record_to_xtdb=Write}, Doc) -> Write(Record, Doc);
                   (#embed{field=Field, mapping=EmbedMapping}, Doc) ->
                        case element(Field, Record) of
                            undefined -> Doc;
                            E -> maps:merge(Doc, to_doc(E, EmbedMapping))
                        end;
                   (#static{attr=Attr,value=Val}, Doc) ->
                           maps:put(Attr, Val, Doc)
                end,
                #{}, Fields).

%% @doc Convert an XTDB document map (as gotten by pull) to an Erlang record tuple using mapping
-spec to_rec(#{atom() => any()}, #mapping{}) -> tuple().
to_rec(Doc, #mapping{empty = Empty, fields = Fields}) ->
    lists:foldl(fun(#field{attr=Name,field=Field,from_xtdb=Conv}, Rec) ->
                        setelement(Field, Rec, conv(Conv, maps:get(Name, Doc, undefined)));
                   (#conversion{xtdb_to_record=Read}, Rec) -> Read(Doc,Rec);
                   (#embed{field=Field, mapping=EmbedMapping}, Rec) ->
                        case to_rec(Doc, EmbedMapping) of
                            %% Embedded had no values (same as empty), don't set it
                            None when None == EmbedMapping#mapping.empty -> Rec;

                            %% Embedded had some values
                            Val -> setelement(Field, Rec, Val)
                        end;
                   (#static{}, Rec) -> Rec

                end,
                Empty, Fields).


%% @doc Create an ':xt/id' mapping for a Field as Key.
-spec idmap(integer(), atom()) -> #conversion{}.
idmap(Field, Key) ->
    #conversion{
       attrs = [':xt/id'],

       %% Read function to set id field when creating record from doc
       xtdb_to_record =
           fun(Doc,Rec) ->
                   case maps:get(':xt/id', Doc, undefined) of
                       undefined -> Rec;
                       IdMap when is_map(IdMap) ->
                           Id = maps:get(Key, IdMap),
                           setelement(Field, Rec, Id);
                       Else ->
                           logger:warning("Encountered invalid :xt/id value, not a map: ~p", [Else]),
                           setelement(Field, Rec, Else)
                   end
           end,
       %% Write function to set doc :xt/id when creating doc from record
       record_to_xtdb =
           fun(Rec,Doc) ->
                   case element(Field, Rec) of
                       undefined -> Doc;
                       Val -> maps:put(':xt/id', #{Key => Val}, Doc)
                   end
           end}.

%% @doc Normal field mapping without any special conversion
field(Attr, Field) -> field(Attr, Field, undefined, undefined).

%% @doc Field with special mapping to and from XTDB values.
%% ToXTDB is called to convert the fields value when sending to XTDB and
%% FromXTDB is called before setting values received form XTDB to a record.
field(Attr, Field, ToXTDB, FromXTDB) ->
    %% Check that attribute name starts with ':' (clj keywords)
    AttrName = case atom_to_list(Attr) of
                   [$: | _] -> Attr;
                   Name -> list_to_atom([$: | Name])
               end,
    #field{attr = AttrName, field = Field,
           to_xtdb = ToXTDB,
           from_xtdb = FromXTDB}.

%% @doc Create a field that is stored as a local date
local_date(Attr, Field) ->
    field(Attr, Field,
          fun(undefined) -> undefined;
             ({Year,Month,Day}) -> {local_date, Year, Month, Day}
          end,
          fun(undefined) -> undefined;
             ({local_date, Year, Month, Day}) -> {Year, Month, Day}
          end).

%% @doc Create a field that is stored as a date and time
local_datetime(Attr,Field) ->
    field(Attr, Field,
          fun(undefined) -> undefined;
             ({{Year,Month,Day},{Hour,Minute,Second}}) -> {local_datetime, Year,Month,Day,Hour,Minute,Second}
          end,
          fun(undefined) -> undefined;
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
-spec embed(#mapping{}, integer()) -> #embed{}.
-spec embed(atom(), #mapping{}, integer()) -> #embed{}.
embed(Mapping, Field) -> #embed{mapping = Mapping, field = Field}.
embed(Prefix, #mapping{fields=Fields}=Mapping, Field) ->
    embed(Mapping#mapping{
            fields =
                [ prefix_name(Prefix,F) || F <- Fields ]}, Field).

prefix_name(Prefix,#field{attr=Attr}=F) ->
    PrefixStr = atom_to_list(Prefix),
    PrefixedAttr = list_to_atom(
                     ":" ++ PrefixStr ++
                         string:sub_string(atom_to_list(Attr), 2)),
    F#field{attr=PrefixedAttr};
prefix_name(_,M) -> M.


%% @doc Register mapping in global registry.
register(#mapping{empty=Rec}=Mapping) ->
    persistent_term:put({xt_mapping,element(1,Rec)}, Mapping).

%% @doc Get a mapping from the global registry.
get(RecordType) ->
    persistent_term:get({xt_mapping,RecordType}).

%% @doc Return all XTDB attributes specified in the mapping. Uses an empty record instance.
attributes(#mapping{fields = Fields}) ->
    lists:foldl(fun attributes/2, [],  Fields).

attributes(#field{attr=A}, Acc) -> [A | Acc];
attributes(#conversion{attrs=Attrs}, Acc) -> Attrs ++ Acc;
attributes(#embed{mapping=M}, Acc) -> attributes(M) ++ Acc;
attributes(#static{}, Acc) -> Acc.

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

qlike_where(Attr, Conv, Val, WhereIn) ->
    case Val of
        {'<', Val1} -> qlike_op('<', Attr, conv(Conv, Val1), WhereIn);
        {'<=', Val1} -> qlike_op('<', Attr, conv(Conv,Val1), WhereIn);
        {'>', Val1} -> qlike_op('>', Attr, conv(Conv, Val1), WhereIn);
        {'>=', Val1} -> qlike_op('>=', Attr, conv(Conv, Val1), WhereIn);
        {'between', Low, High} -> qlike_between(Attr, conv(Conv,Low), conv(Conv,High), WhereIn);
        {'textsearch', Term} -> qlike_textsearch(Attr, conv(Conv,Term), WhereIn);
        _ -> qlike_eq(Attr, conv(Conv,Val), WhereIn)
    end.

where_in(WhereIn0, Candidate, Mapping) ->
    lists:foldl(
      fun(#field{attr=Name,field=Field,to_xtdb=Conv,required=Req}, WhereIn) ->
              Val = element(Field, Candidate),
              case Val of
                  undefined ->
                      if Req -> qlike_req(Name,WhereIn);
                         true -> WhereIn
                      end;
                  _ -> qlike_where(Name, Conv, Val, WhereIn)
              end;
         (#embed{field=Field,mapping=EmbedMapping}, WhereIn) ->
              case element(Field, Candidate) of
                  undefined -> WhereIn;
                  EmbedCandidate ->
                      where_in(WhereIn, EmbedCandidate, EmbedMapping)
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
%%  <li><code>{between,Low,High}</code> between low (inclusive) and high (inclusive)
%%  <li><code>{textsearch,Term}</code>  Lucene text search</li>
%% </ul>
%%
%% The comparison operators can be used on any field type including
%% numbers, strings, dates and so on. But for better text searching
%% it is better to the <code>textsearch</code> operator.
%% Returns a tuple containing the datalog query {Find, Where, In}.
%% @see xt_lucene
qlike(Candidate, Mapping) ->
    {Where,In} = where_in({[],[]}, Candidate, Mapping),
    Find = [[pull,qlike,attributes(Mapping)]],
    {Find, Where, In}.

read_results(Results, Mapping) ->
    [to_rec(R, Mapping) || [R] <- Results].
