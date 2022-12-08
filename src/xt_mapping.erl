%% @doc Mapping from records to XTDB documents
-module(xt_mapping).
-export([to_doc/2, to_rec/2,
         mapping/2, idmap/2, embed/2, embed/3,
         field/2, field/4, local_date/2, local_datetime/2,
         attributes/1, qlike/2]).
-include("xt_mapping.hrl").

%% Maybe run value through conversion function
-spec conv(undefined | function(), any()) -> any().
conv(undefined, Val) -> Val;
conv(Fun, Val) ->  Fun(Val).

%% @doc Convert Erlang record tuple to an XTDB document map using mapping
-spec to_doc(tuple(), #mapping{}) -> #{atom() => any()}.
to_doc(Record, #mapping{fields = Fields}) ->
    lists:foldl(fun(#field{attr=Name,field=Field,to_xtdb=Conv}, Doc) ->
                        case conv(Conv, element(Field, Record)) of
                            undefined -> Doc;
                            Val -> maps:put(Name, Val, Doc)
                        end;
                   (#conversion{record_to_xtdb=Write}, Doc) -> Write(Record, Doc);
                   (#embed{field=Field, mapping=EmbedMapping}, Doc) ->
                        case element(Field, Record) of
                            undefined -> Doc;
                            E -> maps:merge(Doc, to_doc(E, EmbedMapping))
                        end
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
                        end

                end,
                Empty, Fields).


%% @doc Create an ':xt/id' mapping for a Field as Key.
-spec idmap(integer(), atom()) -> #conversion{}.
idmap(Field, Key) ->
    #conversion{
       attrs = [':xt/id'],

       %% Read function to set id field when creating record from doc
       xtdb_to_record = fun(Doc,Rec) ->
                                IdMap = maps:get(':xt/id', Doc),
                                Id = maps:get(Key, IdMap),
                                setelement(Field, Rec, Id)
                        end,
       %% Write function to set doc :xt/id when creating doc from record
       record_to_xtdb = fun(Rec,Doc) ->
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
                        ({Year,Month,Day}) -> {local_date, Year, Month, Day}
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


%% @doc Return all XTDB attributes specified in the mapping. Uses an empty record instance.
attributes(#mapping{fields = Fields}) ->
    lists:foldl(fun attributes/2, [],  Fields).

attributes(#field{attr=A}, Acc) -> [A | Acc];
attributes(#conversion{attrs=Attrs}, Acc) -> Attrs ++ Acc;
attributes(#embed{mapping=M}, Acc) -> attributes(M) ++ Acc.

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

qlike_where(Attr, Conv, Val, WhereIn) ->
    case Val of
        {'<', Val1} -> qlike_op('<', Attr, conv(Conv, Val1), WhereIn);
        {'<=', Val1} -> qlike_op('<', Attr, conv(Conv,Val1), WhereIn);
        {'>', Val1} -> qlike_op('>', Attr, conv(Conv, Val1), WhereIn);
        {'>=', Val1} -> qlike_op('>=', Attr, conv(Conv, Val1), WhereIn);
        {'textsearch', Term} -> qlike_textsearch(Attr, conv(Conv,Term), WhereIn);
        _ -> qlike_eq(Attr, conv(Conv,Val), WhereIn)
    end.

%% @doc Search instances by providing a candidate record. Record values may be
%% direct values to match or tuples containing {op, Val} where op is one of
%% the supported operations:
%% - <, <=, >, >=  range predicate operators
%% - textsearch    search using Lucene index
qlike(Candidate, Mapping) ->
    {Where,In} = lists:foldl(fun(#field{attr=Name,field=Field,to_xtdb=Conv}, WhereIn) ->
                                     Val = element(Field, Candidate),
                                     case Val of
                                         undefined -> WhereIn;
                                         _ -> qlike_where(Name, Conv, Val, WhereIn)
                                     end;
                                (_, WhereIn) -> WhereIn
                             end,
                             {[],[]},
                             Mapping#mapping.fields),
    {ok, Results} = xt:q([[pull,qlike,attributes(Mapping)]],
                         Where, In),
    [to_rec(R, Mapping) || [R] <- Results].
