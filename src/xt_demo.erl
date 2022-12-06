-module(xt_demo).
-compile(export_all).

%% Demo how to use XTDB from Erlang, with mapping Erlang records to XTDB documents

-record(person, {person_id :: string(),
                 first_name :: string(),
                 last_name :: string(),
                 email :: string()}).

person_mapping() ->
    xt_mapping:mapping(
      #person{},
      [xt_mapping:idmap(#person.person_id, ':person'),
       {':person/first-name', #person.first_name},
       {':person/last-name', #person.last_name},
       {':person/email', #person.email}]).

person(Id,Fn,Ln,Email) ->
    #person{person_id = Id, first_name = Fn, last_name = Ln, email = Email}.

put(Person) ->
    xt:put(xt_mapping:to_doc(Person, person_mapping())).

qlike(Person) ->
    xt_mapping:qlike(Person, person_mapping()).


add_persons() ->
    put(person("01234-abc", "Max", "Feedpressure", "max@example.com")),
    put(person("42069-xxx", "Foo", "Barsky", "foobar@example.com")),
    put(person("666100-333", "Nathaniel", "Backpressure", "nate@example.com")),
    put(person("590823-as1", "Eric", "Lang", "er.lang@erlang.org")).
