-module(xt_demo).
-compile(export_all).

%% Demo how to use XTDB from Erlang, with mapping Erlang records to XTDB documents

-record(person, {person_id :: string(),
                 first_name :: string(),
                 last_name :: string(),
                 email :: string(),
                 date_of_birth :: calendar:date()}).

person_mapping() ->
    xt_mapping:mapping(
      #person{},
      [xt_mapping:idmap(#person.person_id, ':person'),
       xt_mapping:field(':person/first-name', #person.first_name),
       xt_mapping:field(':person/last-name', #person.last_name),
       xt_mapping:field(':person/email', #person.email),
       xt_mapping:local_date(':person/date-of-birth', #person.date_of_birth)]).
person(Id,Fn,Ln,Email,Dob) ->

    #person{person_id = Id,
            first_name = Fn, last_name = Ln,
            email = Email,
            date_of_birth = Dob}.

put(Person) ->
    xt:put(xt_mapping:to_doc(Person, person_mapping())).

qlike(Person) ->
    xt_mapping:qlike(Person, person_mapping()).


add_persons() ->
    put(person("01234-abc", "Max", "Feedpressure", "max@example.com", {1981, 4, 8})),
    put(person("42069-xxx", "Foo", "Barsky", "foobar@example.com", {1970, 1, 1})),
    put(person("666100-333", "Nathaniel", "Backpressure", "nate@example.com", {1995,3,6})),
    put(person("123123-333", "Eric", "Lang", "er.lang@erlang.org", {1977, 7, 7})),
    put(person("234234-444", "Matti", "Korhonen", "matti@notreallyexisting.org", {1969, 12, 20})),
    put(person("345345-555", "John", "Doe", "jd@imaginaryjd.com", {2000, 3, 4})),
    put(person("456456-666", "Annika", "Andersson", "aa@existerar-inte.se", {1986, 5, 27})).
