-module(xt_demo).
-compile(export_all).

%% Demo how to use XTDB from Erlang, with mapping Erlang records to XTDB documents

-record(address, {street :: binary(),
                  city :: binary(),
                  zip :: binary(),
                  country :: binary()}).

-record(person, {person_id :: binary(),
                 first_name :: binary(),
                 last_name :: binary(),
                 email :: binary(),
                 date_of_birth :: calendar:date(),
                 shipping_address :: #address{},
                 billing_address :: #address{},
                 company :: tuple()}).

-record(company, {id_code :: binary(),
                  name :: binary(),
                  visiting_address :: #address{},
                  ceo :: #person{}}).

-define(M, xt_mapping).
register_mappings() ->
    Addr =
        ?M:mapping(
          #address{},
          [?M:field(':address/street', #address.street),
           ?M:field(':address/city', #address.city),
           ?M:field(':address/zip', #address.zip),
           ?M:field(':address/country', #address.country)]),
    Person =
        ?M:mapping(
          #person{},
          [?M:idmap(#person.person_id, ':person'),
           ?M:static(':type', ':person'),
           ?M:required(?M:field(':person/first-name', #person.first_name)),
           ?M:field(':person/last-name', #person.last_name),
           ?M:field(':person/email', #person.email),
           ?M:local_date(':person/date-of-birth', #person.date_of_birth),
           ?M:embed('shipping-', Addr, #person.shipping_address),
           ?M:embed('billing-', Addr, #person.billing_address),
           ?M:link(':person/company', #company{}, #person.company, one)]),
    Company =
        ?M:mapping(
           #company{},
           [?M:idmap(#company.id_code, ':company'),
            ?M:required(?M:field(':company/name', #company.name)),
            ?M:embed('visiting-', Addr, #company.visiting_address)]),

    ?M:register(Addr),
    ?M:register(Person),
    ?M:register(Company).


person(Id,Fn,Ln,Email,Dob,Billing,Shipping) ->
    #person{person_id = Id,
            first_name = Fn, last_name = Ln,
            email = Email,
            date_of_birth = Dob,
            billing_address = Billing,
            shipping_address = Shipping}.


init() ->
    register_mappings().

add_data() ->
    xt:put(
      [#person{person_id="01234-abc",
               first_name="Max", last_name="Feedpressure",
               email="max@example.com", date_of_birth={1981, 4, 8}},
       #person{person_id="42069-xxx",
               first_name="Foo", last_name="Barsky",
               email="foobar@example.com", date_of_birth={1970, 1, 1},
               billing_address=#address{street = "Billing avenue 1",
                                        city = "Oulu", zip="90420",
                                        country = ':FI'}},
       #person{person_id="666100-333",
               first_name="Nathaniel", last_name="Backpressure",
               email="nate@example.com", date_of_birth={1995,3,6}},
       #person{person_id="123123-333",
               first_name="Eric", last_name="Lang",
               email="er.lang@erlang.org", date_of_birth={1977, 7, 7}},
       #person{person_id="234234-444",
               first_name="Matti", last_name="Korhonen",
               email="matti@notreallyexisting.org", date_of_birth={1969, 12, 20},
              billing_address=#address{street = "Isokatu 25",
                                       city="Oulu", zip="90100",
                                       country=':FI'}},
       #person{person_id="345345-555",
               first_name="John", last_name="Doe",
               email="jd@imaginaryjd.com", date_of_birth={2000, 3, 4}},
       #person{person_id="456456-666",
               first_name="Annika", last_name="Andersson",
               email="aa@existerar-inte.se", date_of_birth={1986, 5, 27},
               billing_address=#address{street = "Svensk gatan 6",
                                        city = "Stockholm", zip="666123",
                                        country = ':SE'}}]).

add_many() ->
    %% add lots of persons with only a name to test paging
    xt:put(
      [#person{person_id="test"++integer_to_list(I),
               first_name="Paging"++integer_to_list(I),
               last_name=integer_to_list(1000-I)++" Test"} ||
          I <- lists:seq(1, 1000)]).

page(N, Size) ->
    xt:ql(#person{first_name={between, "Paging", "Q"}},
          [{order_by, [{#person.first_name,asc}]},
           {offset, N*Size},
           {limit, Size}]).

batch() ->
    xt:batch(
      fun() ->
              xt:put(#person{person_id="batch001",
                             first_name="Batch123",
                             last_name = "First batch"}),
              xt:put(#person{person_id="batch002",
                             first_name="Batch245",
                             last_name="Second batch"})
      end).

listen() ->
    Pid = spawn(?MODULE, query_listen, []),
    xt:ql(#person{first_name={textsearch,"M*"}}, [{defer, Pid}, {listen,yes}]),

    Pid.

query_listen() ->
    receive
        {ok, _, Results} ->
            io:format("GOT RESULTS: ~p", [Results]),
            query_listen();
        stop ->
            io:format("Stopping listening.", [])
    end.
