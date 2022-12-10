-module(xt_demo).
-compile(export_all).

%% Demo how to use XTDB from Erlang, with mapping Erlang records to XTDB documents

-record(address, {street :: string(),
                  city :: string(),
                  zip :: string(),
                  country :: string()}).

-record(person, {person_id :: string(),
                 first_name :: string(),
                 last_name :: string(),
                 email :: string(),
                 date_of_birth :: calendar:date(),
                 shipping_address :: #address{},
                 billing_address :: #address{}}).

register_mappings() ->
    Addr =
        xt_mapping:mapping(
          #address{},
          [xt_mapping:field(':address/street', #address.street),
           xt_mapping:field(':address/city', #address.city),
           xt_mapping:field(':address/zip', #address.zip),
           xt_mapping:field(':address/country', #address.country)]),
    Person =
        xt_mapping:mapping(
          #person{},
          [xt_mapping:idmap(#person.person_id, ':person'),
           xt_mapping:static(':type', ':person'),
           xt_mapping:field(':person/first-name', #person.first_name),
           xt_mapping:field(':person/last-name', #person.last_name),
           xt_mapping:field(':person/email', #person.email),
           xt_mapping:local_date(':person/date-of-birth', #person.date_of_birth),
           xt_mapping:embed('shipping-', Addr, #person.shipping_address),
           xt_mapping:embed('billing-', Addr, #person.billing_address)]),
    xt_mapping:register(Addr),
    xt_mapping:register(Person).


person(Id,Fn,Ln,Email,Dob,Billing,Shipping) ->
    #person{person_id = Id,
            first_name = Fn, last_name = Ln,
            email = Email,
            date_of_birth = Dob,
            billing_address = Billing,
            shipping_address = Shipping}.

init() ->
    register_mappings(),
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
