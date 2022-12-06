-record(mapping, {empty :: tuple(),
                  fields :: [field_mapping()]}).
-type field_mapping() :: {atom(), integer()} | {atom(), fun(), fun()}.
