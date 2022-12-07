-record(mapping, {empty :: tuple(),
                  fields :: [field_mapping()]}).
-record(field, {attr :: atom(),
                field :: integer(),
                to_xtdb :: function(),
                from_xtdb :: function()
               }).
