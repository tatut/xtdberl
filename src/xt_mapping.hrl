%% @type Defines a single XTDB attribute mapping to a single record field.
%% If to_xtdb and from_xtdb are specified, those functions are used to
%% convert values between domains.
-record(field, {attr :: atom(),
                field :: integer(), %% an Erlang record field (tuple idx)
                key :: atom(), %% or an atom map key
                required = false :: boolean(),
                to_xtdb :: function(),
                from_xtdb :: function()}).

%% @type Defines a function to apply the whole doc or record
%% when transferring between domains. This can use any fields to add
%% fields. Attrs field specifies what attributes should be pulled
%% when fetching documents.
-record(conversion, {record_to_xtdb :: function(),
                     xtdb_to_record :: function(),
                     attrs :: [atom()]}).

%% @type Defines the mapping from Erlang records to XTDB documents.
%% The empty tuple value must be specified (eg #myrecord{}) for
%% some operations to work.
-record(mapping, {empty :: tuple() | map(),
                  fields :: [ mapping_def() ]}).

-type mapping_def() :: #field{} | #conversion{}.

%% @type Embed mapping
-record(embed, {field :: integer(),
                key :: atom(),
                mapping :: #mapping{}}).

%% @type Static attribute in docs
-record(static, {attr :: atom(), value :: any()}).

%% @type Link mapping
-record(link, {%% XTDB document attribute
               attr :: atom(),

               %% Record field or struct key
               field :: integer(), key :: atom(),

               %% Record or struct type of linked document.
               %% Should be the empty instance.
               to :: tuple() | map(),

               %% if owned is true, this will be deleted
               %% when parent is deleted, and pulled by
               %% default
               owned = false :: boolean(),

               %% cardinality many or one
               cardinality :: many | one}).
