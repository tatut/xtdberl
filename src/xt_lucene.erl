-module(xt_lucene).
-export([prefix_words/1,escape/1]).

%% @doc Escape Lucene special characters
escape(Str) ->
    lists:flatmap(fun($+) -> "\\+"; ($-) -> "\\-"; ($&) -> "\\&";
                     ($|) -> "\\|"; ($!) -> "\\!"; ($() -> "\\(";
                     ($)) -> "\\)"; (${) -> "\\{"; ($}) -> "\\}";
                     ($[) -> "\\["; ($]) -> "\\]"; ($^) -> "\\^";
                     ($") -> "\\\""; ($~) -> "\\~"; ($*) -> "\\*";
                     ($?) -> "\\?"; ($:) -> "\\:"; ($\\) -> "\\\\";
                     (Ch) -> [Ch] end, Str).

%% @doc Prefix words search.
%% Split user input to words and return Lucene search
%% that requires that a word starting with every input word
%% is present.
prefix_words(Str) ->
    string:join(["+" ++ W ++ "*" ||
                    W <- string:split(escape(Str), " ", all),
                    length(W) > 0], " ").
