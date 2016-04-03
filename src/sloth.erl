-module('sloth').

%% API exports
-export([slugify/1, slugify/2]).

%%====================================================================
%% API functions
%%====================================================================
slugify(Input) ->
    slugify(Input, []).

slugify(Input, Options) when is_binary(Input) ->
    unicode:characters_to_binary(
      slugify(unicode:characters_to_list(Input), Options)
     );
slugify(Input, Options) when is_list(Input) ->
    slug(Input, none, [], Options).

%%====================================================================
%% Internal functions
%%====================================================================
slug([], _, Acc, _) ->
    lists:reverse(Acc);
slug([Head|Tail], Previous, Acc, Options) ->
    Translated = translate(Head, Options),
    Result = add(Translated, Tail, Previous, Acc, Options),
    Result.

add($-, Tail, _, [], Options) ->
    slug(Tail, none, [], Options);
add($-, Tail, _Previous, Acc, Options) ->
    slug(Tail, $-, Acc, Options);
add(Translated, Tail, $-, Acc, Options) ->
    slug(Tail, Translated, [Translated | [$- | Acc]], Options);
add(Translated, Tail, _, [], Options) ->
    slug(Tail, Translated, [capitalize(Translated, Options) | []], Options);
add(Translated, Tail, _, Acc, Options) ->
    slug(Tail, Translated, [Translated | Acc], Options).

capitalize(Ch, Options) ->
    case proplists:get_value(capitalize, Options, false) of
        true ->
            string:to_upper(Ch);
        false -> Ch
    end.

translate(X, Options) ->
    Lower = string:to_lower(X),
    minusify(case proplists:get_value(to_lower, Options, false) of
                 true -> Lower;
                 false -> X
             end,
             Lower,
             Options).

minusify(Ch, Lower, _Options) when Lower >= $a, Lower =< $z ->
    Ch;
minusify(Ch, Lower, _Options) when Lower >= $0, Lower =< $9 ->
    Ch;
minusify(Ch, Lower, Options) ->
    SafeChars = unicode:characters_to_list(proplists:get_value(safe_chars, Options, "")),
    case lists:any(fun(El) -> Ch =:= El end, SafeChars) of
        true -> Ch;
        false -> $-
    end.

