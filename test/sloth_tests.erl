-module(sloth_tests).
-include_lib("eunit/include/eunit.hrl").

-export([english_test/0]).

english_test() ->
    ?assertEqual(<<"This-is-a-test">>, sloth:slugify(<<"This % is a test ---">>)),
    ?assertEqual(<<"this-is-a-test">>, sloth:slugify(<<"_this_is_a__test___">>)),
    ?assertEqual(<<"This-is-a-test">>, sloth:slugify(<<"- - -This -- is a ## test ---">>)),
    ?assertEqual("This-is-a-test", sloth:slugify("This % is a test ---")),
    ?assertEqual("this-is-a-test", sloth:slugify("_this_is_a__test___")),
    ?assertEqual("This-is-a-test", sloth:slugify("- - -This -- is a ## test ---")).

mixed_test() ->
    ?assertEqual(<<"5-neat-tricks">>, sloth:slugify(<<"5 neat tricks">>)),
    ?assertEqual(<<"these-20-heroes">>, sloth:slugify(<<"these 20 heroes">>)),
    ?assertEqual(<<"building-42">>, sloth:slugify(<<"building 42">>)),
    ?assertEqual("5-neat-tricks", sloth:slugify("5 neat tricks")),
    ?assertEqual("these-20-heroes", sloth:slugify("these 20 heroes")),
    ?assertEqual("building-42", sloth:slugify("building 42")).

 numeric_test() ->
    ?assertEqual(<<"404">>, sloth:slugify(<<"404">>)),
    ?assertEqual(<<"1">>, sloth:slugify(<<"1">>)),
    ?assertEqual("404", sloth:slugify("404")),
    ?assertEqual("1", sloth:slugify("1")).

to_lower_test() ->
    ?assertEqual(<<"test-to-lower">>, sloth:slugify(<<"Test TO lower">>, [{to_lower, true}])),
    ?assertEqual("test-to-lower", sloth:slugify("Test TO lower", [{to_lower, true}])),
    ?assertEqual(<<"Test-TO-lower">>, sloth:slugify(<<"Test TO lower">>, [{to_lower, false}])),
    ?assertEqual("Test-TO-lower", sloth:slugify("Test TO lower", [{to_lower, false}])).
    
to_lower_capitalize_test() ->
    ?assertEqual(<<"Test-to-lower">>, sloth:slugify(<<"Test TO lower">>, [{to_lower, true}, {capitalize, true}])),
    ?assertEqual("Test-to-lower", sloth:slugify("Test TO lower", [{to_lower, true}, {capitalize, true}])).

sanitize_test() ->
    ?assertEqual(<<"test-sanitize">>, sloth:slugify(<<"test_sanitize">>)),
    ?assertEqual("test-sanitize", sloth:slugify("test_sanitize")).

safe_chars_test() ->
    ?assertEqual(<<"test_sanitize">>, sloth:slugify(<<"test_sanitize">>, [{safe_chars, "_"}])),
    ?assertEqual("test_sanitize", sloth:slugify("test_sanitize", [{safe_chars, "_"}])),
    ?assertEqual(<<"test_s-nitize">>, sloth:slugify(<<"test_s%nitize">>, [{safe_chars, "_"}])),
    ?assertEqual("test_s-nitize", sloth:slugify("test_s%nitize", [{safe_chars, "_"}])),
    ?assertEqual(<<"test_s%nitize">>, sloth:slugify(<<"test_s%nitize">>, [{safe_chars, "_%"}])),
    ?assertEqual("test_s%nitize", sloth:slugify("test_s%nitize", [{safe_chars, "_%"}])).

    
    
