sloth
=====

Slugify library inspired by awesome Pythonic awesome-slugify.

Build
-----

    $ rebar3 compile

Use
---

    25> sloth:slugify("what ever").
    "what-ever"
    26> sloth:slugify(<<"what ever">>).
    <<"what-ever">>
    29> sloth:slugify(<<"WhaT _ ever% ---">>, [{to_lower, true}, {capitalize, true}, {safe_chars, "%"}]).
    <<"What-ever%">>
