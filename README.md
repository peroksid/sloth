sloth
=====

Slugify library inspired by awesome Pythonic awesome-slugify.

Build
-----

    $ rebar3 compile

Use
---

sloth:slugify("what ever")
sloth:slugify(<<"what ever">>)
sloth:slugify("foo", [{to_lower, true}, {capitalize, true}, {safe_chars, "%_"}])


