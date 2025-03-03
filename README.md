# Spinsel
Spinsel is a custom static site generator written for my website, [segfault.party](https://segfault.party/). It transforms pages written in Markdown with Jinja2 templating to HTML. It is written just for fun, and as an exercise to get better at Haskell.

# Building
Spinsel uses `stack`, so building is as easy as running `stack build`.

# Running
Spinsel requires a configuration file, `config.toml`, in the root of the site directory. For reference, see the provided sample file.

Once Spinsel has been configured, running it is a `stack run` away.
