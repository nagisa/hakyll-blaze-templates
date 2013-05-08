# Blaze templates for Hakyll

Package provides an alternative to Hakyll's native templates by taking
advantage of [blaze] to create templates.

[blaze]: http://hackage.haskell.org/package/blaze-html

## Why should you care?

You should not. Unless you found Hakyll built-in templates too inflexible for
your liking. Further follows why blaze templates are better and why are they
worse than Hakyll own templates.

### Pros

- Safety. You won't have broken HTML anymore.
- You get to use haskell. Yes, in templates.

### Cons

- No automatic recompilation when templates change. On top of that you'll even
  have to recompile whole generator.
- No dependency tracking, so `rebuild` after template change is required.

## Examples Please!

My personal blog has some [blazeified templates][templates]. You should look at
them.

[templates]: https://github.com/simukis/kazlauskas.me/tree/master/src/Templates
