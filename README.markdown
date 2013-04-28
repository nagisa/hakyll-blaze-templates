# Blaze templates for Hakyll

Package provides an alternative to Hakyll's native templates by taking
advantage of [blaze] to create templates.

[blaze]: http://hackage.haskell.org/package/blaze-html

## Why should you care?

You should not. Unless you found Hakyll built-in templates too inflexible for
your liking. Further follows why blaze templates are better and why are they
worse than Hakyll own templates.

### Pros

- More flexibility. Conditionals? Sure. Loops? Yay!
- Safety. You won't have broken HTML anymore.

### Cons

- No automatic recompilation when templates change. On top of that you'll even
  have to recompile whole generator.
- No dependency tracking, so `rebuild` after template change is required.

## Examples Please!

```haskell
-- Import qualified if you don't want to hide Prelude
import Text.Blaze.Html5

blazeTemplate :: Template Compiler String
-- or blazeTemplate :: MonadMetadata m => Template m String if you prefer so
blazeTemplate ctx item = do
    title <- context "title"
    return $
        docTypeHtml $ do
            head $ title $ toHtml title
            body $ do
                h1 $ toHtml title
                article $ preEscapedToHtml $ itemBody item
```

is equivalent to

```html
<!DOCTYPE html>
<html>
    <head>
        <title>$title$</title>
    </head>
    <body>
        <h1>$title$</h1>
        <article>$body$</article>
    </body>
</html>
```

But this is too boring, so let's do whatever regular Hakyll templates can't.

```haskell
import Text.Blaze.Html5

blazeTemplate :: Template Compiler String
blazeTemplate ctx item = do
    title <- context "title"
    return $
        docTypeHtml $ do
            head $ title $ toHtml title
            body $ do
                h1 $ toHtml if title == "Hakyll is cool"
                                then "Blaze is cool too"
                                else title
                article $ preEscapedToHtml $ itemBody item
                ul $ mapM_ (\e -> li $ toHtml $ e ++ " is cool too")
                    ["Haskell", "Unicode", "Computers", "Whatever else"]
```

Now using it where you would use it in normal circumstances:

```haskell
import qualified Hakyll.Web.Template.Blaze as T
-- …
    match "entries/**.md" $ do
        route $ setExtension "html"
        compile $ entryCompiler
            >>= T.applyTemplate entryTpl entryContext
            >>= T.applyTemplate baseTpl baseContext
            >>= relativizeUrls
```

instead of regular

```haskell
    match "entries/**.md" $ do
        route $ setExtension "html"
        compile $ entryCompiler
            >>= loadAndApplyTemplate "templates/entry.html" entryContext
            >>= loadAndApplyTemplate "templates/base.html" baseContext
            >>= relativizeUrls
```

