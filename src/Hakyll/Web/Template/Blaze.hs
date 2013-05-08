module Hakyll.Web.Template.Blaze
    ( Template
    , applyTemplate
    , applyTemplateList
    , applyTemplateListWith

    , string
    , preEscapedString
    -- For API compatibility with first release
    , toHtml
    , safeToHtml
    ) where

import Hakyll                          (Context(..), Item, Compiler,
                                        itemSetBody, missingField, itemBody)
import Data.Monoid                     (mappend)
import Data.List                       (intercalate)
import Text.Blaze.Html                 (Html)
import Text.Blaze.Internal             (string, preEscapedString)
import qualified Text.Blaze.Html as H
import Text.Blaze.Html.Renderer.String (renderHtml)


type Template m a = (String -> m String) -> Item a -> m Html


applyTemplate :: Template Compiler String -- ^ Blaze template
              -> Context String           -- ^ Hakyll context
              -> Item String              -- ^ The item
              -> Compiler (Item String)   -- ^ Resulting HTML
applyTemplate tpl ctx item =
    tpl ctx' item
    >>= return . renderHtml
    >>= \body -> return $ itemSetBody body item
  where
    ctx' :: String -> Compiler String
    ctx' key = unContext (ctx `mappend` missingField) key item


applyTemplateListWith :: String              -- ^ String to join template with
                      -> Template Compiler String -- ^ Blaze template
                      -> Context String           -- ^ Hakyll context
                      -> [Item String]            -- ^ List of items
                      -> Compiler String          -- ^ Resulting HTML
applyTemplateListWith delimiter tpl ctx items =
    mapM (applyTemplate tpl ctx) items
    >>= return . intercalate delimiter . map itemBody


applyTemplateList = applyTemplateListWith ""


toHtml, safeToHtml :: String -> Html
-- | toHtml specialised to String.
toHtml = string

-- | preEscapedToHtml specialised to String
-- Also safeToHtml sounds better than preEscapedToHtml
safeToHtml = preEscapedString
