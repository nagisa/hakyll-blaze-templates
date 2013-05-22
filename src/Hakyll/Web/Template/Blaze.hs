module Hakyll.Web.Template.Blaze
    ( Template
    , applyTemplate
    , applyTemplateList
    , applyTemplateListWith

    , string
    , preEscapedString
    ) where

import Data.Monoid                     (mappend)
import Data.List                       (intercalate)
import Text.Blaze.Html                 (Html)
import Text.Blaze.Internal             (string, preEscapedString)
import qualified Text.Blaze.Html as H
import Text.Blaze.Html.Renderer.String (renderHtml)

import Hakyll (Context(..), ContextField(..), Item, Compiler, MonadMetadata,
               itemSetBody, itemBody, itemIdentifier)


type Template m a = (String -> Compiler a) -> Item a -> m Html


applyTemplate :: MonadMetadata m
              => Template m String -- ^ Blaze template
              -> Context String    -- ^ Hakyll context
              -> Item String       -- ^ The item
              -> m (Item String)   -- ^ Resulting HTML
applyTemplate tpl ctx item =
    tpl ctx' item
    >>= return . renderHtml
    >>= \body -> return $ itemSetBody body item
  where
    ctx' a = unContext ctx a item >>= getString
    getString (StringField s) = return s
    getString _ = fail "Only String contexts are supported for now"


applyTemplateListWith :: MonadMetadata m
                      => String            -- ^ Value to join template with
                      -> Template m String -- ^ Blaze template
                      -> Context String    -- ^ Hakyll context
                      -> [Item String]     -- ^ List of items
                      -> m String     -- ^ Resulting HTML
applyTemplateListWith delimiter tpl ctx items =
    mapM (applyTemplate tpl ctx) items
    >>= return . intercalate delimiter . map itemBody


applyTemplateList :: MonadMetadata m
                  => Template m String
                  -> Context String
                  -> [Item String]
                  -> m String
applyTemplateList = applyTemplateListWith ""
