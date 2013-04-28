module Hakyll.Web.Template.Blaze
    ( Template
    , applyTemplate
    , applyTemplateList
    , applyTemplateListWith
    ) where

import Hakyll           (Context(..), Item, Compiler, itemSetBody,
                         missingField, itemBody)
import Data.Monoid      (mappend)
import Data.List        (intercalate)

type Template m a = (String -> Item a -> m a) -> Item a -> m a


applyTemplate :: Template Compiler String -- | Blaze template
              -> Context String           -- | Hakyll context
              -> Item String              -- | The item
              -> Compiler (Item String)   -- | Resulting HTML
applyTemplate tpl ctx item = tpl ctx' item >>=
    \body -> return $ itemSetBody body item
  where
    ctx' :: String -> Item String -> Compiler String
    ctx' = unContext (ctx `mappend` missingField)


applyTemplateListWith :: String                   -- | String to join template
                                                  -- with
                      -> Template Compiler String -- | Blaze template
                      -> Context String           -- | Hakyll context
                      -> [Item String]            -- | List of items
                      -> Compiler String          -- | Resulting HTML
applyTemplateListWith delimiter tpl ctx items =
    mapM (applyTemplate tpl ctx) items
    >>= return . intercalate delimiter . map itemBody


applyTemplateList = applyTemplateListWith ""
