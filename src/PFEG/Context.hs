{-# LANGUAGE RankNTypes, OverloadedStrings, FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module PFEG.Context
    ( -- * Types
      Context(..)
    , Item
    , ItemGetter
    , Restriction
      -- * Transformation functions
    , restrictContext
    , getDocumentItems
    , getUniqueDocumentItems
      -- Misc
    , period -- a token representing a period
    ) where

import Text.Search.Sphinx.Types
import Text.Search.Sphinx
import PFEG.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (findIndices)
import Control.Monad.Reader
import PFEG

type ItemGetter = forall st. Document Text -> PFEG st [Item Text]

period :: Token Text
period = Word { surface = ".", lemma = ".", pos = "$." }

getContexts :: (a -> Bool) -> [a] -> [(a,Context a)]
getContexts p s = map (mkContext . flip splitAt s) $ findIndices p s
                  where mkContext (_,[]) = error "findIndices returned some garbage!"
                        mkContext (a,b) = (head b,Context { left = a, right = tail b })

getDocumentItems' :: (Sentence Text -> PFEG st [Item Text]) -> [Sentence Text] -> PFEG st [Item Text]
getDocumentItems' k = liftM concat . mapM (k . (period:) .filter (not.punctuation))
    where punctuation t = surface t `elem` [","]

getDocumentItems :: (Text -> Bool) -> ItemGetter
getDocumentItems p = getDocumentItems' (return . getContexts (p.surface))

-- | FIXME: While filtering this way is OK, taking the random sample on
-- *Items* later in pfeg.hs is terriby inefficient (we check for the
-- sentence's existence in the DB here, generate items that way and only
-- *then* take a random sample, rendering almost all queries to the DB
-- inefficient.)
getUniqueDocumentItems :: (Text -> Bool) -> ItemGetter
getUniqueDocumentItems p = getDocumentItems' k
    where k :: Sentence Text -> PFEG st [Item Text]
          k s = do isInCorpus <- checkExists s
                   if isInCorpus then return []
                                 else return $ getContexts (p.surface) s
          checkExists :: Sentence Text -> PFEG st Bool
          checkExists s = do session <- ask
                             qr <- liftIO $ query (searchConf session)
                                                 (sphinxIndex session)
                                                 (wrap '"' . T.unwords . map surface $ s)
                             case qr of
                                 Ok QueryResult { total = h } | h > 0 -> return True
                                 _                                    -> return False

type Restriction = (Int,Int)

restrictContext :: Restriction -> Context a -> Context a
restrictContext (i_l,i_r) Context { left = l, right = r } =
    Context { left = reverse . take i_l . reverse $ l, right = take i_r r }
