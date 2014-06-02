{-# LANGUAGE TemplateHaskell,
             DeriveDataTypeable,
             RecordWildCards,
             TypeFamilies #-}

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid (AcidState, Query, Update, makeAcidic, openLocalState)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.Data (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy)

type Title = String
type Content = String

data Post = Post { title :: Title, content :: Content, postCount :: Int }
  deriving (Eq, Read, Show, Data, Typeable)

instance Ord Post where
  compare a b = compare (postCount a) (postCount b)

data Posts = Posts { posts :: [Post] } deriving (Eq, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Post)
$(deriveSafeCopy 0 'base ''Posts)

addPost :: Title -> Content -> Update Posts Post
addPost title content = do
  Posts ps <- get
  let nextPostCount = (length ps) + 1
  let newPost = Post title content nextPostCount
  put $ Posts (newPost:ps)
  return newPost

getPosts :: Query Posts [Post]
getPosts = posts <$> ask

emptyPosts :: Posts
emptyPosts = Posts { posts = [] }

$(makeAcidic ''Posts ['addPost, 'getPosts])

data CounterState = CounterState { count :: Integer }
  deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CounterState)

initialCounterState :: CounterState
initialCounterState = CounterState 0

-- inCountBy :: Integer -> State CounterState Integer
incCountBy :: Integer -> Update CounterState Integer
incCountBy n = do
  c@CounterState{..} <- get
  let newCount = count + n
  put $ c { count = newCount }
  return newCount

-- peekCount :: Reader CounterState Integer
peekCount :: Query CounterState Integer
peekCount = count <$> ask

$(makeAcidic ''CounterState ['incCountBy, 'peekCount])
