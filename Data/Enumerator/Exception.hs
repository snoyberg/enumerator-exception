{-# LANGUAGE FlexibleContexts #-}
module Data.Enumerator.Exception
    ( catch
    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Exception.Lifted as E
import Data.Enumerator (Iteratee (..), Step (..))
import Prelude hiding (catch)

catch :: (E.Exception e, MonadBaseControl IO m)
      => Iteratee a m b
      -> (e -> Iteratee a m b)
      -> Iteratee a m b
catch (Iteratee mstep) f = Iteratee $ do
    step <- mstep `E.catch` (runIteratee . f)
    return $ case step of
        Continue k -> Continue $ \s -> catch (k s) f
        Yield b s -> Yield b s
        Error e -> Error e
