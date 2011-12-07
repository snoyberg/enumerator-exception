{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Data.Enumerator.Exception
    ( catch
    , try
    , finally
    , throwIO
    ) where

#if MC03
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Exception.Lifted as E
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
import qualified Control.Exception.Control as E
#define MBCIO MonadControlIO
#endif
import Data.Enumerator (Iteratee (..), Step (..))
import Prelude hiding (catch)
import Control.Monad.Trans.Class (lift)

throwIO :: (E.Exception e, MBCIO m)
        => e
        -> Iteratee a m b
throwIO = lift . E.throwIO

catch :: (E.Exception e, MBCIO m)
      => Iteratee a m b
      -> (e -> Iteratee a m b)
      -> Iteratee a m b
catch (Iteratee mstep) f = Iteratee $ do
    step <- mstep `E.catch` (runIteratee . f)
    return $ case step of
        Continue k -> Continue $ \s -> catch (k s) f
        Yield b s -> Yield b s
        Error e -> Error e

try :: (E.Exception e, MBCIO m)
    => Iteratee a m b
    -> Iteratee a m (Either e b)
try i = catch (fmap Right i) (return . Left)

try'
    :: MBCIO m
    => Iteratee a m b
    -> Iteratee a m (Either E.SomeException b)
try' = try

finally :: MBCIO m
        => Iteratee a m b
        -> Iteratee a m c
        -> Iteratee a m b
finally body final = do
    ea <- try' body
    _ <- try' final
    either throwIO return ea
