{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit
import Control.Exception (SomeException)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Exception as EE
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Writer (runWriterT)
import Control.Monad ((=<<))

#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl)
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
#define MBCIO MonadControlIO
#endif

type Unwrap m = forall a. m a -> IO a
type Case = forall m. MBCIO m => Unwrap m -> IO ()

allMonads :: String -> Case -> Specs
allMonads str f = describe str $ do
    it "IO" $ f id
    it "ReaderT" $ f $ flip runReaderT ()
    it "WriterT" $ f $ fmap fst' . runWriterT

main :: IO ()
main = hspecX $ do
    allMonads "catch" caseCatch
    allMonads "try" caseTry

fst' :: (a, ()) -> a
fst' (a, ()) = a

caseCatch :: Case
caseCatch unwrap = unwrap $ E.run_ $ E.enumList 8 [1..1000 :: Int] E.$$ flip EE.catch ignorer $ do
    _ <- EL.consume
    error "foo"
  where
    ignorer :: Monad m => SomeException -> E.Iteratee a m ()
    ignorer _ = return ()

caseTry :: Case
caseTry unwrap = check =<< (unwrap $ E.run_ $ E.enumList 8 [1..1000 :: Int] E.$$ EE.try $ do
    _ <- EL.consume
    error "foo")
  where
    check :: Either SomeException () -> IO ()
    check (Left _) = return ()
    check (Right ()) = error "There should have been an exception caught"
