{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
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

main :: IO ()
main = hspecX $ do
    describe "catch" $ do
        it "IO" $ caseCatch id
        it "ReaderT" $ caseCatch $ flip runReaderT ()
        it "WriterT" $ caseCatch $ fmap fst' . runWriterT
    describe "try" $ do
        it "IO" $ caseTry id
        it "ReaderT" $ caseTry $ flip runReaderT ()
        it "WriterT" $ caseTry $ fmap fst' . runWriterT

fst' :: (a, ()) -> a
fst' (a, ()) = a

caseCatch :: MBCIO m => (m () -> IO ()) -> IO ()
caseCatch unwrap = unwrap $ E.run_ $ E.enumList 8 [1..1000 :: Int] E.$$ flip EE.catch ignorer $ do
    _ <- EL.consume
    error "foo"
  where
    ignorer :: Monad m => SomeException -> E.Iteratee a m ()
    ignorer _ = return ()

caseTry :: MBCIO m => (forall a. m a -> IO a) -> IO ()
caseTry unwrap = check =<< (unwrap $ E.run_ $ E.enumList 8 [1..1000 :: Int] E.$$ EE.try $ do
    _ <- EL.consume
    error "foo")
  where
    check :: Either SomeException () -> IO ()
    check (Left _) = return ()
    check (Right ()) = error "There should have been an exception caught"
