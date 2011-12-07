{-# LANGUAGE FlexibleContexts #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit
import Control.Exception (SomeException)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Exception as EE
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Writer (runWriterT)

main :: IO ()
main = hspecX $ do
    describe "catch" $ do
        it "IO" $ caseCatch id
        it "ReaderT" $ caseCatch $ flip runReaderT ()
        it "WriterT" $ caseCatch $ fmap fst' . runWriterT

fst' :: ((), ()) -> ()
fst' ((), ()) = ()

caseCatch :: MonadBaseControl IO m => (m () -> IO ()) -> IO ()
caseCatch unwrap = unwrap $ E.run_ $ E.enumList 8 [1..1000 :: Int] E.$$ flip EE.catch ignorer $ do
    _ <- EL.consume
    error "foo"
  where
    ignorer :: Monad m => SomeException -> E.Iteratee a m ()
    ignorer _ = return ()
