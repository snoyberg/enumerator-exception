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
data ErrType = ErrPre | ErrPost | NoErr

allMonads :: String -> Case -> Specs
allMonads str f = describe str $ do
    it "IO" $ f id
    it "ReaderT" $ f $ flip runReaderT ()
    it "WriterT" $ f $ fmap fst' . runWriterT

fst' :: (a, ()) -> a
fst' (a, ()) = a

allErrs :: String -> (ErrType -> Case) -> Specs
allErrs str f = do
    allMonads (str ++ " pre") $ f ErrPre
    allMonads (str ++ " post") $ f ErrPost
    allMonads (str ++ " none") $ f NoErr

main :: IO ()
main = hspecX $ do
    allErrs "catch" caseCatch
    allErrs "try" caseTry

body err = do
    case err of { ErrPre -> error "ErrPre" ; _ -> return () }
    _ <- EL.consume
    case err of { ErrPost -> error "ErrPost" ; _ -> return () }

caseCatch :: ErrType -> Case
caseCatch err unwrap =
    unwrap $ E.run_ $ E.enumList 8 [1..1000 :: Int] E.$$ EE.catch (body err) ignorer
  where
    ignorer :: Monad m => SomeException -> E.Iteratee a m ()
    ignorer _ = return ()

caseTry :: ErrType -> Case
caseTry err unwrap =
    (unwrap $ E.run_ $ E.enumList 8 [1..1000 :: Int] E.$$ EE.try (body err)) >>= check err
  where
    check :: ErrType -> Either SomeException () -> IO ()
    check NoErr (Right ()) = return ()
    check NoErr (Left e) = error "unexpected exception (pun)"
    check _ (Left _) = return ()
    check _ (Right ()) = error "There should have been an exception caught"
