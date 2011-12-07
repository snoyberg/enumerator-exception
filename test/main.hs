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
import qualified Data.IORef as I
import qualified Control.Exception
import Test.HUnit ((@?=))
import Control.Monad.IO.Class (MonadIO, liftIO)

#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl)
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
#define MBCIO MonadControlIO
#endif

type Unwrap m = forall a. m a -> IO a
type Case = forall m. (MBCIO m, MonadIO m) => Unwrap m -> IO ()
data ErrType = ErrPre | ErrPost | NoErr
type ECase = ErrType -> Case

allMonads :: String -> Case -> Specs
allMonads str f = describe str $ do
    it "IO" $ f id
    it "ReaderT" $ f $ flip runReaderT ()
    it "WriterT" $ f $ fmap fst' . runWriterT

fst' :: (a, ()) -> a
fst' (a, ()) = a

allErrs :: String -> (ECase) -> Specs
allErrs str f = do
    allMonads (str ++ " pre") $ f ErrPre
    allMonads (str ++ " post") $ f ErrPost
    allMonads (str ++ " none") $ f NoErr

main :: IO ()
main = hspecX $ do
    allErrs "catch" caseCatch
    allErrs "try" caseTry
    allErrs "finally" caseFinally
    allErrs "bracket" caseBracket

body err = do
    case err of { ErrPre -> error "ErrPre" ; _ -> return () }
    _ <- EL.consume
    case err of { ErrPost -> error "ErrPost" ; _ -> return () }

runner iter = E.run_ $ E.enumList 8 [1..1000 :: Int] E.$$ iter

caseCatch :: ECase
caseCatch err unwrap =
    unwrap $ runner $ EE.catch (body err) ignorer
  where
    ignorer :: Monad m => SomeException -> E.Iteratee a m ()
    ignorer _ = return ()

caseTry :: ECase
caseTry err unwrap = (unwrap $ runner $ EE.try (body err)) >>= check err

check :: ErrType -> Either SomeException () -> IO ()
check NoErr (Right ()) = return ()
check NoErr (Left e) = error "unexpected exception (pun)"
check _ (Left _) = return ()
check _ (Right ()) = error "There should have been an exception caught"

caseFinally :: ECase
caseFinally err unwrap = do
    i <- I.newIORef (0 :: Int)
    ea <- Control.Exception.try $ unwrap $ runner $ EE.finally (body err) (liftIO $ I.modifyIORef i (+ 1))
    res <- I.readIORef i
    res @?= 1
    check err ea

caseBracket :: ECase
caseBracket err unwrap = do
    i <- I.newIORef (0 :: Int)
    ea <- Control.Exception.try $ unwrap $ runner $ do
        EE.bracket
            (liftIO (I.modifyIORef i (+ 3)) >> return i)
            (\j -> liftIO $ I.modifyIORef j (+ 2))
            (\j -> liftIO (I.modifyIORef j (+ 1)) >> body err)
    res <- I.readIORef i
    res @?= 6
    check err ea
