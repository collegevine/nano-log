{-# LANGUAGE FlexibleContexts #-}

module System.Log.Nano (
    module System.Log.Nano.Types,
    module System.Log.Nano.Instances,
    logi,
    logd,
    logw,
    loge,
    runLog
) where

import System.Log.Nano.Types
import System.Log.Nano.Instances

import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Writer (MonadWriter, tell)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import System.IO (hFlush, stdout)

--
-- Log messages to the monad
--
logi :: MonadWriter Trace m => String -> m ()
logi = tell . (:[]) . TraceItem TInfo

logd :: MonadWriter Trace m => String -> m ()
logd = tell . (:[]) . TraceItem TDebug

loge :: MonadWriter Trace m => String -> m ()
loge = tell . (:[]) . TraceItem TError

logw :: MonadWriter Trace m => String -> m ()
logw = tell . (:[]) . TraceItem TWarning

--
-- Print messages to stdout
--
runLog :: (MonadWriter Trace m, HasTraceConfig r, MonadIO m, MonadReader r m) => WriterT Trace m a -> m (a, Trace)
runLog act = do
    level <- view traceLevel
    (res, tx) <- runWriterT act
    mapM_ (liftIO . print) $ filter ((>=level) . _traceItemLevel) tx
    liftIO $ hFlush stdout 
    return (res, tx)
