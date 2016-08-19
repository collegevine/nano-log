{-# LANGUAGE TemplateHaskell #-}

module System.Log.Nano.Types where

import Control.Lens.TH
import Control.Monad.Trans.Writer (WriterT)

type LogM = WriterT Trace IO

type Trace = [TraceItem]

data TraceItem = TraceItem {
    _traceItemLevel :: TraceLevel,
    _traceItemMessage :: String
}

data TraceLevel = 
    TDebug
    | TInfo
    | TWarning
    | TError
    deriving (Eq, Ord, Read, Show)

data TraceConfig = TraceConfig {
    _traceLevel :: TraceLevel
}

makeClassy ''TraceConfig
makeLenses ''TraceItem
