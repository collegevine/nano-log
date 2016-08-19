{-# LANGUAGE OverloadedStrings #-}

module System.Log.Nano.Instances where

import System.Log.Nano.Types

import Data.Aeson

instance Show TraceItem where
    show (TraceItem l m) = "<" ++ show l ++ "> " ++ m

instance ToJSON TraceItem where
    toJSON (TraceItem l m) = object ["level" .= show l, "message" .= m]


