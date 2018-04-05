{-# OPTIONS_GHC -Wextra            #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Launcher.Environment
  (
    readYamlEnvSubstituted
  )
where

import qualified Data.ByteString.Char8        as B8
import           Data.Conduit                    (runConduitRes, (.|))
import qualified Data.Map                     as Map
import           Data.Yaml.Parser         hiding (parseRawDoc)
import           System.Environment              (getEnvironment)
import           Text.Libyaml
import           Universum

-- * Environment variable substitution for the launcher configuration file,
--   typically launcher-config.yaml.  Based on Alexander Vieth's suggestion
--   to reinterpret YAML anchors as environment variables.
--   Code is his own, with very little change.

envAnchorMap :: IO AnchorMap
envAnchorMap = fmap (Map.fromList . (fmap . fmap) makeYamlValue) getEnvironment
  where
    makeYamlValue :: String -> YamlValue
    makeYamlValue str = Scalar (B8.pack str) StrTag SingleQuoted Nothing

-- Defined in Data.Yaml.Parser
parseRawDoc :: (FromYaml a, MonadThrow m) => RawDoc -> m a
parseRawDoc (RawDoc val am) =
    case unYamlParser (fromYaml val) am of
        Left t -> throwM $ FromYamlException t
        Right x -> return x

readYamlEnvSubstituted :: FromYaml a => FilePath -> IO a
readYamlEnvSubstituted fp = do
    am <- envAnchorMap
    RawDoc yaml am' <- runConduitRes (decodeFile fp .| sinkRawDoc)
    parseRawDoc $ RawDoc yaml (Map.union am am') -- Prefer internal anchors over environment variables
