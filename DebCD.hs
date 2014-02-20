{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.Yaml.Config as YConf
import System.Environment
import qualified Data.Text as T
import Network.Mail.Mime.SES
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit
import Control.Monad
import Data.Conduit

confPath = "/etc/debcd/debcd.yml"

main = do
 conf <- getConfig
 sender <- getSender conf

 let upRepos = YConf.lookupDefault "update-repos" ("all"::String) conf

 -- create dpkg freeze file

 -- update
 
 -- upgrade 

 -- run tests

 -- roll back on failure

 return ()

getConfig :: IO YConf.Config
getConfig = do
 allConfig <- YConf.load confPath
 args <- getArgs
 let envNm = case (args, YConf.keys allConfig) of
            (env:_, _) -> T.pack env
            ([], []) -> error $ "debcd: unable to determine a configuration "++
                                "environment from "++confPath
            ([] , env:_) -> env
 case YConf.subconfig envNm allConfig of
           Nothing -> fail $ "Cannot find configuration environment "++
                             T.unpack envNm++" in "++confPath
           Just c -> return c

getSender :: YConf.Config -> IO (LBS.ByteString -> IO ())
getSender conf = 
  let mses = do emailTo <- YConf.lookup "emailTo" conf
                emailFrom <- YConf.lookup "emailTo" conf
                access <-  YConf.lookup "aws_access" conf
                secret <-  YConf.lookup "aws_secret" conf
                return $ SES
                   { sesFrom = S8.pack $ emailFrom
                   , sesTo = [DTE.encodeUtf8 emailTo]
                   , sesAccessKey = S8.pack $ access
                   , sesSecretKey = S8.pack $ secret
                   }
  in case mses of
       Nothing -> return $ const $ return () 
       Just ses -> do man <- newManager conduitManagerSettings
                      return $  runResourceT . sendMailSES man ses 

