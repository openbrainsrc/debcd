{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.Yaml.Config as YConf
import System.Environment
import System.Exit

import qualified Data.Text as T
import Network.Mail.Mime.SES
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit
import Control.Monad
import Data.Conduit
import System.Cmd
import Data.Time
import System.Locale (defaultTimeLocale)
import qualified Data.Time.Format as DTF

confPath = "/etc/debcd/debcd.yml"

main = do
 conf <- getConfig
 sender <- getSender conf

 let upRepos = YConf.lookupDefault "update-repos" ("all"::String) conf

 -- update 
 case upRepos of
   "all" -> void $ system "apt-get update"
   "none" -> return ()   
   _ -> void $ system "apt-get update"

 upgradesAvailable <- do ex <- system "apt-get -u upgrade --assume-no"
                         return $ ex /= ExitSuccess 

 putStrLn $ "Upgrades available: "++ show upgradesAvailable

 -- upgrade 
 when upgradesAvailable $ upgrade sender


upgrade sender = do

 selections <- createFreezeList
 void $ system "apt-get upgrade"

 -- run tests

 -- roll back on failure

 return ()

createFreezeList :: IO String
createFreezeList = do
 now <- getCurrentTime
 let nowS =  DTF.formatTime defaultTimeLocale "%Y%m%d%H%M" now
 -- create dpkg freeze file. http://www.debian-administration.org/article/669/
 system "mkdir -p /var/debcd"
 system $ "aptitude -q -F \"%?p=%?V %M\" --disable-columns search \\~i > /var/debcd/"
          ++nowS
 return nowS


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

