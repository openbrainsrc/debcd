{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.Yaml.Config as YConf
import System.Environment
import System.Directory
import System.Exit

import qualified Data.Text as T
import Network.Mail.Mime.SES
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.String

import Data.Conduit
import System.Cmd
import Data.Time
import System.Locale (defaultTimeLocale)
import qualified Data.Time.Format as DTF

import System.Process
import System.Cmd
import System.IO
import System.Exit
import qualified Control.Exception as C
import Control.Exception
import Control.DeepSeq (rnf)
import Control.Concurrent


confPath = "/etc/debcd/debcd.yml"

main = do
  conf <- getConfig
  sender <- getSender conf

  update conf sender

update conf sender = do

  let upRepos = YConf.lookupDefault "update-repos" ("all"::String) conf

      updateCmd 
         = case upRepos of
             "all"  -> "apt-get update"
             "none" -> "echo Nothing to do"
             repo   -> "apt-get update -o Dir::Etc::sourcelist=\"sources.list.d/"++
                               repo++".list\"-o Dir::Etc::sourceparts=\"-\" "++
                               "-o APT::Get::List-Cleanup=\"0\""

  updateRes <- psh updateCmd
  case updateRes of
    Left err -> sender $ ["debcd: error updating APT", 
                          "\nError:\n ", err]
    Right _ -> do
        upgradesAvailable <- fmap ((/=) ExitSuccess) 
                                  $ system "apt-get -u upgrade --assume-no"                               
        when upgradesAvailable $ upgrade sender

upgrade sender = do

 selections <- createFreezeList
 upgRes <- psh "apt-get upgrade"
 case upgRes of
   Left err -> sender $ ["debcd: error upgrading packages", 
                          "\nError:\n ", err]
   Right _ -> do

       -- run tests
       testsOK <- runTests sender

       -- roll back on failure
       when (not testsOK) $ rollback selections

       return ()

-- these two based on http://www.debian-administration.org/article/669/

runTests sender = do
  files <- getDirectoryContents "/etc/debcd/tests.d/"
  testRess <-  forM files $ \file -> do
    p <- getPermissions $ "/etc/debcd/tests.d/"++file
    if not $ executable p
       then return Nothing
       else do res <- psh $ "/etc/debcd/tests.d/"++file
               case res of 
                 Right _ -> return Nothing
                 Left err -> return $ Just $ "Test: "++file++"\nOutput:\n"++ 
                                               (unlines $ map ("  "++) $ lines err)
  if all isNothing testRess
     then return True
     else do sender ("debcd: Test failure":catMaybes testRess)
             return False

createFreezeList :: IO String
createFreezeList = do
 now <- getCurrentTime
 let nowS =  DTF.formatTime defaultTimeLocale "%Y%m%d%H%M" now
 system "mkdir -p /var/debcd"
 system $ "aptitude -q -F \"%?p=%?V %M\" --disable-columns search \\~i > /var/debcd/"
          ++nowS
 return nowS

rollback :: String -> IO ()
rollback list = void $ do
  system $ "aptitude -q -R --schedule-only install $(awk < /var/debcd/"++
           list++" '{print $1}')"
  system $ "aptitude -q -R --schedule-only markauto $(awk < /var/debcd/"++
           list++" '$2==\"A\" {split($1,A,\"=\");print A[1]}')"
  system $ "aptitude -y -o Dpkg::Options::=\"--force-confdef\" install"

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

getSender :: YConf.Config -> IO ([String] -> IO ())
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
                      return $  runResourceT . sendMailSES man ses . fromString . unlines


psh :: String -> IO (Either String String)
psh cmd =
  bracketOnError
  (createProcess $ (shell cmd) { std_out = CreatePipe
                               , std_err = CreatePipe
                               , create_group = True })
  (\(_, Just hout, Just herr, ph) -> do
      interruptProcessGroupOf ph
      terminateProcess ph
      _ <- slurp hout herr
      _ <- waitForProcess ph
      return $ Left "Terminated")
  (\(_, Just hout, Just herr, ph) -> do
      (sout, serr) <- slurp hout herr
      excode <- waitForProcess ph
      case excode of
        ExitSuccess -> return $ Right sout
        ExitFailure _ -> return $ Left (sout++serr))
  where slurp hout herr = do
          sout <- hGetContents hout ; serr <- hGetContents herr
          waitOut <- forkWait sout  ; waitErr <- forkWait serr
          waitOut                   ; waitErr
          hClose hout               ; hClose herr
          return (sout, serr)
        forkWait a = do
          res <- newEmptyMVar
          _ <- mask $ \restore ->
            forkIO $ try (restore $ C.evaluate $ rnf a) >>= putMVar res
          return (takeMVar res >>=
                  either (\ex -> throwIO (ex :: SomeException)) return)
