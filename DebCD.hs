{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.Yaml.Config as YConf
import System.Environment
import System.Directory
import System.Exit

import qualified Data.Text as T
import Network.Mail.Mime.SES
import Network.Mail.Mime
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.String
import Data.List (sort, intercalate)

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
import Prelude hiding (log)


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
                               repo++".list\" -o Dir::Etc::sourceparts=\"-\" "++
                               "-o APT::Get::List-Cleanup=\"0\""

  updateRes <- psh updateCmd
  case updateRes of
    Left err -> do sender $ ["debcd: error updating APT", 
                               "\nError:\n ", err]
                   logFail $ "apt-update error: "++err 
    Right _ -> do
        upgradesAvailable <- fmap ((/=) ExitSuccess) 
                                  $ system "apt-get -u upgrade --assume-no"                               
        when upgradesAvailable $ upgrade conf sender

upgrade conf sender = do

 let test_delay = YConf.lookupDefault "test-delay-secs" (0::Int) conf

 selections <- createFreezeList

 log $ "upgrading from "++intercalate ", " (map (\(pkg,v)-> pkg++"-"++v) selections)

 putStrLn "upgrading.."
 upgRes <- system "DEBIAN_FRONTEND=noninteractive apt-get upgrade -y"
 putStrLn "..done upgrading"
 case upgRes of
   ExitFailure err -> do rollback selections
                         sender $ ["debcd: error upgrading packages", 
                            "\nError:\n ", show err]
                         logFail $ "apt-upgrade error: "++show err 
                            
                          
   ExitSuccess -> do

       -- run tests
       threadDelay (test_delay* 1000*1000)

       testsOK <- runTests sender

       -- roll back on failure
       when (not testsOK) $ do 
          log $ "rollback"
          rollbackSuccess <- rollback selections
          when (not rollbackSuccess) $ do
             testsOK <- runTests noSender
             if testsOK 
                then do sender ["Rollback failure"]
                        logFail "Rollback failure" 
                else do sender ["Rollback failure, tests failing"]
                        logFail "Rollback failure, tests failing" 

       return ()

-- these two based on http://www.debian-administration.org/article/669/

runTests sender = do
  files <- getDirectoryContents "/etc/debcd/tests.d/"
  testRess <-  forM files $ \file -> do
    p <- getPermissions $ "/etc/debcd/tests.d/"++file
    if not $ executable p
       then return Nothing
       else do debug $ "running test "++ file
               res <- psh $ "/etc/debcd/tests.d/"++file
               case res of 
                 Right _ -> return Nothing
                 Left err -> return $ Just $ "Test: "++file++"\nOutput:\n"++ 
                                               (unlines $ map ("  "++) $ lines err)
  if all isNothing testRess
     then return True
     else do log $ "test failures: "++ unlines (catMaybes testRess)
             sender ("debcd: Test failure":catMaybes testRess)
             return False

createFreezeList :: IO [(String, String)]
createFreezeList = do
 now <- getCurrentTime
 res <- psh $ "aptitude -F%p --disable-columns search ~U"
 case res of 
   Right s -> forM (lines s) $ \pkgNm -> do
                Right version <- psh $ "dpkg-query -W -f='${Version}' "++pkgNm
                return (pkgNm,version)

rollback :: [(String, String)] -> IO Bool -- all pass
rollback list = fmap and $ forM list $ \(pkgNm, ver) -> do
  putStrLn $ "rolling back "++pkgNm++" to version "++ver++".."

  -- this would be the right thing to do if reprepro stored old versions
  -- system $ "apt-get install "++pkgNm++"="++ver 

  -- cheap, fragile hack
  fmap (==ExitSuccess) $ system $ "dpkg -i /var/cache/apt/archives/"++pkgNm++"_"++ver++"_*.deb"

getConfig :: IO YConf.Config
getConfig = do
 allConfig <- YConf.load confPath
 args <- getArgs
 envNm <- case (filter notOption args, YConf.keys allConfig) of
            (env:_, _)  -> return $ T.pack env
            ([], [])    -> logFail $ "No env args, no envs in config file "++confPath
            ([] , envs) -> return $ head $ sort envs

 case YConf.subconfig envNm allConfig of
           Nothing -> logFail $ "Cannot find configuration environment "++
                                 T.unpack envNm++" in "++confPath
           Just c -> do log $ "invoke with environment "++T.unpack envNm
                        return c

notOption ('-':_) = False
notOption _ = True

noSender :: [String] -> IO ()
noSender = const $ return ()

getSender :: YConf.Config -> IO ([String] -> IO ())
getSender conf = 
  let mses = do emailTo <- YConf.lookup "emailTo" conf
                emailFrom <- YConf.lookup "emailFrom" conf
                access <-  YConf.lookup "aws_access" conf
                secret <-  YConf.lookup "aws_secret" conf
                return $ SES
                   { sesFrom = S8.pack $ emailFrom
                   , sesTo = [DTE.encodeUtf8 emailTo]
                   , sesAccessKey = S8.pack $ access
                   , sesSecretKey = S8.pack $ secret
                   }
  in case mses of
       Nothing -> return $ putStrLn . unlines 
       Just ses -> do manager <- newManager conduitManagerSettings
                      return $  \ss -> do
                          let mailPart 
                               = Part { partType = "text/plain; charset=utf-8"
                                      , partEncoding = None, partFilename = Nothing
                                      , partHeaders = []
                                      , partContent = LBS.fromStrict $ DTE.encodeUtf8 $ T.pack $ unlines ss }

                          renderSendMailSES manager ses
                              (emptyMail $ Address (Just "DebCD instance") (DTE.decodeUtf8 $ sesFrom ses))
                                  { mailTo = map (Address Nothing . DTE.decodeUtf8) (sesTo ses)
                                  , mailHeaders = [ ("Subject", "DebCD report") ]
                                  , mailParts = [[mailPart]] }

                          putStrLn $ unlines ss
                          putStrLn "(sent email)"             

psh :: String -> IO (Either String String)
psh cmd =
  bracketOnError
  (debug cmd >> (createProcess $ (shell cmd) { std_out = CreatePipe
                               , std_err = CreatePipe
                               , create_group = True }))
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
        ExitSuccess ->   do debug sout
                            return $ Right sout
        ExitFailure _ -> do debug $ sout++serr
                            return $ Left (sout++serr))
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

debug s = do 
  args <- getArgs 
  when ("--verbose" `elem` args) $ putStrLn s

log :: String -> IO ()
log s = do
  now <- getCurrentTime
  let ts = DTF.formatTime defaultTimeLocale "%F %T" now
  appendFile "/var/log/debcd.log" $ ts++" "++s++"\n"

logFail :: String -> IO a
logFail s = do
  log $ "FATAL ERROR:" ++s
  hPutStrLn stderr s
  exitWith $ ExitFailure 1
  --return undefined