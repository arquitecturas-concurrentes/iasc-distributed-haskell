{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure

import Control.Monad
import Text.Printf
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

import DistribUtils

-- <<Message
data Message = Ping (SendPort ProcessId)
  deriving (Typeable, Generic)

instance Binary Message
-- >>

-- <<pingServer
pingServer :: Process ()
pingServer = do
  Ping chan <- expect
  say $ printf "ping received from %s" (show chan)
  mypid <- getSelfPid
  sendChan chan mypid
-- >>

-- <<remotable
remotable ['pingServer]
-- >>

-- <<master
master :: [NodeId] -> Process ()
master peers = do

  ps <- forM peers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  mapM_ monitor ps

  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sendport,recvport) <- newChan
    send pid (Ping sendport)
    return recvport

  forM_ ports $ \port -> do             
     _ <- receiveChan port
     return ()

  say "All pongs successfully received"
  terminate
-- >>

-- <<main
main :: IO ()
main = distribMain master Main.__remoteTable
-- >>
