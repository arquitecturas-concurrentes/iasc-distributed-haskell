{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure

import DistribUtils

import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

-- <<Message
data Message = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic)

instance Binary Message
-- >>

-- <<pingServer
pingServer :: Process ()
pingServer = do
  Ping from <- expect
  say $ printf "ping received from %s" (show from)
  mypid <- getSelfPid
  send from (Pong mypid)
-- >>

-- <<remotable
remotable ['pingServer]
-- >>

-- <<master
master :: Process ()
master = do
  node <- getSelfNode

  say $ printf "spawning on %s" (show node)
  pid <- spawn node $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid
  say $ printf "sending ping to %s" (show pid)
  send pid (Ping mypid)

  Pong _ <- expect
  say "pong."

  terminate                                         
-- >>

-- <<main
main :: IO ()
main = distribMain (\_ -> master) Main.__remoteTable
-- >>
