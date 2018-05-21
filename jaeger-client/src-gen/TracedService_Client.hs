{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (0.11.0)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module TracedService_Client(startTrace,joinTrace) where
import qualified Data.IORef as R
import Prelude (($), (.), (>>=), (==), (++))
import qualified Prelude as P
import qualified Control.Exception as X
import qualified Control.Monad as M ( liftM, ap, when )
import Data.Functor ( (<$>) )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import qualified Data.Int as I
import qualified Data.Maybe as M (catMaybes)
import qualified Data.Text.Lazy.Encoding as E ( decodeUtf8, encodeUtf8 )
import qualified Data.Text.Lazy as LT
import qualified GHC.Generics as G (Generic)
import qualified Data.Typeable as TY ( Typeable )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector
import qualified Test.QuickCheck.Arbitrary as QC ( Arbitrary(..) )
import qualified Test.QuickCheck as QC ( elements )

import qualified Thrift as T
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T


import Tracetest_Types
import TracedService
seqid = R.newIORef 0
startTrace (ip,op) arg_request = do
  send_startTrace op arg_request
  recv_startTrace ip
send_startTrace op arg_request = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessage op ("startTrace", T.M_CALL, seqn) $
    write_StartTrace_args op (StartTrace_args{startTrace_args_request=arg_request})
recv_startTrace ip = do
  T.readMessage ip $ \(fname, mtype, rseqid) -> do
    M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; X.throw exn }
    res <- read_StartTrace_result ip
    P.return $ startTrace_result_success res
joinTrace (ip,op) arg_request = do
  send_joinTrace op arg_request
  recv_joinTrace ip
send_joinTrace op arg_request = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessage op ("joinTrace", T.M_CALL, seqn) $
    write_JoinTrace_args op (JoinTrace_args{joinTrace_args_request=arg_request})
recv_joinTrace ip = do
  T.readMessage ip $ \(fname, mtype, rseqid) -> do
    M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; X.throw exn }
    res <- read_JoinTrace_result ip
    P.return $ joinTrace_result_success res
