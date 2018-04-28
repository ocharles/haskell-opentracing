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

module Dependency_Client(getDependenciesForTrace,saveDependencies) where
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


import Dependency_Types
import Dependency
seqid = R.newIORef 0
getDependenciesForTrace (ip,op) arg_traceId = do
  send_getDependenciesForTrace op arg_traceId
  recv_getDependenciesForTrace ip
send_getDependenciesForTrace op arg_traceId = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessage op ("getDependenciesForTrace", T.M_CALL, seqn) $
    write_GetDependenciesForTrace_args op (GetDependenciesForTrace_args{getDependenciesForTrace_args_traceId=arg_traceId})
recv_getDependenciesForTrace ip = do
  T.readMessage ip $ \(fname, mtype, rseqid) -> do
    M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; X.throw exn }
    res <- read_GetDependenciesForTrace_result ip
    P.return $ getDependenciesForTrace_result_success res
saveDependencies (ip,op) arg_dependencies = do
  send_saveDependencies op arg_dependencies
send_saveDependencies op arg_dependencies = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessage op ("saveDependencies", T.M_ONEWAY, seqn) $
    write_SaveDependencies_args op (SaveDependencies_args{saveDependencies_args_dependencies=arg_dependencies})