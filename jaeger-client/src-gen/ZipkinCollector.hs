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

module ZipkinCollector where
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


import Zipkincore_Types
import qualified ZipkinCollector_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data SubmitZipkinBatch_args = SubmitZipkinBatch_args  { submitZipkinBatch_args_spans :: (Vector.Vector Span)
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable SubmitZipkinBatch_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` submitZipkinBatch_args_spans record  
instance QC.Arbitrary SubmitZipkinBatch_args where 
  arbitrary = M.liftM SubmitZipkinBatch_args (QC.arbitrary)
  shrink obj | obj == default_SubmitZipkinBatch_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_SubmitZipkinBatch_args{submitZipkinBatch_args_spans = submitZipkinBatch_args_spans obj} then P.Nothing else P.Just $ default_SubmitZipkinBatch_args{submitZipkinBatch_args_spans = submitZipkinBatch_args_spans obj}
    ]
from_SubmitZipkinBatch_args :: SubmitZipkinBatch_args -> T.ThriftVal
from_SubmitZipkinBatch_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v57 -> P.Just (1, ("spans",T.TList (T.T_STRUCT typemap_Span) $ P.map (\_v59 -> from_Span _v59) $ Vector.toList _v57))) $ submitZipkinBatch_args_spans record
  ]
write_SubmitZipkinBatch_args :: T.Protocol p => p -> SubmitZipkinBatch_args -> P.IO ()
write_SubmitZipkinBatch_args oprot record = T.writeVal oprot $ from_SubmitZipkinBatch_args record
encode_SubmitZipkinBatch_args :: T.StatelessProtocol p => p -> SubmitZipkinBatch_args -> LBS.ByteString
encode_SubmitZipkinBatch_args oprot record = T.serializeVal oprot $ from_SubmitZipkinBatch_args record
to_SubmitZipkinBatch_args :: T.ThriftVal -> SubmitZipkinBatch_args
to_SubmitZipkinBatch_args (T.TStruct fields) = SubmitZipkinBatch_args{
  submitZipkinBatch_args_spans = P.maybe (submitZipkinBatch_args_spans default_SubmitZipkinBatch_args) (\(_,_val61) -> (case _val61 of {T.TList _ _val62 -> (Vector.fromList $ P.map (\_v63 -> (case _v63 of {T.TStruct _val64 -> (to_Span (T.TStruct _val64)); _ -> P.error "wrong type"})) _val62); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_SubmitZipkinBatch_args _ = P.error "not a struct"
read_SubmitZipkinBatch_args :: T.Protocol p => p -> P.IO SubmitZipkinBatch_args
read_SubmitZipkinBatch_args iprot = to_SubmitZipkinBatch_args <$> T.readVal iprot (T.T_STRUCT typemap_SubmitZipkinBatch_args)
decode_SubmitZipkinBatch_args :: T.StatelessProtocol p => p -> LBS.ByteString -> SubmitZipkinBatch_args
decode_SubmitZipkinBatch_args iprot bs = to_SubmitZipkinBatch_args $ T.deserializeVal iprot (T.T_STRUCT typemap_SubmitZipkinBatch_args) bs
typemap_SubmitZipkinBatch_args :: T.TypeMap
typemap_SubmitZipkinBatch_args = Map.fromList [(1,("spans",(T.T_LIST (T.T_STRUCT typemap_Span))))]
default_SubmitZipkinBatch_args :: SubmitZipkinBatch_args
default_SubmitZipkinBatch_args = SubmitZipkinBatch_args{
  submitZipkinBatch_args_spans = Vector.empty}
data SubmitZipkinBatch_result = SubmitZipkinBatch_result  { submitZipkinBatch_result_success :: (Vector.Vector Response)
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable SubmitZipkinBatch_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` submitZipkinBatch_result_success record  
instance QC.Arbitrary SubmitZipkinBatch_result where 
  arbitrary = M.liftM SubmitZipkinBatch_result (QC.arbitrary)
  shrink obj | obj == default_SubmitZipkinBatch_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_SubmitZipkinBatch_result{submitZipkinBatch_result_success = submitZipkinBatch_result_success obj} then P.Nothing else P.Just $ default_SubmitZipkinBatch_result{submitZipkinBatch_result_success = submitZipkinBatch_result_success obj}
    ]
from_SubmitZipkinBatch_result :: SubmitZipkinBatch_result -> T.ThriftVal
from_SubmitZipkinBatch_result record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v67 -> P.Just (0, ("success",T.TList (T.T_STRUCT typemap_Response) $ P.map (\_v69 -> from_Response _v69) $ Vector.toList _v67))) $ submitZipkinBatch_result_success record
  ]
write_SubmitZipkinBatch_result :: T.Protocol p => p -> SubmitZipkinBatch_result -> P.IO ()
write_SubmitZipkinBatch_result oprot record = T.writeVal oprot $ from_SubmitZipkinBatch_result record
encode_SubmitZipkinBatch_result :: T.StatelessProtocol p => p -> SubmitZipkinBatch_result -> LBS.ByteString
encode_SubmitZipkinBatch_result oprot record = T.serializeVal oprot $ from_SubmitZipkinBatch_result record
to_SubmitZipkinBatch_result :: T.ThriftVal -> SubmitZipkinBatch_result
to_SubmitZipkinBatch_result (T.TStruct fields) = SubmitZipkinBatch_result{
  submitZipkinBatch_result_success = P.maybe (submitZipkinBatch_result_success default_SubmitZipkinBatch_result) (\(_,_val71) -> (case _val71 of {T.TList _ _val72 -> (Vector.fromList $ P.map (\_v73 -> (case _v73 of {T.TStruct _val74 -> (to_Response (T.TStruct _val74)); _ -> P.error "wrong type"})) _val72); _ -> P.error "wrong type"})) (Map.lookup (0) fields)
  }
to_SubmitZipkinBatch_result _ = P.error "not a struct"
read_SubmitZipkinBatch_result :: T.Protocol p => p -> P.IO SubmitZipkinBatch_result
read_SubmitZipkinBatch_result iprot = to_SubmitZipkinBatch_result <$> T.readVal iprot (T.T_STRUCT typemap_SubmitZipkinBatch_result)
decode_SubmitZipkinBatch_result :: T.StatelessProtocol p => p -> LBS.ByteString -> SubmitZipkinBatch_result
decode_SubmitZipkinBatch_result iprot bs = to_SubmitZipkinBatch_result $ T.deserializeVal iprot (T.T_STRUCT typemap_SubmitZipkinBatch_result) bs
typemap_SubmitZipkinBatch_result :: T.TypeMap
typemap_SubmitZipkinBatch_result = Map.fromList [(0,("success",(T.T_LIST (T.T_STRUCT typemap_Response))))]
default_SubmitZipkinBatch_result :: SubmitZipkinBatch_result
default_SubmitZipkinBatch_result = SubmitZipkinBatch_result{
  submitZipkinBatch_result_success = Vector.empty}
process_submitZipkinBatch (seqid, iprot, oprot, handler) = do
  args <- read_SubmitZipkinBatch_args iprot
  (X.catch
    (do
      val <- Iface.submitZipkinBatch handler (submitZipkinBatch_args_spans args)
      let res = default_SubmitZipkinBatch_result{submitZipkinBatch_result_success = val}
      T.writeMessage oprot ("submitZipkinBatch", T.M_REPLY, seqid) $
        write_SubmitZipkinBatch_result oprot res)
    ((\_ -> do
      T.writeMessage oprot ("submitZipkinBatch", T.M_EXCEPTION, seqid) $
        T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")) :: X.SomeException -> P.IO ()))
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "submitZipkinBatch" -> process_submitZipkinBatch (seqid,iprot,oprot,handler)
  _ -> do
    _ <- T.readVal iprot (T.T_STRUCT Map.empty)
    T.writeMessage oprot (name,T.M_EXCEPTION,seqid) $
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN_METHOD ("Unknown function " ++ LT.unpack name))
process handler (iprot, oprot) = do
  T.readMessage iprot (
    proc_ handler (iprot,oprot))
  P.return P.True
