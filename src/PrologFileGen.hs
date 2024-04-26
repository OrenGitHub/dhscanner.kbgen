{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module PrologFileGen

where

-- project imports
import Location ( Location )

-- project qualified imports
import qualified Fqn
import qualified Token
import qualified KbGen as KnowledgeBase
import qualified Location
import qualified Bitcode

-- general imports
import Data.List
import Data.Aeson
import GHC.Generics

data PrologFile = PrologFile { content :: [ String ] } deriving ( Show, Generic, ToJSON )

toPrologFile :: KnowledgeBase.KnowledgeBase -> PrologFile
toPrologFile kb = let
    calls = toPrologFileCalls (KnowledgeBase.calls kb)
    params = toPrologFileParams (KnowledgeBase.params kb)
    args = toPrologFileArgs (KnowledgeBase.args kb)
    lambdas = toPrologFileLambdas (KnowledgeBase.lambdas kb)
    -- dataflow = toPrologFileDataflow (KnowledgeBase.dataflow kb)
    in PrologFile (calls ++ params ++ args ++ lambdas) -- ++ dataflow)

toPrologFileLambdas :: [ KnowledgeBase.KBLambda ] -> [ String ]
toPrologFileLambdas lambdas = Data.List.foldl' (++) [] (toPrologFileLambdas' lambdas)

toPrologFileLambdas' :: [ KnowledgeBase.KBLambda ] -> [[ String ]]
toPrologFileLambdas' = Data.List.map toPrologFileLambda

toPrologFileLambda :: KnowledgeBase.KBLambda -> [ String ]
toPrologFileLambda lambda = let
    location = KnowledgeBase.lambdaLocation lambda
    locstring = stringify location
    in [ "kb_callable( " ++ locstring ++ " )." ]

toPrologFileArgs :: [ KnowledgeBase.Arg ] -> [ String ]
toPrologFileArgs args = Data.List.foldl' (++) [] (toPrologFileArgs' args)

toPrologFileArgs' :: [ KnowledgeBase.Arg ] -> [[ String ]]
toPrologFileArgs' = Data.List.map toPrologFileArg

toPrologFileArg :: KnowledgeBase.Arg -> [ String ]
toPrologFileArg arg = let
    location = KnowledgeBase.argLocation arg
    locstring = stringify location
    call = stringify (KnowledgeBase.callContext arg)
    in [ "kb_arg_for_call( " ++ locstring ++ ", " ++ call ++ " )." ]

toPrologFileCalls :: [ KnowledgeBase.Call ] -> [ String ]
toPrologFileCalls calls = Data.List.foldl' (++) [] (toPrologFileCalls' calls)

toPrologFileCalls' :: [ KnowledgeBase.Call ] -> [[ String ]]
toPrologFileCalls' = Data.List.map toPrologFileCall

toPrologFileCall :: KnowledgeBase.Call -> [ String ]
toPrologFileCall call = let
    location = KnowledgeBase.callLocation call
    locstring = stringify location
    theCall = "kb_call( " ++ locstring ++ " )."
    quotedFqn = "'" ++ Fqn.content (KnowledgeBase.calleeFqn call) ++ "'"
    theFqn = "kb_has_fqn( " ++ locstring ++ ", " ++ quotedFqn ++ ")."
    in [ theCall, theFqn ]

toPrologFileParams :: [ KnowledgeBase.Param ] -> [ String ]
toPrologFileParams params = Data.List.foldl' (++) [] (toPrologFileParams' params)

toPrologFileParams' :: [ KnowledgeBase.Param ] -> [[ String ]]
toPrologFileParams' = Data.List.map toPrologFileParam

toPrologFileParam :: KnowledgeBase.Param -> [ String ]
toPrologFileParam param = let
    callable = stringify $ KnowledgeBase.callableContext param
    serialIdx = show $ KnowledgeBase.paramSerialIdx param
    name = Token.content (Token.getParamNameToken (KnowledgeBase.paramName param))
    location = Token.getParamNameLocation (KnowledgeBase.paramName param)
    locstring = stringify location
    fact1 = "kb_param( " ++ locstring ++ " )."
    fact2 = "kb_param_has_name(" ++ locstring ++ "," ++ name ++ ")."
    fact3 = "kb_callable_has_param(" ++ locstring ++ "," ++ callable ++ ")."
    in [ fact1, fact2, fact3 ]

stringify :: Location -> String
stringify location = let
    sLine = show $ Location.lineStart location
    sCol  = show $ Location.colStart location
    eLine = show $ Location.lineEnd location
    eCol  = show $ Location.colEnd location
    filename = Location.filename location
    in "startloc_" ++ sLine ++ "_" ++ sCol ++ "_endloc_" ++ eLine ++ "_" ++ eCol
