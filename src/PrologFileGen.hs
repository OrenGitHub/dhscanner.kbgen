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
import qualified Callable

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
    dataflow = toPrologFileDataflow (KnowledgeBase.dataflow kb)
    funcs = toPrologFileFuncs (KnowledgeBase.funcs kb)
    subclasses = toPrologFileSubclasses (KnowledgeBase.subclasses kb)
    methodsof = toPrologFileMethodsof (KnowledgeBase.methodsof kb)
    in PrologFile (calls ++ params ++ args ++ lambdas ++ dataflow ++ funcs ++ subclasses ++ methodsof)

toPrologFileMethodsof :: [(Token.MethdName, Location, Token.ClassName)] -> [ String ]
toPrologFileMethodsof methods = Data.List.foldl' (++) [] (toPrologFileMethodsof' methods)

toPrologFileMethodsof' :: [(Token.MethdName, Location, Token.ClassName)] -> [[ String ]]
toPrologFileMethodsof' = Data.List.map toPrologFileMethodof

toPrologFileMethodof :: (Token.MethdName, Location, Token.ClassName) -> [ String ]
toPrologFileMethodof (_, loc, c) = let
    locBased = "kb_method_of_class( " ++ (stringify loc) ++ ", " ++ (Token.content (Token.getClassNameToken c)) ++ " )."
    in [ locBased ]

toPrologFileDataflow :: [ KnowledgeBase.Edge ] -> [ String ]
toPrologFileDataflow edges = Data.List.foldl' (++) [] (toPrologFileEdges edges)

toPrologFileEdges :: [ KnowledgeBase.Edge ] -> [[ String ]]
toPrologFileEdges = Data.List.map toPrologFileEdge

toPrologFileEdge :: KnowledgeBase.Edge -> [ String ]
toPrologFileEdge edge = let
    u = stringify $ Bitcode.locationVariable (KnowledgeBase.from edge)
    v = stringify $ Bitcode.locationVariable (KnowledgeBase.to edge)
    dataflowEdge = "kb_dataflow_edge( " ++ u ++ ", " ++ v ++ " )."
    fqnU = "kb_has_fqn( " ++ u ++ ", " ++ "'" ++ Fqn.content (Bitcode.variableFqn (KnowledgeBase.from edge)) ++ "'" ++ " )."
    fqnV = "kb_has_fqn( " ++ v ++ ", " ++ "'" ++ Fqn.content (Bitcode.variableFqn (KnowledgeBase.to edge)) ++ "'" ++ " )."
    in [ dataflowEdge, fqnU, fqnV ]

toPrologFileSubclasses :: [(Token.ClassName, Token.SuperName)] -> [ String ]
toPrologFileSubclasses tuples = Data.List.foldl' (++) [] (toPrologFileSubclasses' tuples)

toPrologFileSubclasses' :: [(Token.ClassName, Token.SuperName)] -> [[ String ]]
toPrologFileSubclasses' = Data.List.map toPrologFileSubclass

toPrologFileSubclass :: (Token.ClassName, Token.SuperName) -> [ String ]
toPrologFileSubclass (c,s) = let
    c' = Token.content (Token.getClassNameToken c)
    s' = Token.content (Token.getSuperNameToken s)
    subclass = "kb_subclass_of( " ++ c' ++ ", " ++ s' ++ " )."
    className = "kb_class_name( " ++ c' ++ ", " ++ "'" ++ c' ++ "'" ++ " )."
    superName = "kb_class_name( " ++ s' ++ ", " ++ "'" ++ s' ++ "'" ++ " )."
    in [ className, superName, subclass ]

toPrologFileLambdas :: [ KnowledgeBase.KBLambda ] -> [ String ]
toPrologFileLambdas lambdas = Data.List.foldl' (++) [] (toPrologFileLambdas' lambdas)

toPrologFileLambdas' :: [ KnowledgeBase.KBLambda ] -> [[ String ]]
toPrologFileLambdas' = Data.List.map toPrologFileLambda

toPrologFileLambda :: KnowledgeBase.KBLambda -> [ String ]
toPrologFileLambda lambda = let
    location = KnowledgeBase.lambdaLocation lambda
    locstring = stringify location
    in [ "kb_callable( " ++ locstring ++ " )." ]

toPrologFileFuncs :: [ KnowledgeBase.KBCallable ] -> [ String ]
toPrologFileFuncs funcs = Data.List.foldl' (++) [] (toPrologFileFuncs' funcs)

toPrologFileFuncs' :: [ KnowledgeBase.KBCallable ] -> [[ String ]]
toPrologFileFuncs' = Data.List.map toPrologFileCallable

toPrologFileCallable :: KnowledgeBase.KBCallable -> [ String ]
toPrologFileCallable callable = let
    quotedFqn = "'" ++ Fqn.content (KnowledgeBase.callableFqn callable) ++ "'"
    location = KnowledgeBase.callableLocation callable
    locstring = stringify location
    annotations = KnowledgeBase.callableAnnotations callable
    annotation = case annotations of { [] -> "moishe.zuchmir"; ((Callable.Annotation a _):_) -> a }
    quotedAnnotation = "'" ++ annotation ++ "'"
    callable_loc = "kb_callable( " ++ locstring ++ " )."
    callable_fqn = "kb_has_fqn( " ++ locstring ++ ", " ++ quotedFqn ++ " )."
    callable_annotation = "kb_callable_annotated_with( " ++ locstring ++ ", " ++ quotedAnnotation ++ " )."
    in [ callable_loc, callable_fqn, callable_annotation ]

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
    theFqn = "kb_has_fqn( " ++ locstring ++ ", " ++ quotedFqn ++ " )."
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
    fact2 = "kb_param_has_name(" ++ locstring ++ "," ++ "'" ++ name ++ "'" ++ ")."
    fact3 = "kb_callable_has_param( " ++ callable ++ ", " ++ locstring ++ " )."
    in [ fact1, fact2, fact3 ]

stringify :: Location -> String
stringify location = let
    sLine = show $ Location.lineStart location
    sCol  = show $ Location.colStart location
    eLine = show $ Location.lineEnd location
    eCol  = show $ Location.colEnd location
    filename = Location.filename location
    in "startloc_" ++ sLine ++ "_" ++ sCol ++ "_endloc_" ++ eLine ++ "_" ++ eCol
