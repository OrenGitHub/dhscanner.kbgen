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
import Data.Text ( replace, pack, unpack )
import Data.Aeson
import GHC.Generics
import Data.List.Split
import System.FilePath ( splitPath, joinPath, takeBaseName )

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
    methodvars = toPrologFileMethodvars (KnowledgeBase.methodvars kb)
    strings = toPrologFileStrings (KnowledgeBase.strings kb)
    returns = toPrologFileReturns (KnowledgeBase.returns kb)
    in PrologFile (calls ++ params ++ args ++ lambdas ++ dataflow ++ funcs ++ subclasses ++ methodsof ++ methodvars ++ strings ++ returns)

toPrologFileReturn :: (Bitcode.Variable, Location) -> String
toPrologFileReturn (v, callable) = let
    src = stringify (Bitcode.locationVariable v)
    dst = stringify callable
    in "kb_dataflow_edge( " ++ src ++ ", " ++ dst ++ " )."

toPrologFileReturns :: [(Bitcode.Variable, Location)] -> [ String ]
toPrologFileReturns = Data.List.map toPrologFileReturn

toPrologFileStrings :: [ Token.ConstStr ] -> [ String ]
toPrologFileStrings = Data.List.map toPrologFileString

escapeSingleQuotes :: String -> String
escapeSingleQuotes = concatMap (\c -> if c == '\'' then "\\'" else [c])

toPrologFileString :: Token.ConstStr -> String
toPrologFileString s = let
    loc = stringify (Token.constStrLocation s)
    value = Token.constStrValue s
    escapedSingleQuotes = escapeSingleQuotes value
    in "kb_const_string( " ++ loc ++ ", " ++ "'" ++ escapedSingleQuotes ++ "' )."

toPrologFileMethodvars :: [(Bitcode.Variable, Location)] -> [ String ]
toPrologFileMethodvars = Data.List.map toPrologFileMethodvar

toPrologFileMethodvar :: (Bitcode.Variable, Location) -> String
toPrologFileMethodvar (v, methodloc) = let
    vstr = stringify (Bitcode.locationVariable v)
    mstr = stringify methodloc
    in "kb_var_in_method( " ++ vstr ++ ", " ++ mstr ++ " )."

toPrologFileMethodsof :: [(Token.MethdName, Location, Token.ClassName)] -> [ String ]
toPrologFileMethodsof methods = Data.List.foldl' (++) [] (toPrologFileMethodsof' methods)

toPrologFileMethodsof' :: [(Token.MethdName, Location, Token.ClassName)] -> [[ String ]]
toPrologFileMethodsof' = Data.List.map toPrologFileMethodof

toPrologFileMethodof :: (Token.MethdName, Location, Token.ClassName) -> [ String ]
toPrologFileMethodof (_, loc, c) = let
    classLoc = stringify (Token.location (Token.getClassNameToken c))
    locBased = "kb_method_of_class( " ++ (stringify loc) ++ ", " ++ classLoc ++ " )."
    in [ locBased ]

toPrologFileDataflow :: [ KnowledgeBase.Edge ] -> [ String ]
toPrologFileDataflow edges = Data.List.foldl' (++) [] (toPrologFileEdges edges)

toPrologFileEdges :: [ KnowledgeBase.Edge ] -> [[ String ]]
toPrologFileEdges = Data.List.map toPrologFileEdge

omitNewFromFqn :: String -> String
omitNewFromFqn fqn = unpack (replace (pack ".new.") (pack ".") (pack fqn))

-- Arrrggghh ... this is an ugly patch
-- until I decide what to do with the fqns
replaceConstructorNameInFqn :: String -> String
replaceConstructorNameInFqn fqn = unpack (replace (pack ".initialize") (pack ".new") (pack fqn))

toPrologFileEdge :: KnowledgeBase.Edge -> [ String ]
toPrologFileEdge edge = let
    u = stringify $ Bitcode.locationVariable (KnowledgeBase.from edge)
    v = stringify $ Bitcode.locationVariable (KnowledgeBase.to edge)
    dataflowEdge = "kb_dataflow_edge( " ++ u ++ ", " ++ v ++ " )."
    fqnU = "kb_has_fqn( " ++ u ++ ", " ++ "'" ++ omitNewFromFqn (Fqn.content (Bitcode.variableFqn (KnowledgeBase.from edge))) ++ "'" ++ " )."
    fqnV = "kb_has_fqn( " ++ v ++ ", " ++ "'" ++ omitNewFromFqn (Fqn.content (Bitcode.variableFqn (KnowledgeBase.to   edge))) ++ "'" ++ " )."
    in [ dataflowEdge, fqnU, fqnV ]

toPrologFileSubclasses :: [(Token.ClassName, Fqn.Fqn)] -> [ String ]
toPrologFileSubclasses tuples = Data.List.foldl' (++) [] (toPrologFileSubclasses' tuples)

toPrologFileSubclasses' :: [(Token.ClassName, Fqn.Fqn)] -> [[ String ]]
toPrologFileSubclasses' = Data.List.map toPrologFileSubclass

toPrologFileSubclass :: (Token.ClassName, Fqn.Fqn) -> [ String ]
toPrologFileSubclass (c,s) = let
    loc = stringify (Token.location (Token.getClassNameToken c))
    c' = Token.content (Token.getClassNameToken c)
    filename = takeBaseName (Location.filename (Token.location (Token.getClassNameToken c)))
    s' = Fqn.content s
    subclass = "kb_subclass_of( " ++ loc ++ ", " ++ "'" ++ s' ++ "'" ++ " )."
    className = "kb_class_name( " ++ loc ++ ", " ++ "'" ++ c' ++ "'" ++ " )."
    className' = "kb_class_name( " ++ loc ++ ", " ++ "'" ++ filename ++ "." ++ c' ++ "'" ++ " )."
    in [ className, className', subclass ]

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
    callable_fqn = "kb_has_fqn( " ++ locstring ++ ", " ++ (omitNewFromFqn quotedFqn) ++ " )."
    ctor_hack = "kb_has_fqn( " ++ locstring ++ ", " ++ (replaceConstructorNameInFqn quotedFqn) ++ " )."
    callable_annotation = "kb_callable_annotated_with( " ++ locstring ++ ", " ++ quotedAnnotation ++ " )."
    in [ callable_loc, callable_fqn, callable_annotation, ctor_hack ]

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

addClassContextIfNeeded :: KnowledgeBase.Call -> [ String ]
addClassContextIfNeeded call = let
    location = KnowledgeBase.callLocation call
    locstring = stringify location
    rawFqn = Fqn.content (KnowledgeBase.calleeFqn call)
    classContext = KnowledgeBase.callFromClass call
    f = Token.content . Token.getClassNameToken
    g = \s -> "kb_has_fqn( " ++ locstring ++ ", " ++ "'" ++ s ++ "." ++ rawFqn ++ "' )."
    in case fmap (g . f) classContext of { Nothing -> []; Just oneMoreFact -> [ oneMoreFact ] }

addEnclosingMethodIfRelevant :: KnowledgeBase.Call -> [ String ]
addEnclosingMethodIfRelevant call = let
    l = stringify (KnowledgeBase.callLocation call)
    m = KnowledgeBase.calledFromMethodLoc call
    in case m of
        (Just m') -> ["kb_called_from_method( " ++ l ++ ", " ++ (stringify m') ++ " )."]
        _ -> []

messageify' :: String -> (String, Int) -> String
messageify' locstring (part, idx) = "kb_has_fqn_parts( " ++ locstring ++ ", " ++ (show idx) ++ ", '" ++ part ++ "' )."

messageify :: String -> [(String, Int)] -> [ String ]
messageify locstring parts = Data.List.map (messageify' locstring) parts

fqnPartify :: String -> String -> [ String ]
fqnPartify locstring rawFqn = let
    parts = Data.List.Split.splitOn "." rawFqn
    in messageify locstring (zip parts [0..])

toPrologFileCall :: KnowledgeBase.Call -> [ String ]
toPrologFileCall call = let
    location = KnowledgeBase.callLocation call
    locstring = stringify location
    theCall = "kb_call( " ++ locstring ++ " )."
    rawFqn = Fqn.content (KnowledgeBase.calleeFqn call)
    fqnParts = fqnPartify locstring rawFqn
    in [ theCall] ++ fqnParts ++ (addClassContextIfNeeded call) ++ (addEnclosingMethodIfRelevant call)

toPrologFileParams :: [ KnowledgeBase.Param ] -> [ String ]
toPrologFileParams params = Data.List.foldl' (++) [] (toPrologFileParams' params)

toPrologFileParams' :: [ KnowledgeBase.Param ] -> [[ String ]]
toPrologFileParams' = Data.List.map toPrologFileParam

toPrologFileParam :: KnowledgeBase.Param -> [ String ]
toPrologFileParam param = let
    callable = stringify $ KnowledgeBase.callableContext param
    serialIdx = show $ KnowledgeBase.paramSerialIdx param
    token = Token.getParamNameToken (KnowledgeBase.paramName param)
    name = Token.content token
    locationCallable = KnowledgeBase.callableContext param
    locstringCallable = stringify locationCallable
    locationParam = Token.location token
    locstringParam = stringify locationParam
    nominalType = Fqn.content (KnowledgeBase.paramNominalType param)
    fact1 = "kb_param( " ++ locstringParam ++ " )."
    fact2 = "kb_param_has_name(" ++ locstringParam ++ "," ++ "'" ++ name ++ "'" ++ ")."
    fact3 = "kb_param_has_type(" ++ locstringParam ++ "," ++ "'" ++ nominalType ++ "'" ++ ")."
    fact4 = "kb_callable_has_param( " ++ locstringCallable ++ ", " ++ locstringParam ++ " )."
    in [ fact1, fact2, fact3, fact4 ]

normalizeChar :: Char -> String
normalizeChar '/' = "_slash_"
normalizeChar '.' = "_dot_"
normalizeChar c = [c]

normalize :: FilePath -> FilePath
normalize path = concatMap normalizeChar (joinPath (drop 3 (splitPath path)))

stringify :: Location -> String
stringify location = let
    sLine = show $ Location.lineStart location
    sCol  = show $ Location.colStart location
    eLine = show $ Location.lineEnd location
    eCol  = show $ Location.colEnd location
    filename = Location.filename location
    in "startloc_" ++ sLine ++ "_" ++ sCol ++ "_endloc_" ++ eLine ++ "_" ++ eCol ++ "_" ++ (normalize filename)
