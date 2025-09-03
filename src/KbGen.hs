{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Werror=missing-fields #-}

module KbGen

where

-- project imports
import Fqn
import Location
import Callable
import Cfg ( Cfg )

-- project qualified imports
import qualified Cfg
import qualified Token
import qualified Bitcode

-- general imports
import Data.Map
import Data.Set
import Data.List
import Data.Maybe
import Data.Aeson
import Data.Sequence
import Data.Foldable
import Control.Monad.State.Lazy

-- general imports (with hidden names)
import GHC.Generics hiding ( from, to )

data KnowledgeBase
   = KnowledgeBase
     {
          calls :: [ Call ],
          args :: [ Arg ],
          lambdas :: [ KBLambda ],
          params :: [ Param ],
          dataflow :: [ Edge ],
          funcs :: [ KBCallable ],
          subclasses :: [(Token.ClassName, Fqn)],
          methodsof :: [(Token.MethodName, Location, Token.ClassName)],
          methodvars :: [(Bitcode.Variable, Location)],
          strings :: [Token.ConstStr],
          returns :: [(Bitcode.Variable, Location)],
          dataflow_v2 :: [ DataflowEdge ],
          subscripts :: [(Bitcode.Variable, Bitcode.Variable, Bitcode.Variable)],
          control_flow :: [ ControlFlowEdge ]
     }
     deriving ( Show, Generic, ToJSON )

data DataflowEdge
   = DataflowEdgeCall Edge
   | DataflowEdgeBinop Edge
   | DataflowEdgeAssign Edge
   | DataflowEdgeFieldRead Edge
   | DataflowEdgeWriteField Edge
   | DataflowEdgeSubscriptRead Edge
   deriving ( Show, Generic, ToJSON )

data Edge
   = Edge
     {
         from :: Bitcode.Variable,
         to :: Bitcode.Variable
     }
     deriving ( Show, Generic, ToJSON )

data ControlFlowEdge
   = ControlFlowEdge
     {
         start :: Bitcode.Instruction,
         end :: Bitcode.Instruction
     }
     deriving ( Show, Generic, ToJSON )

data Call
   = Call
     {
         calleeFqn :: Fqn,
         callLocation :: Location,
         calledFrom :: Maybe Location,
         callFromClass :: Maybe Token.ClassName
     }
     deriving ( Show, Generic, ToJSON )

data Arg
   = Arg
     {
         argFqn :: Fqn,
         argSerialIdx :: Word,
         argLocation :: Location,
         callContext :: Location
     }
     deriving ( Show, Generic, ToJSON )

data KBLambda
   = KBLambda
     {
         lambdaLocation :: Location
     }
     deriving ( Show, Generic, ToJSON )

data KBCallable
   = KBCallable
     {
         callableFqn :: Fqn,
         callableAnnotations :: [ Callable.Annotation ],
         callableLocation :: Location
     }
     deriving ( Show, Generic, ToJSON )

data Param
   = Param
     {
         paramSerialIdx :: Word,
         paramName :: Token.ParamName,
         paramNominalType :: Fqn,
         callableContext :: Location
     }
     deriving ( Show, Generic, ToJSON )

emptyKnowledgeBase :: KnowledgeBase
emptyKnowledgeBase = KnowledgeBase [] [] [] [] [] [] [] [] [] [] [] [] [] []

-- | API: generate a prolog knowledge base from a single callable
kbGen :: Callable -> KnowledgeBase
kbGen (Callable.Script script) = kbGenScript script
kbGen (Callable.Lambda lambda) = kbGenLambda lambda
kbGen (Callable.Method method) = kbGenMethod method
kbGen (Callable.Function func) = kbGenFunction func

kbGenMethod:: Callable.MethodContent -> KnowledgeBase
kbGenMethod method = let
    hostingClass = Callable.hostingClassName method
    body = Callable.methodBody method
    name = Callable.methodName method
    loc = Callable.methodLocation method
    calls' = kbGenCallsFromMethods body loc (Just hostingClass)
    args' = kbGenArgs (Callable.methodBody method)
    vars' = kbGenVars (Callable.methodBody method)
    params' = extractMethodParams method
    dataflow' = extractMethodDataflow method
    funcs' = [ KBCallable (fqnifyMethod method) [] (Callable.methodLocation method) ]
    funcs'' = [ KBCallable (fqnifyMethod' method) [] (Callable.methodLocation method) ]
    funcs''' = funcs' ++ funcs''
    supers = Callable.hostingClassSupers method
    subclasses' = [(hostingClass, s) | s <- supers ]
    methodsof' = [(Callable.methodName method, Callable.methodLocation method, hostingClass)]
    methodvars' = [(v, Callable.methodLocation method) | v <- vars']
    strings' = kbGenStrings (Callable.methodBody method)
    returnValues = kbGenReturns body
    returns' = [(v, loc) | v <- returnValues ]
    subscripts' = extractSubscripts body
    control_flow_edges = extractControlFlowEdges body
    in KnowledgeBase calls' args' [] params' dataflow' funcs''' subclasses' methodsof' methodvars' strings' returns' [] subscripts' control_flow_edges

kbGenFunction :: Callable.FunctionContent -> KnowledgeBase
kbGenFunction func = let
    args' = kbGenArgs (Callable.funcBody func)
    body = Callable.funcBody func
    loc = Callable.funcLocation func
    calls' = kbGenCallsFromMethods body loc Nothing
    params' = extractFunctionParams func
    dataflow' = extractFunctionDataflow func
    annotations' = Callable.funcAnnotations func
    funcs' = [ KBCallable (fqnifyFunc func) annotations' (Callable.funcLocation func) ]
    strings' = kbGenStrings (Callable.funcBody func)
    returnValues = kbGenReturns body
    returns' = [(v, loc) | v <- returnValues ]
    in KnowledgeBase calls' args' [] params' dataflow' funcs' [] [] [] strings' returns' [] [] []

kbGenScript :: Callable.ScriptContent -> KnowledgeBase
kbGenScript script = let
    calls' = kbGenScriptCalls script
    args' = kbGenScriptArgs script
    strings' = kbGenStrings (Callable.scriptBody script)
    in KnowledgeBase calls' args' [] [] [] [] [] [] [] strings' [] [] [] []

kbGenLambda :: Callable.LambdaContent -> KnowledgeBase
kbGenLambda lambda = let
    calls' = kbGenCalls (Callable.lambdaBody lambda)
    args' = kbGenArgs (Callable.lambdaBody lambda)
    lambdas' = [ KBLambda (Callable.lambdaLocation lambda) ]
    params' = extractLambdaParams lambda
    dataflow' = extractLambdaDataflow lambda
    strings' = kbGenStrings (Callable.lambdaBody lambda)
    in KnowledgeBase calls' args' lambdas' params' dataflow' [] [] [] [] strings' [] [] [] []

getEdges :: Cfg -> [ Cfg.Edge ]
getEdges = Data.Set.toList . Cfg.actualEdges . Cfg.edges

getPairNodes :: Cfg.Edge -> (Cfg.Node, Cfg.Node)
getPairNodes edge = (Cfg.from edge, Cfg.to edge)

getNodes :: [ Cfg.Edge ] -> [ (Cfg.Node, Cfg.Node) ]
getNodes = Data.List.map getPairNodes

getPairInstructions :: (Cfg.Node, Cfg.Node) -> (Bitcode.Instruction, Bitcode.Instruction)
getPairInstructions (u, v) = (Cfg.theInstructionInside u, Cfg.theInstructionInside v)

getInstructions :: [(Cfg.Node, Cfg.Node)] -> [(Bitcode.Instruction, Bitcode.Instruction)]
getInstructions = Data.List.map getPairInstructions

insertEdge :: (Bitcode.Instruction, Bitcode.Instruction) -> Map Bitcode.Instruction [Bitcode.Instruction] -> Map Bitcode.Instruction [Bitcode.Instruction]
insertEdge (u,v) = Data.Map.insertWith (++) u [v]

data Graph = Graph { adjacencyList :: Map Bitcode.Instruction [ Bitcode.Instruction ] }

data BFSState
   = BFSState
     {
         visited :: Set Bitcode.Instruction,
         result :: Seq ControlFlowEdge,
         graph :: Graph,
         root :: Bitcode.Instruction
     }

type BFSContext = State BFSState

pushback :: Seq a -> Seq a -> Seq a
pushback queue newElements = queue >< newElements

keepJustCalls :: Bitcode.Instruction -> Bool
keepJustCalls (Bitcode.Instruction _ (Bitcode.Call _)) = True
keepJustCalls _ = False

removeCalls :: Bitcode.Instruction -> Bool
removeCalls = not . keepJustCalls

getSuccessors :: Graph -> Bitcode.Instruction -> [ Bitcode.Instruction ]
getSuccessors (Graph graph) node = case Data.Map.lookup node graph of {
    Nothing -> [];
    Just _successors -> _successors
}

bfs'' :: Bitcode.Instruction -> Seq Bitcode.Instruction -> BFSContext ()
bfs'' head tail = do
    ctx <- get
    let successors = getSuccessors (graph ctx) head
    let calls = Data.List.filter keepJustCalls successors
    let calls' = [ ControlFlowEdge (root ctx) c | c <- calls ]
    let noncalls = Data.List.filter removeCalls successors
    let visited' = Data.Set.insert head (visited ctx)
    let result' = Data.Sequence.fromList calls' >< result ctx
    put $ ctx { visited = visited', result = result' }
    bfs (pushback tail (Data.Sequence.fromList noncalls))

bfs' :: Bitcode.Instruction -> Seq Bitcode.Instruction -> BFSContext ()
bfs' head tail = do { ctx <- get; case Data.Set.member head (visited ctx) of { True -> bfs tail; _ -> bfs'' head tail } }

bfs :: Seq Bitcode.Instruction -> BFSContext ()
bfs queue = case viewl queue of { EmptyL -> return (); head :< tail -> bfs' head tail }

init_bfs_state :: Graph -> Bitcode.Instruction -> BFSState
init_bfs_state g r = BFSState { visited = Data.Set.empty, result = Data.Sequence.empty, graph = g, root = r }

run_bfs :: Graph -> Bitcode.Instruction -> [ ControlFlowEdge ]
run_bfs g r =  Data.Foldable.toList (result (execState (bfs (Data.Sequence.singleton r)) (init_bfs_state g r)))

mkGraph' :: [(Bitcode.Instruction, Bitcode.Instruction)] -> Graph
mkGraph' = Graph . Data.List.foldr insertEdge Data.Map.empty

mkGraph :: Cfg -> Graph
mkGraph = mkGraph' . getInstructions . getNodes . getEdges

run_bfs_from_all_calls :: Graph -> [ Bitcode.Instruction ] -> [ ControlFlowEdge ]
run_bfs_from_all_calls graph calls = concatMap (run_bfs graph) calls

extractControlFlowEdges :: Cfg -> [ ControlFlowEdge ]
extractControlFlowEdges cfg = let
    graph = mkGraph cfg
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.toList $ Data.Set.map Cfg.theInstructionInside nodes
    calls = Data.List.filter keepJustCalls instructions
    entrypoint = Cfg.theInstructionInside (Cfg.entry cfg)
    in run_bfs_from_all_calls graph (entrypoint:calls)

extractSubscripts :: Cfg -> [ (Bitcode.Variable,Bitcode.Variable,Bitcode.Variable) ]
extractSubscripts cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    subscriptReads = catMaybes (Data.Set.toList (Data.Set.map keepSubscriptReads instructions))
    in [(Bitcode.subscriptReadOutput i,Bitcode.subscriptReadInput i,Bitcode.subscriptReadIdx i) | i <- subscriptReads ]

fqnifyMethod' :: Callable.MethodContent -> Fqn
fqnifyMethod' = Fqn . Token.content . Token.getMethodNameToken . Callable.methodName

fqnifyMethod :: Callable.MethodContent -> Fqn
fqnifyMethod method = let
    rawMethodName = Token.content (Token.getMethodNameToken (Callable.methodName method))
    hostingClassName = Token.content (Token.getClassNameToken (Callable.hostingClassName method))
    in Fqn (hostingClassName ++ "." ++ rawMethodName)

fqnifyFunc :: Callable.FunctionContent -> Fqn
fqnifyFunc func = let
    funcName = Token.content (Token.getFuncNameToken (Callable.funcName func))
    in Fqn funcName

extractFunctionDataflow :: Callable.FunctionContent -> [ Edge ]
extractFunctionDataflow func = extractDataflow (Callable.funcBody func)

extractMethodDataflow :: Callable.MethodContent -> [ Edge ]
extractMethodDataflow method = extractDataflow (Callable.methodBody method)

extractLambdaDataflow :: Callable.LambdaContent -> [ Edge ]
extractLambdaDataflow lambda = extractDataflow (Callable.lambdaBody lambda)

extractDataflow :: Cfg -> [ Edge ]
extractDataflow cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    in Data.List.foldl' (++) [] (dataflowEdges (Data.Set.toList instructions))

dataflowEdges :: [ Bitcode.InstructionContent ] -> [[ Edge ]]
dataflowEdges = Data.List.map dataflowEdges'

dataflowEdges' :: Bitcode.InstructionContent -> [ Edge ]
dataflowEdges' (Bitcode.Call call)              = dataflowCallEdges call
dataflowEdges' (Bitcode.Binop binop)            = dataflowBinopEdge binop
dataflowEdges' (Bitcode.Assign assign)          = [ dataflowAssignEdge assign ]
dataflowEdges' (Bitcode.FieldRead fieldRead)    = [ dataflowFieldReadEdge fieldRead ]
dataflowEdges' (Bitcode.FieldWrite fieldWrite)  = [ dataflowFieldWriteEdge fieldWrite ]
dataflowEdges' (Bitcode.UnresolvedRef unrslv)   = [ dataflowUnresolvedRefEdge unrslv ]
dataflowEdges' (Bitcode.SubscriptRead sbRead)   = [ dataflowSubscriptReadEdge sbRead ]
dataflowEdges' (Bitcode.SubscriptWrite sbWrite) = [ dataflowSubscriptWriteEdge sbWrite ]
dataflowEdges' _ = []

dataflowCallEdges :: Bitcode.CallContent -> [ Edge ]
dataflowCallEdges c = let
    everyArgToOutput = dataflowCallArgsEdges (Bitcode.callOutput c) (Bitcode.args c)
    calleeToOutput = edgify (Bitcode.callOutput c) (Bitcode.callee c)
    in calleeToOutput : everyArgToOutput 

dataflowCallArgsEdges :: Bitcode.Variable -> [ Bitcode.Variable ] -> [ Edge ]
dataflowCallArgsEdges output = Data.List.map $ edgify output

edgify :: Bitcode.Variable -> Bitcode.Variable -> Edge
edgify output arg = Edge { from = arg, to = output }

dataflowBinopEdge :: Bitcode.BinopContent -> [ Edge ]
dataflowBinopEdge b = let
    e1 = Edge { from = Bitcode.binopLhs b, to = Bitcode.binopOutput b }
    e2 = Edge { from = Bitcode.binopRhs b, to = Bitcode.binopOutput b }
    in [ e1, e2 ]

dataflowAssignEdge :: Bitcode.AssignContent -> Edge
dataflowAssignEdge a = Edge { from = Bitcode.assignInput a, to = Bitcode.assignOutput a }

dataflowUnresolvedRefEdge :: Bitcode.UnresolvedRefContent -> Edge
dataflowUnresolvedRefEdge c = Edge { from = Bitcode.unresolvedRef c, to = Bitcode.unresolvedRefOutput c } 

dataflowSubscriptWriteEdge :: Bitcode.SubscriptWriteContent -> Edge
dataflowSubscriptWriteEdge r = Edge { from = Bitcode.subscriptWriteInput r, to = Bitcode.subscriptWriteOutput r }

dataflowSubscriptReadEdge :: Bitcode.SubscriptReadContent -> Edge
dataflowSubscriptReadEdge r = Edge { from = Bitcode.subscriptReadInput r, to = Bitcode.subscriptReadOutput r } 

dataflowFieldReadEdge :: Bitcode.FieldReadContent -> Edge
dataflowFieldReadEdge f = Edge { from = Bitcode.fieldReadInput f, to = Bitcode.fieldReadOutput f } 

dataflowFieldWriteEdge :: Bitcode.FieldWriteContent -> Edge
dataflowFieldWriteEdge f = Edge { from = Bitcode.fieldWriteInput f, to = Bitcode.fieldWriteOutput f } 

extractLambdaParams :: Callable.LambdaContent -> [ Param ]
extractLambdaParams lambda = extractParams (Callable.lambdaLocation lambda) (Callable.lambdaBody lambda)

extractFunctionParams :: Callable.FunctionContent -> [ Param ]
extractFunctionParams f = extractParams (Callable.funcLocation f) (Callable.funcBody f)

extractMethodParams :: Callable.MethodContent -> [ Param ]
extractMethodParams m = extractParams (Callable.methodLocation m) (Callable.methodBody m)

extractParams :: Location -> Cfg -> [ Param ]
extractParams location cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    paramDecls = catMaybes (Data.Set.toList (Data.Set.map keepParamDecls instructions))
    in (toParams location paramDecls) 

toParams :: Location -> [ Bitcode.ParamDeclContent ] -> [ Param ]
toParams location = Data.List.map (toParam location)

toParam :: Location -> Bitcode.ParamDeclContent -> Param
toParam loc (Bitcode.ParamDeclContent p) = let
    i = Bitcode.paramVariableSerialIdx p
    token = Bitcode.paramVariableToken p
    fqn = Bitcode.paramVariableFqn p
    in Param i token fqn loc

keepSubscriptReads :: Bitcode.InstructionContent -> Maybe Bitcode.SubscriptReadContent
keepSubscriptReads (Bitcode.SubscriptRead subscriptRead) = Just subscriptRead
keepSubscriptReads _ = Nothing

keepParamDecls :: Bitcode.InstructionContent -> Maybe Bitcode.ParamDeclContent
keepParamDecls (Bitcode.ParamDecl paramDecl) = Just paramDecl
keepParamDecls _ = Nothing

removeNondetCalls :: Bitcode.Variable -> Bitcode.CallContent -> Maybe Bitcode.CallContent
removeNondetCalls (Bitcode.SrcVariableCtor (Bitcode.SrcVariable _ (Token.VarName (Token.Named name _)))) call = case name of {
    "nondet" -> Nothing;
    _ -> Just call
}
removeNondetCalls _ call = Just call

keepCalls :: Bitcode.InstructionContent -> Maybe Bitcode.CallContent
keepCalls (Bitcode.Call call) = removeNondetCalls (Bitcode.callee call) call
keepCalls _ = Nothing

keepStrings :: Bitcode.InstructionContent -> Maybe Token.ConstStr
keepStrings (Bitcode.LoadImmStr li) = Just (Bitcode.loadImmStrValue li)
keepStrings _ = Nothing

keepReturns :: Bitcode.InstructionContent -> Maybe Bitcode.Variable
keepReturns (Bitcode.Return (Bitcode.ReturnContent (Just v))) = Just v
keepReturns _ = Nothing

combine :: [ Fqn ] -> [ Location ] -> [ Call ]
combine [] _ = []
combine _ [] = []
combine (fqn:fqns) (loc:locs) = (Call fqn loc Nothing Nothing) : (combine fqns locs)

combine' :: Location -> [ Fqn ] -> [ Location ] -> (Maybe Token.ClassName) -> [ Call ]
combine' _ [] _ _ = []
combine' _ _ [] _ = []
combine' l (fqn:fqns) (loc:locs) c = (Call fqn loc (Just l) c) : (combine' l fqns locs c)

kbGenScriptCalls :: Callable.ScriptContent -> [ Call ]
kbGenScriptCalls = kbGenCalls . Callable.scriptBody

kbGenCallsFromMethods :: Cfg -> Location -> (Maybe Token.ClassName) -> [ Call ]
kbGenCallsFromMethods cfg loc className = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    calls = catMaybes (Data.Set.toList (Data.Set.map keepCalls instructions))
    callees = Data.List.map Bitcode.callee calls
    fqns = Data.List.map Bitcode.variableFqn callees
    locations = Data.List.map Bitcode.callLocation calls
    in combine' loc fqns locations className

kbGenReturns :: Cfg -> [ Bitcode.Variable ]
kbGenReturns cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    in catMaybes (Data.Set.toList (Data.Set.map keepReturns instructions))

kbGenStrings :: Cfg -> [ Token.ConstStr ]
kbGenStrings cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    in catMaybes (Data.Set.toList (Data.Set.map keepStrings instructions))

kbGenCalls :: Cfg -> [ Call ]
kbGenCalls cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    calls = catMaybes (Data.Set.toList (Data.Set.map keepCalls instructions))
    callees = Data.List.map Bitcode.callee calls
    fqns = Data.List.map Bitcode.variableFqn callees
    locations = Data.List.map Bitcode.callLocation calls
    in combine fqns locations

kbGenScriptArgs :: Callable.ScriptContent -> [ Arg ]
kbGenScriptArgs = kbGenArgs . Callable.scriptBody 

kbGenArgs :: Cfg -> [ Arg ]
kbGenArgs cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes) 
    calls = catMaybes (Data.Set.toList (Data.Set.map keepCalls instructions))
    in kbGenScriptArgsFromCalls calls 

kbGenVars :: Cfg -> [ Bitcode.Variable ]
kbGenVars cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map (Bitcode.instructionContent . Cfg.theInstructionInside) nodes
    variables_sets = Data.Set.map Bitcode.variables instructions
    variables_set = Data.Set.foldl' Data.Set.union Data.Set.empty variables_sets
    in Data.Set.toList variables_set

kbGenScriptArgsFromCalls :: [ Bitcode.CallContent ] -> [ Arg ]
kbGenScriptArgsFromCalls calls = Data.List.foldl' (++) [] (kbGenScriptArgsFromCalls' calls)

kbGenScriptArgsFromCalls' :: [ Bitcode.CallContent ] -> [[ Arg ]]
kbGenScriptArgsFromCalls' = Data.List.map kbGenScriptArgsFromCall

kbGenScriptArgsFromCall :: Bitcode.CallContent -> [ Arg ]
kbGenScriptArgsFromCall call = kbGenScriptArgsFromCall' 0 (Bitcode.callLocation call) (Bitcode.args call)

kbGenScriptArgsFromCall' :: Word -> Location -> [ Bitcode.Variable ] -> [ Arg ]
kbGenScriptArgsFromCall' _ location [] = []
kbGenScriptArgsFromCall' i location (arg:args) = let
    a = Arg (Bitcode.variableFqn arg) i (Bitcode.locationVariable arg) location
    in a : (kbGenScriptArgsFromCall' (i+1) location args)

