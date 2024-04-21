{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Werror=missing-fields #-}

module KbGen

where

-- project imports
import Fqn
import Cfg
import Callable
import Location

-- project qualified imports
import qualified Bitcode

-- general imports
import Data.Set
import Data.List
import Data.Maybe
import Data.Aeson
import GHC.Generics

data KnowledgeBase
   = KnowledgeBase
     {
          calls :: [ Call ],
          args :: [ Arg ]
     }
     deriving ( Show, Generic, ToJSON )

data Call
   = Call
     {
         calleeFqn :: Fqn,
         callLocation :: Location
     }
     deriving ( Show, Generic, ToJSON )

data Arg
   = Arg
     {
         argFqn :: Fqn,
         argSerialIdx :: Word,
         argLocation :: Location
     }
     deriving ( Show, Generic, ToJSON )

emptyKnowledgeBase :: KnowledgeBase
emptyKnowledgeBase = KnowledgeBase [] []

-- | API: generate a prolog knowledge base from
-- a collection of callables
kbGen :: Callables -> KnowledgeBase
kbGen = kbGen' . actualCallables

kbGen' :: [ Callable ] -> KnowledgeBase
kbGen' [] = emptyKnowledgeBase
kbGen' (c:cs) = let
    kb = kbGen'' c
    rest = kbGen' cs
    calls' = (calls kb) ++ (calls rest)
    args' = (args kb) ++ (args rest)
    in KnowledgeBase { calls = calls', args = args' }

kbGen'' :: Callable -> KnowledgeBase
kbGen'' (Callable.Script script) = kbGenScript script
kbGen'' (Callable.Lambda lambda) = emptyKnowledgeBase -- kbGenLambda lambda
kbGen'' (Callable.Method method) = emptyKnowledgeBase
kbGen'' (Callable.Function func) = emptyKnowledgeBase

kbGenScript :: Callable.ScriptContent -> KnowledgeBase
kbGenScript script = let
    calls = kbGenScriptCalls script
    args' = kbGenScriptArgs script
    in KnowledgeBase calls args'

keepCalls :: Bitcode.InstructionContent -> Maybe Bitcode.CallContent
keepCalls (Bitcode.Call call) = Just call 
keepCalls _ = Nothing

combine :: [ Fqn ] -> [ Location ] -> [ Call ]
combine [] _ = []
combine _ [] = []
combine (fqn:fqns) (loc:locs) = (Call fqn loc) : (combine fqns locs)

kbGenScriptCalls :: Callable.ScriptContent -> [ Call ]
kbGenScriptCalls script = let
    nodes = Cfg.actualNodes $ Cfg.nodes (scriptBody script)
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    calls = catMaybes (Data.Set.toList (Data.Set.map keepCalls instructions))
    callees = Data.List.map Bitcode.callee calls
    fqns = Data.List.map Bitcode.variableFqn callees
    locations = Data.List.map Bitcode.callLocation calls
    in combine fqns locations

kbGenScriptArgs :: Callable.ScriptContent -> [ Arg ]
kbGenScriptArgs script = []
