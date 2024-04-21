{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Werror=missing-fields #-}

module KbGen

where

-- project imports
import Fqn
import Callable
import Location

-- general imports
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
kbGen :: Callable -> KnowledgeBase
kbGen (Callable.Script script) = kbGenScript script
kbGen (Callable.Lambda lambda) = emptyKnowledgeBase -- kbGenLambda lambda
kbGen (Callable.Method method) = emptyKnowledgeBase
kbGen (Callable.Function func) = emptyKnowledgeBase

kbGenScript :: Callable.ScriptContent -> KnowledgeBase
kbGenScript script = let
    calls = kbGenScriptCalls script
    args' = kbGenScriptArgs script
    in KnowledgeBase calls args'

kbGenScriptCalls :: Callable.ScriptContent -> [ Call ]
kbGenScriptCalls script = []

kbGenScriptArgs :: Callable.ScriptContent -> [ Arg ]
kbGenScriptArgs script = []
