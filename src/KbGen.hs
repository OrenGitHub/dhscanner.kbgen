{-# OPTIONS -Werror=missing-fields #-}

module KbGen

where

import Fqn
import Location

data KnowledgeBase
   = KnowledgeBase
     {
          calls :: [ Call ],
          args :: [ Arg ]
     }

data Call
   = Call
     {
         calleeFqn :: Fqn,
         callLocation :: Location
     }
     deriving ( Show )

data Arg
   = Arg
     {
         argFqn :: Fqn,
         argSerialIdx :: Word,
         argLocation :: Location
     }
     deriving ( Show )

emptyKnowledgeBase :: KnowledgeBase
emptyKnowledgeBase = KnowledgeBase [] []

-- | API: generate a prolog knowledge base from
-- a collection of callables
kbGen :: Callable -> KnowledgeBase
kbGen (Callable.Script script) = kbGenScript script
kbGen (Callable.Lambda lambda) = kbGenLambda lambda
kbGen (Callable.Method method) = emptyKnowledgeBase
kbGen (Callable.Function func) = emptyKnowledgeBase

kbGenScript :: Callable.ScriptContent -> KnowledgeBase
kbGenScript script = let
    calls = kbGenScriptCalls script
    args' = kbGenScriptArgs script
    in KnowledgeBase calls [] args' [] []

kbGenScriptCalls :: Callable.ScriptContent -> [ Call ]
kbGenScriptCalls script = []

kbGenScriptArgs :: Callable.ScriptContent -> [ Arg ]
kbGenScriptArgs script = []
