-- |
--
-- * The [knowledge base](https://en.wikipedia.org/wiki/Knowledge_representation_and_reasoning) ( kb )
--   aims to be a data structure able to:
--
--     * represent /multiple/ facts about the source code repo
--     * from /various/ programming languages
--     * each fact can be translated to a [Prolog](https://en.wikipedia.org/wiki/Prolog) fact
--     * facts can be combined to create [predicates](https://en.wikipedia.org/wiki/Predicate_%28logic%29)
--     * [predicates](https://en.wikipedia.org/wiki/Predicate_%28logic%29) can be combined to formulate /security queries/
--
-- * Facts describe relations between:
--
--     * code locations
--     * const strings
--     * const integers
--
-- * Code locations are capable of representing /all/ program entities
--
--     * classes
--     * methods
--     * lambdas
--     * annotations
--     * arguments
--     * parameters
--     * etc.
--
-- * Facts can be /combined/:
--
--     * [conjunction](https://en.wikipedia.org/wiki/Logical_conjunction)
--     * [disjunction](https://en.wikipedia.org/wiki/Logical_disjunction)
--     * [negation](https://en.wikipedia.org/wiki/Negation)
--
-- * [Prolog](https://en.wikipedia.org/wiki/Prolog) queries are /easy/ to write:
--
--     * you /don't/ have to be a Prolog expert
--     * copy-paste the basic facts to /any/ [LLM](https://en.wikipedia.org/wiki/Large_language_model)
--     * explain in plain English your query's purpose
--     * et voilà !
--
-- * Its main purpose is to serve as the:
--
--     * penultimate step for /static code analysis/ 
--     * part of the [dhscanner](https://github.com/OrenGitHub/dhscanner.vps) framework
--
--         * [SAST](https://en.wikipedia.org/wiki/Static_application_security_testing) for security scans 🔒
--         * [PII](https://en.wikipedia.org/wiki/Personal_data) leaks detection 🪪
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Kbgen (
    KeywordArgForCall(..),
    ParamResolvedType(..),
    ClassDef(..),
    FuncDef(..),
    Call1stPartyFuncDefinedInDir(..),
    AssignValueToToplevelVarName(..),
    Call1stPartyFuncDefinedInFile(..),
    ParamName(..),
    CallMethodOfClass(..),
    ConstString(..),
    ConstInteger(..),
    MethodOfClass(..),
    ClassHas1stPartySuper(..),
    ClassHas3rdPartySuper(..),
    CallMethodOfUntypedNamedParam(..),
    ArgiForCall(..),
    DataflowEdge(..),
    ConstBoolTrue(..),
    ConstNull(..),
    GatedReturn(..),
    Comparison(..),
    CallableReturnsValue(..),
    CallableReturnsWithoutValue(..),
    Cond(..),
    Lhs(..),
    Rhs(..),
    ComparisonOp(..),
    ReturnedValue(..),
    ClassAnnotation,
    CallableAnnotation,
    ParamiOfCallable(..),
    prologify,
    To(..),
    Arg(..),
    From(..),
    Func(..),
    Call(..),
    Class(..),
    Param(..),
    Method(..),
    Callable(..),
    Keyword(..),
    Resolved(..),
    ArgIndex(..),
    ParamIndex(..),
    FuncName(..),
    MethodName(..),
    ResolvedType(..),
    CallResolved(..),
    CalledFrom(..),
    AssignedValue(..),
    ResolvedSuper(..),
    FuncDefinedInDir(..),
    FuncDefinedInFile(..),
    ClassDefinedInFile(..),
    SuperDefinedInFile(..),
    SuperQualifiedName(..),
    ConstStr(..),
    ConstInt(..),
    Fact(..),
    locationify,
    restoreloc
)

where

-- general imports
import Data.Aeson
import Text.Printf
import GHC.Generics (Generic)
import System.FilePath (takeDirectory)
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly, string, takeText)
import Data.Text (Text)
import qualified Data.Text as T

-- general qualified imports
import qualified Data.List as List

-- project imports
import Location
import qualified Token
import qualified Fqn

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_keyword_arg_for_call( Keyword, Arg, Call ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-52803](https://nvd.nist.gov/vuln/detail/CVE-2024-52803) )
--
-- Code snippet ( Python ):
--
-- @
-- Popen(f'llamafactory-cli train {save_cmd(args)}', env=env, shell=True)
-- @
--
-- See complete source example: [here](https://github.com/hiyouga/LLaMA-Factory/commit/b3aa80d54a67da45e9e237e349486fb9c162b2ac)
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-52803](https://nvd.nist.gov/vuln/detail/CVE-2024-52803) )
--
-- @
-- subprocess_Popen_called_with_shell_eq_True( Call ) :-
--     kb_keyword_arg_for_call( \'shell\', TrueValue, Call ),
--     kb_call_resolved( Call, \'subprocess.Popen\' ),
--     kb_const_bool_true( TrueValue ).
-- @
--
-- Other facts combined in this example predicate:
--
--     * 'CallResolved'
--     * 'ConstBoolTrue'
--
data KeywordArgForCall = KeywordArgForCall
    Keyword -- ^
    Arg -- ^
    Call -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_param_has_resolved_type( Param, ResolvedType ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-49380](https://nvd.nist.gov/vuln/detail/CVE-2024-49380) )
--
-- Code snippet ( Golang ):
--
-- @
-- func postLocal(w http.ResponseWriter, r *http.Request) { ... }
-- @
--
-- See complete source example: [here](https://github.com/plentico/plenti/blob/01825e0dcd3505fac57adc2edf29f772d585c008/cmd/serve.go#L205)
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-49380](https://nvd.nist.gov/vuln/detail/CVE-2024-49380) )
--
-- @
-- looks_like_user_input( RequestParam ) :-
--     kb_param_has_resolved_type( RequestParam, \'net/http.Request\' ),
--     kb_param_has_resolved_type( ResponseParam, \'net/http.ResponseWriter\' ),
--     kb_param_i_of_callable( ResponseParam, 0, Handler ),
--     kb_param_i_of_callable( RequestParam, 1, Handler ).
-- @
--
-- Other facts combined in this predicate:
--
--     * 'ParamiOfCallable'
--
data ParamResolvedType = ParamResolvedType
    Param -- ^
    ResolvedType -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_class_def( Class, Name, DefinedInFile ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-53995](https://nvd.nist.gov/vuln/detail/CVE-2024-53995) )
--
-- @
-- # authentication.py
-- class LoginHandler(BaseHandler): ...
--
-- # index.py
-- from tornado.web import RequestHandler
-- class BaseHandler(RequestHandler): ...
-- @
--
-- See complete source example [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/authentication.py#L10),
-- [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/index.py#L35)
-- and [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/index.py#L15)
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-53995](https://nvd.nist.gov/vuln/detail/CVE-2024-53995) )
--
-- @
-- skip_level_subclass_of( Subclass, \'tornado.web.RequestHandler\' ) :-
--     kb_class_def( Subclass, _, _ ),
--     kb_class_has_1st_party_super( Subclass, ClassName, ClassDefinedInFile ),
--     kb_class_def( Class, ClassName, ClassDefinedInFile )
--     kb_class_has_3rd_party_super( Class, _, \'tornado.web.RequestHandler\' ).
-- @
--
-- Other facts combined in this predicate:
--
--     * 'ClassHas1stPartySuper'
--     * 'ClassHas3rdPartySuper'
--
data ClassDef = ClassDef
    Class -- ^
    Token.ClassName -- ^
    ClassDefinedInFile -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_func_def( Func, Name, DefinedInFile, DefinedInDir ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-29028](https://nvd.nist.gov/vuln/detail/CVE-2024-29028) )
--
-- @
-- # http_getter.go
-- g.GET("/get/httpmeta", func(c echo.Context) error {
--     urlStr := c.QueryParam("url")
--     ...
--     getter.GetHTMLMeta(urlStr)
--
-- # html_meta.go
-- func GetHTMLMeta(urlStr string) (*HTMLMeta, error) {
--     ...
--     http.Get(urlStr)
-- @
--
-- See complete source example [here](https://github.com/usememos/memos/blob/v0.14.0/api/v1/http_getter.go#L23),
-- and [here](https://github.com/usememos/memos/blob/v0.14.0/plugin/http-getter/html_meta.go#L24)
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-29028](https://nvd.nist.gov/vuln/detail/CVE-2024-29028) )
--
-- @
-- suspected_ssrf_sink( Func ) :-
--     kb_resolved_call( Call, \'net/http.Get\' ),
--     kb_arg_i_for_call( Arg, 0, Call ),
--     utils_dataflow_path( Param, Arg ),
--     kb_param_has_resolved_type( Param, \'string\' ),
--     kb_param_i_of_callable( Param, _, Func ),
--     kb_func_def( Func, Name, _, FuncDefinedInDir ),
--     kb_call_1st_party_func_defined_in_dir( _, Name, FuncDefinedInDir ).
-- @
--
-- Other facts combined in this predicate:
--
--     * 'CallResolved'
--     * 'ArgiForCall'
--     * 'ParamResolvedType'
--     * 'ParamiOfCallable'
--     * 'Call1stPartyFuncDefinedInDir'
--
data FuncDef = FuncDef
    Func -- ^
    Token.FuncName -- ^
    FuncDefinedInFile -- ^
    FuncDefinedInDir -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_call_1st_party_func_defined_in_dir( Call, Name, DefinedInDir ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-29028](https://nvd.nist.gov/vuln/detail/CVE-2024-29028) )
--
-- @
-- # http_getter.go
-- g.GET("/get/httpmeta", func(c echo.Context) error {
--     urlStr := c.QueryParam("url")
--     ...
--     getter.GetHTMLMeta(urlStr)
--
-- # html_meta.go
-- func GetHTMLMeta(urlStr string) (*HTMLMeta, error) {
--     ...
--     http.Get(urlStr)
-- @
--
-- See complete source example [here](https://github.com/usememos/memos/blob/v0.14.0/api/v1/http_getter.go#L23),
-- and [here](https://github.com/usememos/memos/blob/v0.14.0/plugin/http-getter/html_meta.go#L24)
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-29028](https://nvd.nist.gov/vuln/detail/CVE-2024-29028) )
--
-- @
-- suspected_ssrf_sink( Func ) :-
--     kb_resolved_call( Call, \'net/http.Get\' ),
--     kb_arg_i_for_call( Arg, 0, Call ),
--     utils_dataflow_path( Param, Arg ),
--     kb_param_has_resolved_type( Param, \'string\' ),
--     kb_param_i_of_callable( Param, _, Func ),
--     kb_func_def( Func, Name, _, FuncDefinedInDir ),
--     kb_call_1st_party_func_from_dir( _, Name, FuncDefinedInDir ).
-- @
--
-- Other facts combined in this predicate:
--
--     * 'CallResolved'
--     * 'ArgiForCall'
--     * 'ParamResolvedType'
--     * 'ParamiOfCallable'
--
data Call1stPartyFuncDefinedInDir = Call1stPartyFuncDefinedInDir
    Call -- ^
    FuncName -- ^
    FuncDefinedInDir -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Call1stPartyFuncDefinedInFile = Call1stPartyFuncDefinedInFile
    Call -- ^
    FuncName -- ^
    FuncDefinedInFile -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data AssignValueToToplevelVarName = AssignValueToToplevelVarName
    AssignedValue -- ^
    Token.VarName -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Description:__
--
-- Input parameter of some callable ( method, function, lambda ) has the specified name
--
-- __Prolog signature:__
--
-- @
-- kb_param_has_name( Param, Name ).
-- @
--
-- __Code Example ( Javascript ):__
--
-- Motivated by [GHSL-2023-203](https://github.com/advplyr/audiobookshelf/security/advisories/GHSA-mgj7-rfx8-vhpr#event-116560)
-- ( [source code](https://github.com/advplyr/audiobookshelf/blob/d7b2476473ef1934eedec41425837cddf2d4b13e/server/controllers/AuthorController.js#L66)
-- and see also [here](https://github.com/advplyr/audiobookshelf/blob/d7b2476473ef1934eedec41425837cddf2d4b13e/server/controllers/AuthorController.js#L74)
-- and [here](https://github.com/advplyr/audiobookshelf/blob/d7b2476473ef1934eedec41425837cddf2d4b13e/server/routers/ApiRouter.js#L201) )
--
-- @
-- // ApiRouter.js
-- const express = require(\'express\')
--
-- class ApiRouter {
--   constructor(Server) { this.router = express() }
--   init() { this.router.patch(\'\/authors\/:id', ... , AuthorController.update.bind(this)) }
--
-- // AuthorController.js
-- class AuthorController { async update(req, res) { ... } }
-- @
--
-- __Writing new predicates:__
--
-- Facts combined in this predicate:
--
--     * 'ParamName'
--     * 'ParamiOfCallable'
--
-- @
-- http_request_param_of_method( Param, Method ) :-
--     kb_param_name( Param, \'req\' ),
--     kb_param_name( SecondParam, \'res\' ),
--     kb_param_i_of_callable( Param, 0, Method ),
--     kb_param_i_of_callable( SecondParam, 1, Method ).
-- @
--
-- Facts combined in this predicate:
--
--     * 'ArgiForCall'
--     * 'ClassName'
--     * 'MethodName'
--
-- @
-- request_handler( Method ) :-
--     kb_resolved_call( Call, \'express.patch\' ),
--     kb_arg_i_for_call( RequestHandler, 2, Call ),
--     kb_resolved_part_i_call( RequestHandler, 0, ClassName ),
--     kb_resolved_part_i_call( RequestHandler, 1, MethodName ),
--     kb_method_of_class( Method, Class ),
--     kb_method_name( Method, MethodName ),
--     kb_class_name( Class, ClassName ).
-- @
--
--
data ParamName = ParamName
    Param -- ^
    Token.ParamName -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_class_has_1st_party_super( Class, SuperName, SuperDefinedInFile ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-53995](https://nvd.nist.gov/vuln/detail/CVE-2024-53995) )
--
-- @
-- # authentication.py
-- class LoginHandler(BaseHandler): ...
--
-- # index.py
-- from tornado.web import RequestHandler
-- class BaseHandler(RequestHandler): ...
-- @
--
-- See complete source example [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/authentication.py#L10),
-- [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/index.py#L35)
-- and [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/index.py#L15)
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-53995](https://nvd.nist.gov/vuln/detail/CVE-2024-53995) )
--
-- @
-- skip_level_subclass_of( Subclass, \'tornado.web.RequestHandler\' ) :-
--     kb_class_def( Subclass, _, _ ),
--     kb_class_has_1st_party_super( Subclass, ClassName, ClassDefinedInFile ),
--     kb_class_def( Class, ClassName, ClassDefinedInFile )
--     kb_class_has_3rd_party_super( Class, _, \'tornado.web.RequestHandler\' ).
-- @
--
-- Other facts combined in this predicate:
--
--     * 'ClassDef'
--     * 'ClassHas3rdPartySuper'
--
data ClassHas1stPartySuper = ClassHas1stPartySuper
    Class -- ^
    Token.SuperName -- ^
    SuperDefinedInFile -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_class_has_3rd_party_super( Class, SuperName, SuperQualifiedName ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-53995](https://nvd.nist.gov/vuln/detail/CVE-2024-53995) )
--
-- @
-- # authentication.py
-- class LoginHandler(BaseHandler): ...
--
-- # index.py
-- from tornado.web import RequestHandler
-- class BaseHandler(RequestHandler): ...
-- @
--
-- See complete source example [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/authentication.py#L10),
-- [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/index.py#L35)
-- and [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/index.py#L15)
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-53995](https://nvd.nist.gov/vuln/detail/CVE-2024-53995) )
--
-- @
-- skip_level_subclass_of( Subclass, \'tornado.web.RequestHandler\' ) :-
--     kb_class_def( Subclass, _, _ ),
--     kb_class_has_1st_party_super( Subclass, ClassName, ClassDefinedInFile ),
--     kb_class_def( Class, ClassName, ClassDefinedInFile )
--     kb_class_has_3rd_party_super( Class, _, \'tornado.web.RequestHandler\' ).
-- @
--
-- Other facts combined in this predicate:
--
--     * 'ClassDef'
--     * 'ClassHas1stPartySuper'
--
data ClassHas3rdPartySuper = ClassHas3rdPartySuper
    Class -- ^
    Token.SuperName -- ^
    SuperQualifiedName -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_call_method_of_class( Call, Method, Class ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-53995](https://nvd.nist.gov/vuln/detail/CVE-2024-53995) )
--
-- @
-- # index.py
-- from tornado.web import RequestHandler
-- class BaseHandler(RequestHandler): ...
--
-- # authentication.py
-- class LoginHandler(BaseHandler):
--     def post(self, ...):
--         n = self.get_query_argument("next", ...)
--         self.redirect(n or ...)
-- @
--
-- See complete source example [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/authentication.py#L10),
-- [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/index.py#L35)
-- and [here](https://github.com/SickChill/sickchill/blob/846adafdfab579281353ea08a27bbb813f9a9872/sickchill/views/index.py#L15)
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-53995](https://nvd.nist.gov/vuln/detail/CVE-2024-53995) )
--
-- @
-- user_controlled_query_argument( Call ) :-
--     kb_call_method_of_class( Call, \'get_query_argument\', Class ),
--     kb_class_has_3rd_party_super( Class, \'tornado.web.RequestHandler\'),
--     kb_arg_i_for_call( QueryParamName, 0, Call),
--     kb_const_string( QueryParamName, _ ).
-- @
--
-- Other facts combined in this predicate:
--
--     * 'ClassHas3rdPartySuper'
--     * 'ArgiForCall'
--     * 'ConstString'
--
data CallMethodOfClass = CallMethodOfClass
    Call -- ^
    MethodName -- ^
    Class -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_call_method_of_untyped_named_param( Param, ParamName, MethodName ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-47769](https://nvd.nist.gov/vuln/detail/CVE-2024-47769) )
--
-- @
-- // corePublicRouter.js
-- const express = require('express');
-- const router = express.Router();
--
-- router.route('...').get(function (req, res) {
--     const { ..., file } = req.params;
--     const fileName = file;
--     return res.sendFile(fileName, ...);
-- }
-- @
--
-- See complete source example [here](https://github.com/idurar/idurar-erp-crm/blob/d7b2215a17bb2b52acfdab8f1646685d13df9a00/backend/src/routes/coreRoutes/corePublicRouter.js#L8-L24),
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-47769](https://nvd.nist.gov/vuln/detail/CVE-2024-47769) )
--
-- @
-- arbitrary_file_read( Call ) :-
--     kb_call_resolved( Call, \'express.Router.route.get\'),
--     kb_call_method_of_untyped_named_param( Param, \'res\', \'sendFile\' ),
--     kb_arg_i_for_call( Lambda, 0, Call ),
--     kb_param_i_of_callable( Param, 1, Lambda ).
-- @
--
-- Other facts combined in this predicate:
--
--     * 'CallResolved'
--     * 'ArgiForCall'
--     * 'ParamiOfCallable'
--
data CallMethodOfUntypedNamedParam = CallMethodOfUntypedNamedParam
    Call -- ^
    MethodName -- ^
    Token.ParamName -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
-- capture inheritance from third party classes
data ClassAnnotation = ClassAnnotation
    Class -- ^
    Annotation -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data CallableAnnotation = CallableAnnotation
    Callable -- ^
    Annotation -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
-- capture usage of inherited third party methods
data MethodOfClass = MethodOfClass
    Method -- ^
    Class -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_arg_i_for_call( Arg, Index, Call ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-47769](https://nvd.nist.gov/vuln/detail/CVE-2024-47769) )
--
-- @
-- // corePublicRouter.js
-- const express = require('express');
-- const router = express.Router();
--
-- router.route('...').get(function (req, res) {
--     const { ..., file } = req.params;
--     const fileName = file;
--     return res.sendFile(fileName, ...);
-- }
-- @
--
-- See complete source example [here](https://github.com/idurar/idurar-erp-crm/blob/d7b2215a17bb2b52acfdab8f1646685d13df9a00/backend/src/routes/coreRoutes/corePublicRouter.js#L8-L24),
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-47769](https://nvd.nist.gov/vuln/detail/CVE-2024-47769) )
--
-- @
-- arbitrary_file_read( Call ) :-
--     kb_call_resolved( Call, \'express.Router.route.get\'),
--     kb_call_method_of_untyped_named_param( Param, \'res\', \'sendFile\' ),
--     kb_arg_i_for_call( Lambda, 0, Call ),
--     kb_param_i_of_callable( Param, 1, Lambda ).
-- @
--
-- Other facts combined in this predicate:
--
--     * 'CallResolved'
--     * 'CallMethodOfUntypedNamedParam'
--     * 'ParamiOfCallable'
--
-- ==== __Tip 💡__
--
-- When the exact index is irrelevant, use:
--
-- @
-- kb_arg_i_for_call( Arg, _, Call ).
-- @
--
data ArgiForCall = ArgiForCall
    Arg -- ^
    ArgIndex -- ^
    Call -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_param_i_of_callable( Param, Index, Callable ).
-- @
--
-- __When should I use this fact__ ( motivation: [CVE-2024-29028](https://nvd.nist.gov/vuln/detail/CVE-2024-29028) )
--
-- @
-- # http_getter.go
-- g.GET("/get/httpmeta", func(c echo.Context) error {
--     urlStr := c.QueryParam("url")
--     ...
--     getter.GetHTMLMeta(urlStr)
--
-- # html_meta.go
-- func GetHTMLMeta(urlStr string) (*HTMLMeta, error) {
--     ...
--     http.Get(urlStr)
-- @
--
-- See complete source example [here](https://github.com/usememos/memos/blob/v0.14.0/api/v1/http_getter.go#L23),
-- and [here](https://github.com/usememos/memos/blob/v0.14.0/plugin/http-getter/html_meta.go#L24)
--
-- __Writing a predicate with this fact and others__ ( motivation: [CVE-2024-29028](https://nvd.nist.gov/vuln/detail/CVE-2024-29028) )
--
-- @
-- suspected_ssrf_sink( Func ) :-
--     kb_resolved_call( Call, \'net/http.Get\' ),
--     kb_arg_i_for_call( Arg, 0, Call ),
--     utils_dataflow_path( Param, Arg ),
--     kb_param_has_resolved_type( Param, \'string\' ),
--     kb_param_i_of_callable( Param, _, Func ),
--     kb_func_def( Func, Name, _, FuncDefinedInDir ),
--     kb_call_1st_party_func_defined_in_dir( _, Name, FuncDefinedInDir ).
-- @
--
-- Other facts combined in this predicate:
--
--     * 'CallResolved'
--     * 'ArgiForCall'
--     * 'ParamResolvedType'
--     * 'FuncDef'
--     * 'Call1stPartyFuncDefinedInDir'
--
-- ==== __Tip 💡__
--
-- When the _exact index_ is _irrelevant_, use:
--
-- @
-- kb_param_i_of_callable( Param, _, Callable ).
-- @
--
data ParamiOfCallable = ParamiOfCallable
    Param -- ^
    ParamIndex -- ^ 0-based
    Callable -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data CallResolved = CallResolved
    Call -- ^
    Resolved -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_called_from( Call, Callable ).
-- @
--
-- __When should I use this fact__
--
-- Web-app request handlers routinely /extract fields/ from the incoming
-- request \- headers, cookies, query parameters, body \- and the /name/
-- of the field is often the security-relevant piece ( e.g. the constant
-- string \'x-api-key\' says "this callable does API-key authentication" ).
-- To reason about /which/ callable extracts /which/ field, you need to
-- know that a particular call site lives inside the body of a particular
-- callable.
--
-- No pre-existing fact tied a 'Call' back to its enclosing 'Callable' \-
-- 'ParamiOfCallable' does the analogous thing for /parameters/ only.
-- Because 'CalledFrom' exposes the 'Call' /location/ ( not just its FQN ),
-- it composes with 'ArgiForCall' and 'ConstString' to also surface the
-- /arguments/ passed at that specific call site.
--
-- Code snippet ( Typescript, Next.js \- formbricks at v3.16.0 ):
--
-- @
-- export const authenticateRequest = async (request: Request) => {
--     const apiKey = request.headers.get(\"x-api-key\");
--     if (!apiKey) return null;
--     const apiKeyData = await getApiKeyWithPermissions(apiKey);
--     ...
-- };
-- @
--
-- See complete source example [here](https://github.com/formbricks/formbricks/blob/v3.16.0/apps/web/app/api/v1/auth.ts#L7-L36)
--
-- __Writing a predicate with this fact and others__
--
-- Compose 'CalledFrom' with 'ArgiForCall' and 'ConstString' to enumerate
-- the constant-string header names each callable extracts from the
-- incoming request:
--
-- @
-- extracts_request_header_named( Callable, HeaderName ) :-
--     kb_called_from( Call, Callable ),
--     kb_call_resolved( Call, \'Request.headers.get\' ),
--     kb_arg_i_for_call( HeaderConst, 0, Call ),
--     kb_const_string( HeaderConst, HeaderName ).
-- @
--
-- Applied to @authenticateRequest@ above, this yields
-- @extracts_request_header_named( authenticateRequest, \'x-api-key\' )@ \-
-- one of the six structural indicators for recognizing Shape-A
-- ( identity-carrier ) request authenticators.
--
-- Other facts combined in this predicate:
--
--     * 'CallResolved'
--     * 'ArgiForCall'
--     * 'ConstString'
--
data CalledFrom = CalledFrom
    Call -- ^
    Callable -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )


-- |
--
-- Usage:
--
-- @
-- kb_const_str( Param, Index, Callable ).
-- @
--
-- ==== __Tip 💡__
--
-- sometimes the exact index is irrelevant, see 'Arg_for_Call'
--
data ConstString = ConstString
    ConstStr -- ^
    Token.ConstStr -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_const_int( Loc, Value ).
-- @
--
-- __When should I use this fact__
--
-- Any time a query needs to discriminate on the /value/ of a constant
-- integer literal appearing in source code, typically as an argument
-- to a function that carries semantic information in that integer.
-- The canonical example is the HTTP status code passed to a response
-- constructor:
--
-- @
-- return Response.json({ error: \"unauthorized\" }, { status: 401 });
-- @
--
-- Recognizing this shape ( a bad status code returned early from a
-- callable ) is used to identify authenticating functions.
--
-- __Writing a predicate with this fact and others__
--
-- @
-- utils_ts_response_json_with_status( Function, Code ) :-
--     kb_call_resolved( OuterCall, \'nodejs.Response.json\' ),
--     kb_arg_i_for_call( DictifyCall, 1, OuterCall ),
--     kb_arg_i_for_call( KvCall, _, DictifyCall ),
--     kb_arg_i_for_call( KeyLoc, 0, KvCall ),
--     kb_const_string( KeyLoc, \'status\' ),
--     kb_arg_i_for_call( ValueLoc, 1, KvCall ),
--     kb_const_int( ValueLoc, Code ),
--     kb_called_from( OuterCall, Function ).
-- @
--
-- Other facts combined in this example predicate:
--
--     * 'CallResolved'
--     * 'ArgiForCall'
--     * 'ConstString'
--     * 'CalledFrom'
--
data ConstInteger = ConstInteger
    ConstInt -- ^
    Token.ConstInt -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_const_null( Loc ).
-- @
--
-- __When should I use this fact__
--
-- Returning early from a function with a null value has many uses cases related to security.
-- ( Python @None@ and Ruby @nil@ use this predicate as well )
--
-- __Writing a predicate with this fact and others__
--
-- @
-- kb_gated_return_null( Cond ) :-
--     kb_gated_return( Cond, ReturnedValue ),
--     kb_const_null( ReturnedValue ).
-- @
--
-- Other facts combined in this example predicate:
--
--     * 'GatedReturn'
--
data ConstNull = ConstNull Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_gated_return( Cond, ReturnedValue ).
-- @
--
-- __When should I use this fact__
--
-- Authenticating functions reject invalid credentials with an early return
-- from a guarded @if@. This fact pinpoints that rejection gate.
--
-- __Writing a predicate with this fact and others__
--
-- @
-- utils_early_return_null_on_missing_request_header_value( Callable, KeyName ) :-
--     kb_called_from( Call, Callable ),
--     kb_call_resolved( Call, \'nodejs.Request.headers.get\' ),
--     kb_arg_i_for_call( KeyArg, 0, Call ),
--     kb_const_string( KeyArg, KeyName ),
--     utils_intra_dataflow_path( Call, Var, _ ),
--     kb_gated_return_null( Cond ),
--     utils_intra_dataflow_path( Var, Cond, _ ).
--
-- kb_gated_return_null( Cond ) :- kb_gated_return( Cond, RV ), kb_const_null( RV ).
-- @
--
-- Other facts combined in this example predicate:
--
--     * 'ConstNull'
--     * 'CallResolved'
--     * 'ArgiForCall'
--     * 'ConstString'
--     * 'CalledFrom'
--
data GatedReturn = GatedReturn
    Cond -- ^
    ReturnedValue -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_comparison( Cond, Lhs, Rhs, Op ).
-- @
--
-- __When should I use this fact__
--
-- Binary comparison instructions ( currently @==@ \/ @===@ mapped to
-- the Prolog atom @eq@, and @!=@ \/ @!==@ mapped to @neq@ ) fire this
-- fact. The @Cond@ slot is the location of the comparison\'s output
-- variable, which makes composing with 'GatedReturn' straightforward:
-- when the boolean condition of an @if@ that early-returns is itself a
-- comparison, both facts share the same location in the @Cond@ slot.
--
-- __Writing a predicate with this fact and others__
--
-- @
-- gated_return_on_comparison( Lhs, Rhs, Op, ReturnedValue ) :-
--     kb_gated_return( Cond, ReturnedValue ),
--     kb_comparison( Cond, Lhs, Rhs, Op ).
-- @
--
-- Other facts combined in this example predicate:
--
--     * 'GatedReturn'
--
data Comparison = Comparison
    Cond -- ^
    Lhs -- ^
    Rhs -- ^
    ComparisonOp -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_callable_returns_value( Callable, ReturnedValue ).
-- @
--
-- __When should I use this fact__
--
-- Every explicit `return exp;` inside a callable's body fires this fact
-- once, keyed by the enclosing callable and the returned expression's
-- location. Downstream predicates like `kb_const_null` or
-- `kb_call_resolved` add semantic meaning to the value slot.
--
data CallableReturnsValue = CallableReturnsValue
    Callable -- ^
    ReturnedValue -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_callable_returns_without_value( Callable, ReturnStmtLocation ).
-- @
--
-- __When should I use this fact__
--
-- User-written bare `return;` (no value expression) fires this fact once,
-- keyed by the enclosing callable and the location of the return
-- statement itself. Distinct fact from 'CallableReturnsValue' because
-- there is no value-expression location to anchor on; the return
-- statement location is used instead.
--
data CallableReturnsWithoutValue = CallableReturnsWithoutValue
    Callable -- ^
    Location -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- __Name__
--
-- This is how the fact will look inside the Prolog file
--
-- @
-- kb_dataflow_edge( From, To ).
-- @
--
data DataflowEdge = DataflowEdge
    From -- ^
    To -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Arg = Arg Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Call = Call Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Param = Param Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Class = Class Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data To = To Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data From = From Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Func = Func Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Method = Method Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Callable = Callable Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data ConstStr = ConstStr Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data ConstInt = ConstInt Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Annotation = Annotation Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data AssignedValue = AssignedValue Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data ConstBoolTrue = ConstBoolTrue Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Cond = Cond Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Lhs = Lhs Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Rhs = Rhs Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data ComparisonOp = ComparisonOp String deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data ReturnedValue = ReturnedValue Location deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Keyword = Keyword String deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data Resolved = Resolved Fqn.Fqn deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data ArgIndex = ArgIndex Word deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data ParamIndex = ParamIndex Word deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data FuncName = FuncName String deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data MethodName = MethodName String deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data ConstStrValue = ConstStrValue String deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data SuperQualifiedName = SuperQualifiedName Fqn.Fqn deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data FuncDefinedInDir = FuncDefinedInDir FilePath deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data FuncDefinedInFile = FuncDefinedInFile FilePath deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data ClassDefinedInFile = ClassDefinedInFile FilePath deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
data SuperDefinedInFile = SuperDefinedInFile FilePath deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
data ResolvedType = ResolvedType
    Fqn.Fqn -- ^
    deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ResolvedSuper = ResolvedSuper Fqn.Fqn deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
-- * each fact is derived from a /single/ bitcode 'Bitcode.Instruction'
--
-- * often, a single bitcode instruction will yield /more/ than one fact
--
data Fact
   = FuncDefCtor FuncDef 
   | ClassDefCtor ClassDef
   | ParamNameCtor ParamName
   | ArgiForCallCtor ArgiForCall
   | ConstStringCtor ConstString
   | ConstIntegerCtor ConstInteger
   | DataflowEdgeCtor DataflowEdge
   | CallResolvedCtor CallResolved
   | CalledFromCtor CalledFrom
   | ConstBoolTrueCtor ConstBoolTrue
   | ConstNullCtor ConstNull
   | GatedReturnCtor GatedReturn
   | ComparisonCtor Comparison
   | CallableReturnsValueCtor CallableReturnsValue
   | CallableReturnsWithoutValueCtor CallableReturnsWithoutValue
   | MethodOfClassCtor MethodOfClass
   | ClassAnnotationCtor ClassAnnotation
   | ParamiOfCallableCtor ParamiOfCallable
   | KeywordArgForCallCtor KeywordArgForCall
   | ParamResolvedTypeCtor ParamResolvedType
   | CallMethodOfClassCtor CallMethodOfClass
   | CallableAnnotationCtor CallableAnnotation
   | ClassHas1stPartySuperCtor ClassHas1stPartySuper
   | ClassHas3rdPartySuperCtor ClassHas3rdPartySuper
   | AssignValueToToplevelVarNameCtor AssignValueToToplevelVarName
   | Call1stPartyFuncDefinedInDirCtor Call1stPartyFuncDefinedInDir
   | Call1stPartyFuncDefinedInFileCtor Call1stPartyFuncDefinedInFile
   | CallMethodOfUntypedNamedParamCtor CallMethodOfUntypedNamedParam
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
-- Translate a code fact into a prolog fact
--
-- 'ParamName'
--
-- @
-- kb_param_has_name( Param, Name ).
-- @
--
prologify :: Fact -> String
prologify (FuncDefCtor content) = prologify_FuncDef content
prologify (ClassDefCtor content) = prologify_ClassDef content
prologify (ParamNameCtor content) = prologify_ParamName content
prologify (ArgiForCallCtor content) = prologifyArgiForCall content
prologify (ConstStringCtor content) = prologify_ConstString content
prologify (ConstIntegerCtor content) = prologify_ConstInteger content
prologify (DataflowEdgeCtor content) = prologify_DataflowEdge content
prologify (CallResolvedCtor content) = prologify_CallResolved content
prologify (CalledFromCtor content) = prologify_CalledFrom content
prologify (ConstBoolTrueCtor content) = prologify_ConstBoolTrue content
prologify (ConstNullCtor content) = prologify_ConstNull content
prologify (GatedReturnCtor content) = prologify_GatedReturn content
prologify (ComparisonCtor content) = prologify_Comparison content
prologify (CallableReturnsValueCtor content) = prologify_CallableReturnsValue content
prologify (CallableReturnsWithoutValueCtor content) = prologify_CallableReturnsWithoutValue content
prologify (MethodOfClassCtor content) = prologify_MethodOfClass content
prologify (ClassAnnotationCtor content) = prologify_ClassAnnotation content
prologify (ParamiOfCallableCtor content) = prologify_ParamiOfCallable content
prologify (ParamResolvedTypeCtor content) = prologify_ParamResolvedType content
prologify (CallMethodOfClassCtor content) = prologify_CallMethodOfClass content
prologify (KeywordArgForCallCtor content) = prologify_KeywordArgForCall content
prologify (CallableAnnotationCtor content) = prologify_CallableAnnotation content
prologify (ClassHas1stPartySuperCtor content) = prologify_ClassHas1stPartySuper content
prologify (ClassHas3rdPartySuperCtor content) = prologify_ClassHas3rdPartySuper content
prologify (AssignValueToToplevelVarNameCtor content) = prologify_AssignValueToToplevelVarName content
prologify (Call1stPartyFuncDefinedInDirCtor content) = prologify_Call1stPartyFuncDefinedInDir content
prologify (Call1stPartyFuncDefinedInFileCtor content) = prologify_Call1stPartyFuncDefinedInFile content
prologify (CallMethodOfUntypedNamedParamCtor content) = prologify_CallMethodOfUntypedNamedParam content

prologify_ParamResolvedType' :: Location -> String -> String
prologify_ParamResolvedType' l fqn = printf "kb_param_has_resolved_type( %s, \'%s\' )." (locationify l) fqn

prologify_ParamResolvedType :: ParamResolvedType -> String
prologify_ParamResolvedType (ParamResolvedType (Param loc) (ResolvedType fqn)) = prologify_ParamResolvedType' loc (prologifyFqn fqn)

prologify_CallMethodOfClass' :: Location -> String -> Location -> String
prologify_CallMethodOfClass' call method klass = printf "kb_call_method_of_class( %s, \'%s\', %s )." (locationify call) method (locationify klass)

prologify_CallMethodOfClass :: CallMethodOfClass -> String
prologify_CallMethodOfClass (CallMethodOfClass (Call loc) (MethodName m) (Class c)) = prologify_CallMethodOfClass' loc m c

prologify_ParamName' :: Location -> String -> String
prologify_ParamName' l name = printf "kb_param_has_name( %s, \'%s\' )." (locationify l) name

prologify_ParamName :: ParamName -> String
prologify_ParamName (ParamName (Param loc) (Token.ParamName (Token.Named name _))) = prologify_ParamName' loc name

prologify_ClassDef' :: Location -> String -> FilePath -> String
prologify_ClassDef' l name f = printf "kb_class_def( %s, \'%s\', \'%s\' )." (locationify l) name f

prologify_ClassDef :: ClassDef -> String
prologify_ClassDef (ClassDef (Class loc) (Token.ClassName (Token.Named name _)) (ClassDefinedInFile f)) = prologify_ClassDef' loc name f

prologify_FuncDef' :: Location -> String -> FilePath -> FilePath -> String
prologify_FuncDef' l name f d = printf "kb_func_def( %s, \'%s\', \'%s\', \'%s\' )." (locationify l) name f d

prologify_FuncDef :: FuncDef -> String
prologify_FuncDef (FuncDef (Func loc) (Token.FuncName (Token.Named name _)) (FuncDefinedInFile f) (FuncDefinedInDir d)) = prologify_FuncDef' loc name f d

unquote :: String -> String
unquote = filter (/= '\'')

prologify_ConstString' :: Location -> String -> String
prologify_ConstString' l value = printf "kb_const_string( %s, \'%s\' )." (locationify l) (unquote value)

prologify_ConstString :: ConstString -> String
prologify_ConstString (ConstString (ConstStr loc) (Token.ConstStr value _)) = prologify_ConstString' loc value

prologify_ConstInteger' :: Location -> Int -> String
prologify_ConstInteger' l value = printf "kb_const_int( %s, %d )." (locationify l) value

prologify_ConstInteger :: ConstInteger -> String
prologify_ConstInteger (ConstInteger (ConstInt loc) (Token.ConstInt value _)) = prologify_ConstInteger' loc value

prologify_CallResolved'' :: Location -> String -> Token.ClassName -> String
prologify_CallResolved'' call m c = printf "kb_call_method_of_class( %s, %s, %s )." (locationify call) m (locationify (Token.getClassNameLocation c))

prologify_CallResolved' :: Location -> Fqn.Fqn -> String
prologify_CallResolved' call (Fqn.CallMethodOfClass _ m c) = prologify_CallResolved'' call m c 
prologify_CallResolved' call fqn = printf "kb_call_resolved( %s, \'%s\' )." (locationify call) (prologifyFqn fqn)

prologify_DataflowEdge :: DataflowEdge -> String
prologify_DataflowEdge (DataflowEdge (From u) (To v)) = prologify_DataflowEdge' u v

prologify_DataflowEdge' :: Location -> Location -> String
prologify_DataflowEdge' u v = printf "kb_dataflow_edge( %s, %s )." (locationify u) (locationify v)

prologify_CallResolved :: CallResolved -> String
prologify_CallResolved (CallResolved (Call call) (Resolved fqn)) = prologify_CallResolved' call fqn

prologify_ConstBoolTrue' :: Location -> String
prologify_ConstBoolTrue' trueValue = printf "kb_class_name( %s, \'%s\' )." (locationify trueValue)

prologify_ConstBoolTrue :: ConstBoolTrue -> String
prologify_ConstBoolTrue (ConstBoolTrue trueValue) = prologify_ConstBoolTrue' trueValue

prologify_ConstNull' :: Location -> String
prologify_ConstNull' l = printf "kb_const_null( %s )." (locationify l)

prologify_ConstNull :: ConstNull -> String
prologify_ConstNull (ConstNull loc) = prologify_ConstNull' loc

prologify_GatedReturn' :: Location -> Location -> String
prologify_GatedReturn' c v = printf "kb_gated_return( %s, %s )." (locationify c) (locationify v)

prologify_GatedReturn :: GatedReturn -> String
prologify_GatedReturn (GatedReturn (Cond c) (ReturnedValue v)) = prologify_GatedReturn' c v

prologify_Comparison' :: Location -> Location -> Location -> String -> String
prologify_Comparison' c l r op = printf "kb_comparison( %s, %s, %s, %s )." (locationify c) (locationify l) (locationify r) op

prologify_Comparison :: Comparison -> String
prologify_Comparison (Comparison (Cond c) (Lhs l) (Rhs r) (ComparisonOp op)) = prologify_Comparison' c l r op

prologify_CallableReturnsValue' :: Location -> Location -> String
prologify_CallableReturnsValue' c v = printf "kb_callable_returns_value( %s, %s )." (locationify c) (locationify v)

prologify_CallableReturnsValue :: CallableReturnsValue -> String
prologify_CallableReturnsValue (CallableReturnsValue (Callable c) (ReturnedValue v)) = prologify_CallableReturnsValue' c v

prologify_CallableReturnsWithoutValue' :: Location -> Location -> String
prologify_CallableReturnsWithoutValue' c s = printf "kb_callable_returns_without_value( %s, %s )." (locationify c) (locationify s)

prologify_CallableReturnsWithoutValue :: CallableReturnsWithoutValue -> String
prologify_CallableReturnsWithoutValue (CallableReturnsWithoutValue (Callable c) s) = prologify_CallableReturnsWithoutValue' c s

prologify_MethodOfClass' :: Location -> Location -> String
prologify_MethodOfClass' m c = printf "kb_method_of_class( %s, %s )." (locationify m) (locationify c)

prologify_MethodOfClass :: MethodOfClass -> String
prologify_MethodOfClass (MethodOfClass (Method m) (Class c)) = prologify_MethodOfClass' m c

prologify_ClassHas1stPartySuper' :: Location -> String -> FilePath -> String
prologify_ClassHas1stPartySuper' c s f = printf "kb_class_has_1st_party_super( %s, \'%s\', \'%s\' )." (locationify c) s f
 
prologify_ClassHas1stPartySuper :: ClassHas1stPartySuper -> String
prologify_ClassHas1stPartySuper (ClassHas1stPartySuper (Class c) (Token.SuperName (Token.Named s _)) (SuperDefinedInFile f)) = prologify_ClassHas1stPartySuper' c s f

prologify_ClassHas3rdPartySuper' :: Location -> String -> Fqn.Fqn -> String
prologify_ClassHas3rdPartySuper' c s f = printf "kb_class_has_3rd_party_super( %s, \'%s\', \'%s\' )." (locationify c) s (prologifyFqn f)
 
prologify_ClassHas3rdPartySuper :: ClassHas3rdPartySuper -> String
prologify_ClassHas3rdPartySuper (ClassHas3rdPartySuper (Class c) (Token.SuperName (Token.Named s _)) (SuperQualifiedName f)) = prologify_ClassHas3rdPartySuper' c s f

prologify_AssignValueToToplevelVarName :: AssignValueToToplevelVarName -> String
prologify_AssignValueToToplevelVarName (AssignValueToToplevelVarName value varname) = prologify_AssignValueToToplevelVarName' value varname

prologify_AssignValueToToplevelVarName' :: AssignedValue -> Token.VarName -> String
prologify_AssignValueToToplevelVarName' (AssignedValue value) (Token.VarName (Token.Named varname l)) = prologify_AssignValueToToplevelVarName'' value varname (Location.filename l)

prologify_AssignValueToToplevelVarName'' :: Location -> String -> FilePath -> String
prologify_AssignValueToToplevelVarName'' value varname f = printf "kb_assign_value_to_toplevel_varname( %s, \'%s\', \'%s\', \'%s\')." (locationify value) varname f (takeDirectory f)

prologify_Call1stPartyFuncDefinedInFile :: Call1stPartyFuncDefinedInFile -> String
prologify_Call1stPartyFuncDefinedInFile (Call1stPartyFuncDefinedInFile call func f) = prologify_Call1stPartyFuncDefinedInFile' call func f

prologify_Call1stPartyFuncDefinedInFile' :: Call -> FuncName -> FuncDefinedInFile -> String
prologify_Call1stPartyFuncDefinedInFile' (Call call) (FuncName func) (FuncDefinedInFile f) = prologify_Call1stPartyFuncDefinedInFile'' call func f

prologify_Call1stPartyFuncDefinedInFile'' :: Location -> String -> FilePath -> String
prologify_Call1stPartyFuncDefinedInFile'' call func f = printf "kb_call_1st_party_func_defined_in_file( %s, \'%s\', \'%s\' )." (locationify call) func f

prologify_Call1stPartyFuncDefinedInDir :: Call1stPartyFuncDefinedInDir -> String
prologify_Call1stPartyFuncDefinedInDir (Call1stPartyFuncDefinedInDir call func d) = prologify_Call1stPartyFuncDefinedInDir' call func d

prologify_Call1stPartyFuncDefinedInDir' :: Call -> FuncName -> FuncDefinedInDir -> String
prologify_Call1stPartyFuncDefinedInDir' (Call call) (FuncName f) (FuncDefinedInDir d) = prologify_Call1stPartyFuncDefinedInDir'' call f d

prologify_Call1stPartyFuncDefinedInDir'' :: Location -> String -> FilePath -> String
prologify_Call1stPartyFuncDefinedInDir'' call f d = printf "kb_call_1st_party_func_defined_in_dir( %s, \'%s\', \'%s\' )." (locationify call) f d

prologify_CallMethodOfUntypedNamedParam :: CallMethodOfUntypedNamedParam -> String
prologify_CallMethodOfUntypedNamedParam (CallMethodOfUntypedNamedParam call method p) = prologify_CallMethodOfUntypedNamedParam' call method p

prologify_CallMethodOfUntypedNamedParam' :: Call -> MethodName -> Token.ParamName -> String
prologify_CallMethodOfUntypedNamedParam' (Call call) (MethodName m) (Token.ParamName (Token.Named _ p)) = prologify_CallMethodOfUntypedNamedParam'' call m p

prologify_CallMethodOfUntypedNamedParam'' :: Location -> String -> Location -> String
prologify_CallMethodOfUntypedNamedParam'' call m p = printf "kb_call_method_of_untyped_named_param( %s, \'%s\', %s )." (locationify call) m (locationify p)

prologifyArgiForCall' :: Location -> Word -> Location -> String
prologifyArgiForCall' a i c = printf "kb_arg_i_for_call( %s, %u, %s )." (locationify a) i (locationify c)

prologifyArgiForCall :: ArgiForCall -> String
prologifyArgiForCall (ArgiForCall (Arg a) (ArgIndex i) (Call c)) = prologifyArgiForCall' a i c

prologify_ClassAnnotation' :: Location -> Location -> String
prologify_ClassAnnotation' c a = printf "kb_class_has_annotation( %s, %s )." (locationify c) (locationify a)

prologify_ClassAnnotation :: ClassAnnotation -> String
prologify_ClassAnnotation (ClassAnnotation (Class c) (Annotation a)) = prologify_ClassAnnotation' c a

prologify_KeywordArgForCall' ::  String -> Location -> Location -> String
prologify_KeywordArgForCall' kw a c = printf "kb_keyword_arg_for_call( %s, %s, %s )." (locationify a) kw (locationify c)

prologify_KeywordArgForCall :: KeywordArgForCall -> String
prologify_KeywordArgForCall (KeywordArgForCall (Keyword kw) (Arg a) (Call c)) = prologify_KeywordArgForCall' kw a c

prologify_CallableAnnotation' :: Location -> Location -> String
prologify_CallableAnnotation' c a = printf "kb_callable_has_annotation( %s, %s )." (locationify c) (locationify a)

prologify_CallableAnnotation :: CallableAnnotation -> String
prologify_CallableAnnotation (CallableAnnotation (Callable c) (Annotation a)) = prologify_CallableAnnotation' c a

prologify_ParamiOfCallable' :: Location -> Word -> Location -> String
prologify_ParamiOfCallable' p i c = printf "kb_param_i_of_callable( %s, %u, %s )." (locationify p) i (locationify c)

prologify_ParamiOfCallable :: ParamiOfCallable -> String
prologify_ParamiOfCallable (ParamiOfCallable (Param p) (ParamIndex i) (Callable c)) = prologify_ParamiOfCallable' p i c

prologify_CalledFrom' :: Location -> Location -> String
prologify_CalledFrom' call callable = printf "kb_called_from( %s, %s )." (locationify call) (locationify callable)

prologify_CalledFrom :: CalledFrom -> String
prologify_CalledFrom (CalledFrom (Call call) (Callable callable)) = prologify_CalledFrom' call callable

normalizeChar :: Char -> String
normalizeChar '/' = "_slash_"
normalizeChar '.' = "_dot_"
normalizeChar '-' = "_dash_"
normalizeChar '['  = "_lbrack_"
normalizeChar ']'  = "_rbrack_"
normalizeChar '('  = "_lparen_"
normalizeChar ')'  = "_rparen_"
normalizeChar c = [c]

normalize :: FilePath -> FilePath
normalize path = concatMap normalizeChar path

prologifyFqn :: Fqn.Fqn -> String
prologifyFqn (Fqn.ThirdPartyImport fqn) = prologifyThirdPartyImport fqn
prologifyFqn (Fqn.FieldedAccess fqn fieldName) = prologifyFieldedAccess fqn fieldName
prologifyFqn fqn = show fqn

prologifyThirdPartyImport :: Fqn.ThirdPartyImportContent -> String
prologifyThirdPartyImport (Fqn.ThirdPartyImportContent p [] Nothing _) = p
prologifyThirdPartyImport (Fqn.ThirdPartyImportContent p [] (Just name) _) = p ++ "." ++ name
prologifyThirdPartyImport (Fqn.ThirdPartyImportContent p rest Nothing _) = p ++ "." ++ List.intercalate "." rest
prologifyThirdPartyImport (Fqn.ThirdPartyImportContent p rest (Just name) _) = p ++ "." ++ List.intercalate "." rest ++ "." ++ name

prologifyFieldedAccess :: Fqn.Fqn -> Token.FieldName -> String
prologifyFieldedAccess fqn (Token.FieldName (Token.Named fieldName _)) = (prologifyFqn fqn) ++ "." ++ fieldName

locationify :: Location -> String
locationify l = let
    x = Location.lineStart l
    y = Location.colStart l
    z = Location.lineEnd l
    w = Location.colEnd l
    f = normalize (Location.filename l)
    in printf "startloc_%u_%u_endloc_%u_%u_%s" x y z w f

restoreloc :: String -> Maybe Location
restoreloc s = case parseOnly locationParser (T.pack s) of { Right l -> Just l; _ -> Nothing; }

locationParser :: Parser Location
locationParser = do {

    _ <- string "startloc_"; x <- decimal; _ <- char '_'; y <- decimal;
    _ <- string "_endloc_" ; z <- decimal; _ <- char '_'; w <- decimal;
    _ <- char '_'; fname <- takeText;

    pure Location {
        filename  = restoreFilename (T.unpack fname),
        lineStart = x,
        colStart = y,
        lineEnd = z,
        colEnd = w
    }
}

data Rule = Rule { from :: Text, to :: Text }

rules :: [ Rule ]
rules =
    [ Rule { from = "_slash_",  to = "/" }
    , Rule { from = "_dot_",    to = "." }
    , Rule { from = "_dash_",   to = "-" }
    , Rule { from = "_lbrack_", to = "[" }
    , Rule { from = "_rbrack_", to = "]" }
    , Rule { from = "_lparen_", to = "(" }
    , Rule { from = "_rparen_", to = ")" }
    ]

applyRule :: Rule -> Text -> Text
applyRule Rule { from = f, to = t } = T.replace f t

applyRuleStep :: Text -> Rule -> Text
applyRuleStep acc r = applyRule r acc

restore :: Text -> Text
restore txt = List.foldl' applyRuleStep txt rules

textToStringAdapter :: (Text -> Text) -> String -> String
textToStringAdapter f = T.unpack . f . T.pack

restoreFilename :: String -> String
restoreFilename = textToStringAdapter restore