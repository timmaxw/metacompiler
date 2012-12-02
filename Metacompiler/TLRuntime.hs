module Metacompiler.TLRuntime where

import Control.Monad (liftM, unless)
import Control.Monad.State
import Control.Monad.Trans (lift)
import qualified Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Maybe (isJust)
import qualified Language.ECMAScript3.PrettyPrint as JS
import qualified Language.ECMAScript3.Syntax as JS
import qualified Language.ECMAScript3.Syntax.Annotations as JS
import qualified Metacompiler.JSUtils as JSUtils
import Metacompiler.SExpr (Range, formatRange)
import Metacompiler.SExprToSL (errorContext)   -- TODO: Move `errorContext` into its own file
import qualified Metacompiler.SLSyntax as SL
import qualified Metacompiler.TLSyntax as TL

-- `RMT` stands for `reduced meta-type`. It's an internal de-sugared
-- representation of meta-types.

data RMT
	= RMTJSType
	| RMTJSTerm RMO Bool
	| RMTFun (String, RMT) RMT

formatRMT :: RMT -> String
formatRMT RMTJSType = "js-type"
formatRMT (RMTJSTerm ty True) = "(js-sl-term " ++ formatRMO ty ++ ")"
formatRMT (RMTJSTerm ty False) = "(js-term " ++ formatRMO ty ++ ")"
formatRMT (RMTFun (name, arg) res) = "(fun (" ++ name ++ " :: " ++ formatRMT arg ++ ") -> " ++ formatRMT res ++ ")"

-- `RMO` stands for `reduced meta-object`. Note that it's not necessarily in
-- weak head normal form because of `RMOUnknown`.

data RMO
	-- `RMOUnknown ty _` represents an unknown value of type `ty`. These arise
	-- when we try to evaluate a term which contains a variable whose value is
	-- not known; this happens when we are reasoning about dependent types.
	-- 
	-- If the `RMOUnknown` corresponds exactly to a single variable, then the
	-- second field will be `(Just name)` where `name` is the name of the
	-- variable. This allows for comparison of dependent types in simple cases.
	-- If the second field is `Nothing`, then the `RMOUnknown` could have any
	-- value.
	= RMOUnknown RMT (Maybe String)

	-- `RMOJSRepr`, `RMOJSTerm`, and `RMOFun` are in weak head normal form.
	| RMOJSRepr String [RMO]
	| RMOJSTerm RMO (Maybe (SL.Term Range)) (State JSGlobals (JS.Expression ()))
	| RMOFun (String, RMT) RMT (RMO -> RMO)

formatRMO :: RMO -> String
formatRMO (RMOUnknown ty (Just name)) = name
formatRMO (RMOUnknown ty Nothing) = "<unknown " ++ formatRMT ty ++ ">"
formatRMO (RMOJSRepr name params) = "(" ++ Data.List.intercalate " " (name:map formatRMO params) ++ ")"
formatRMO unprintable = "<" ++ formatRMT (typeOfRMO unprintable) ++ ">"

-- `typeOfRMO` returns the `RMT` representing the type of the given `RMO`

typeOfRMO :: RMO -> RMT
typeOfRMO (RMOUnknown ty _) = ty
typeOfRMO (RMOJSRepr _ _) = RMTJSType
typeOfRMO (RMOJSTerm ty (Just _) _) = RMTJSTerm ty True
typeOfRMO (RMOJSTerm ty Nothing _) = RMTJSTerm ty False
typeOfRMO (RMOFun (name, argTy) retTy _) = RMTFun (name, argTy) retTy

-- `substituteRMT` and `substituteRMO` assign to the value of a variable in the
-- given RMO.

substituteRMT :: M.Map String RMO -> RMT -> RMT
substituteRMT vars RMTJSType =
	RMTJSType
substituteRMT vars (RMTJSTerm ty hasSL) =
	RMTJSTerm (substituteRMO vars ty) hasSL
substituteRMT vars (RMTFun (argName, argType) retType) =
	RMTFun
		(argName, substituteRMT vars argType)
		(substituteRMT (M.delete argName vars) retType)

substituteRMO :: M.Map String RMO -> RMO -> RMO
substituteRMO vars (RMOUnknown ty (Just name)) | name `M.member` vars =
	(M.!) vars name
substituteRMO vars (RMOUnknown ty val) =
	RMOUnknown (substituteRMT vars ty) val
substituteRMO vars (RMOJSRepr name params) =
	RMOJSRepr name (map (substituteRMO vars) params)
substituteRMO vars (RMOJSTerm ty maybeSL jsEquivalent) =
	RMOJSTerm
		(substituteRMO vars ty)
		(fmap (const (error "SL not supported yet")) maybeSL)
		jsEquivalent
substituteRMO vars (RMOFun (argName, argType) retType impl) =
	RMOFun
		(argName, substituteRMT vars argType)
		(substituteRMT (M.delete argName vars) retType)
		(substituteRMO vars . impl)

-- `equivalentRMO` checks if two `RMO`s are equivalent. `canCastRMT` checks if
-- it's safe to cast from one `RMT` to another. Both return one of three
-- outcomes: `Provably True`, `NotProvable`, or `Provably False`. Both take a
-- mapping from pairs of variable names to `Provability Bool`, which indicates
-- which variables in the first expression's context are provably equal /
-- provably not equal to variables in the second expression's context.

data Provability a = Provably a | NotProvable

provabilityAnd :: Provability Bool -> Provability Bool -> Provability Bool
provabilityAnd (Provably True) (Provably True) = Provably True
provabilityAnd (Provably False) _ = Provably False
provabilityAnd _ (Provably False) = Provably False
provabilityAnd _ _ = NotProvable

equivalentRMO :: M.Map (String, String) (Provability Bool) -> RMO -> RMO -> Provability Bool
equivalentRMO vars (RMOUnknown _ (Just var1)) (RMOUnknown _ (Just var2)) =
	case M.lookup (var1, var2) vars of
		Just res -> res
		Nothing
			| var1 == var2 && varsNotMentioned -> Provably True
			| otherwise -> NotProvable
			where varsNotMentioned = all (\ (v1, v2) -> v1 /= var1 && v2 /= var2) (M.keys vars)
equivalentRMO vars (RMOJSRepr name1 params1) (RMOJSRepr name2 params2) =
	if name1 == name2
		then foldr provabilityAnd (Provably True) (zipWith (equivalentRMO vars) params1 params2)
		else Provably False
equivalentRMO vars _ _ = NotProvable

canCastRMT :: M.Map (String, String) (Provability Bool) -> RMT -> RMT -> Provability Bool
canCastRMT vars (RMTJSType) (RMTJSType) = Provably True
canCastRMT vars (RMTJSTerm ty1 hasSL1) (RMTJSTerm ty2 hasSL2) =
	if (hasSL1 || not hasSL2)
		then equivalentRMO vars ty1 ty2
		else Provably False
canCastRMT vars (RMTFun (var1, arg1) ret1) (RMTFun (var2, arg2) ret2) =
	canCastRMT vars arg2 arg1
	`provabilityAnd`
	canCastRMT
		(M.insert (var1, var2) (Provably True) $ M.filterWithKey (\ (v1, v2) _ -> v1 /= var1 && v2 /= var2) $ vars)
		ret1 ret2
canCastRMT vars _ _ = Provably False

-- Sometimes it's necessary to use RMTs as indices for a map. However, it
-- would be a little strange to define `(Ord RMT)`. So instead we have the type
-- `IndexRMT`; it is a wrapper around `RMT`, but with `Eq` and `Ord` defined.
-- It's an error to put an RMT containing `RMOUnknown _ Nothing` into an
-- `IndexRMT`. `IndexRMO` is like `IndexRMT`, but it's an error for it to
-- contain anything other than `RMOJSRepr`.
--
-- Naturally, if `IndexRMT a == IndexRMT b`, then `canCastRMT a b` will return
-- `Provably True`, and if `IndexRMO a == IndexRMO b`, then `equivalentRMO a b`
-- will return `Provably True`.

newtype IndexRMT = IndexRMT { unIndexRMT :: RMT }

instance Eq IndexRMT where
	a == b = compare a b == EQ

instance Ord IndexRMT where
	compare (IndexRMT a) (IndexRMT b) = compareRMTs ([], []) a b

newtype IndexRMO = IndexRMO { unIndexRMO :: RMO }

instance Eq IndexRMO where
	a == b = compare a b == EQ

instance Ord IndexRMO where
	compare (IndexRMO a) (IndexRMO b) = compareRMOs ([], []) a b

thenCompare :: Ordering -> Ordering -> Ordering
thenCompare EQ a = a
thenCompare a _ = a

compareRMTs :: ([String], [String]) -> RMT -> RMT -> Ordering
compareRMTs _ RMTJSType RMTJSType =
	EQ
compareRMTs vars (RMTJSTerm ty1 hasSL1) (RMTJSTerm ty2 hasSL2) =
	compareRMOs vars ty1 ty2 `thenCompare` compare hasSL1 hasSL2
compareRMTs vars@(vars1, vars2) (RMTFun (argName1, argType1) retType1) (RMTFun (argName2, argType2) retType2) =
	compareRMTs vars argType1 argType2 `thenCompare`
		compareRMTs (argName1:vars1, argName2:vars2) retType1 retType2
compareRMTs _ (RMTJSType) (RMTJSTerm _ _) = LT
compareRMTs _ (RMTJSType) (RMTFun _ _) = LT
compareRMTs _ (RMTJSTerm _ _) (RMTJSType) = GT
compareRMTs _ (RMTJSTerm _ _) (RMTFun _ _) = LT
compareRMTs _ (RMTFun _ _) (RMTJSType) = GT
compareRMTs _ (RMTFun _ _) (RMTJSTerm _ _) = GT

compareRMOs :: ([String], [String]) -> RMO -> RMO -> Ordering
compareRMOs (vars1, vars2) (RMOUnknown _ (Just var1)) (RMOUnknown _ (Just var2)) =
	compare (Data.List.elemIndex var1 vars1) (Data.List.elemIndex var2 vars2)
compareRMOs vars (RMOJSRepr name1 params1) (RMOJSRepr name2 params2) =
	compare name1 name2 `thenCompare` foldr thenCompare EQ (zipWith (compareRMOs vars) params1 params2)
compareRMOs _ (RMOUnknown _ (Just _)) (RMOJSRepr _ _) =
	LT
compareRMOs _ (RMOJSRepr _ _) (RMOUnknown _ (Just _)) =
	GT
compareRMOs _ _ _ =
	error "can't compare RMOs other than (RMOUnknown _ (Just _)) or (RMOJSRepr _ _)"

-- `JSGlobals` is the type of the state that is maintained when generating
-- JavaScript equivalents.

data JSGlobals = JSGlobals {
	topLevelOfJSGlobals :: String,
	instantiationsOfJSGlobals :: M.Map (JSGlobalUniqueId, M.Map String IndexRMO) String,
	symbolRenamingOfJSGlobals :: JSUtils.SymbolRenaming
	}

initialJSGlobals :: JSGlobals
initialJSGlobals = JSGlobals {
	topLevelOfJSGlobals = "",
	instantiationsOfJSGlobals = M.empty,
	symbolRenamingOfJSGlobals = JSUtils.initialSymbolRenaming
	}

