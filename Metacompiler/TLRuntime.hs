module Metacompiler.TLRuntime where

import qualified Data.Map as M
import Metacompiler.GenSym
import Metacompiler.TLSyntax

data ReducedMetaObject
	= RMOFun
		(ReducedMetaObject -> GenSym ReducedMetaObject)
	| RMOJSType {
		nameOfJSType :: String,
		paramsOfJSType :: [MetaObject]
	}
	| RMOJSTerm {
		slEquivalentOfJSTerm :: SL.Term,
		jsEquivalentOfJSTerm :: JSNode
	}

reduce :: M.Map String ReducedMetaObject -> M.Map String JSNode -> MetaObject -> GenSym ReducedMetaObject
reduce vars jsVars (MOApp { funOfMetaObject = f, argOfMetaObject = a }) = do
	RMOFun f' <- reduce vars jsVars f
	a' <- reduce vars jsVars a
	f' a'
reduce vars jsVars (MOAbs { paramsOfMetaObject = params, resultOfMetaObject = result }) = build vars params
	where
		build vars [] =
			reduce vars jsVars result
		build vars ((paramName, paramType):params) =
			return $ RMOFun (\paramValue -> build (M.insert paramName paramValue vars) params)
reduce vars jsVars (MOVar { varOfMetaObject = var }) = case M.lookup var vars of
	Just value -> value
	Nothing -> error ("var not in scope: " ++ show var)
reduce vars jsVars (MOJSExpr { specOfMetaObject = s, implOfMetaObject = b }) = do
	jsEquivalent <- expandJavascriptBlock vars jsVars b
	RMOJSTerm {
		slEquivalentOfJSTerm = undefined,
		jsEquivalentOfJSTerm = jsEquivalent
	}
reduce vars jsVars (MOJSSubstitution { jsSubstitutionOfMetaObject = x }) =
	RMOJSTerm {
		slEquivalentOfJSTerm = undefined,
		jsEquivalentOfJSTerm = substituteJSVars jsVars x
	}

safeUnion :: (Ord k, Show k) => M.Map k v -> M.Map k v -> M.Map k v
safeUnion = M.unionWithKey (\k -> error ("redefinition of " ++ show k))

safeFromList :: (Ord k, Show k) => [(k, v)] -> M.Map k v
safeFromList [] = M.empty
safeFromList ((k, v):xs) = case M.lookup k rest of
	Just _ -> error ("redefinition of " ++ show k)
	Nothing -> M.insert k v rest
	where rest = safeFromList xs

expandJavascriptBlock :: M.Map String ReducedMetaObject -> M.Map String JSNode -> JavascriptBlock -> JSNode
expandJavascriptBlock vars jsVars (JavascriptBlock { codeOfJavascriptBlock = code, varsOfJavascriptBlock = jsvs }) = do
	freeVars <- liftM safeFromList $ sequence [do
		uniqueSymbol <- genSym
		return (name, name ++ uniqueSymbol)
		| (name, JBVFree) <- jsvs]
	let bannedVars = safeFromList [(name, error "ambiguous substitution") | (name, JBVSet _) <- jsvs]
	setVars <- liftM safeFromList $ sequence [do
		reduced <- reduce vars (jsVars `safeUnion` freeVars `safeUnion` bannedVars) value
		return (name, jsEquivalentOfJSTerm reduced)
		| (name, JBVSet value) <- jsvs]
	substituteJSVars jsVars (jsVars `safeUnion` freeVars `safeUnion` setVars) code

