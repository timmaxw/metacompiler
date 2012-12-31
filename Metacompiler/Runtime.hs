module Metacompiler.Runtime where

newtype Name = Name { unName :: String } deriving (Ord, Eq)
newtype NameOfSLType = NameOfSLType { unNameOfSLType :: String } deriving (Eq, Ord)
newtype NameOfSLTerm = NameOfSLTerm { unNameOfSLTerm :: String } deriving (Eq, Ord)
newtype NameOfSLCtor = NameOfSLCtor { unNameOfSLCtor :: String } deriving (Eq, Show, Ord)

data SLKind
	= SLKindType
	| SLKindFun SLKind SLKind

data MetaType
	= MTFun (Name, MetaType) MetaType
	| MTSLType SLKind
	| MTSLTerm MetaObject
{-
	| MTJSEquivExprType MetaObject
	| MTJSEquivExpr MetaObject MetaObject
-}

data MetaObject
	= MOApp MetaObject MetaObject
	| MOAbs (Name, MetaType) MetaObject
	| MOName Name MetaType

	| MOSLTypeName NameOfSLType SLKind
	| MOSLTypeApp MetaObject MetaObject
	| MOSLTypeFun MetaObject MetaObject
	| MOSLTypeLazy MetaObject

	| MOSLTermName NameOfSLTerm [MetaObject] MetaObject
	| MOSLTermApp MetaObject MetaObject
	| MOSLTermAbs (NameOfSLTerm, MetaObject) MetaObject
	| MOSLTermCase MetaObject [(NameOfSLCtor, [NameOfSLTerm], MetaObject)]
	| MOSLTermWrap MetaObject
	| MOSLTermUnwrap MetaObject

{-
	| MOJSEquivExprLiteral MetaObject MetaObject (JS.Expression ()) (M.Map (JS.Id ()) BindingJSEquivExpr)
-}

{-
data BindingJSEquivExpr = BindingJSEquivExpr {
	paramsOfBindingJSEquivExpr :: [(Name, MetaObject, Name, MetaObject)],
	valueOfBindingJSEquivExpr :: MetaObject
	}
-}

typeOfMetaObject :: MetaObject -> MetaType
typeOfMetaObject (MOApp fun arg) = case typeOfMetaObject fun of
	MTFun (paramName, _) bodyType -> reduceMetaType (substituteMetaType (M.singleton paramName arg) bodyType)
	_ -> error "bad meta-object: MOApp of non-function"
typeOfMetaObject (MOAbs (paramName, paramType) body) = MTFun (paramName, paramType) (typeOfMetaObject body)
typeOfMetaObject (MOName _ type_) = type_
typeOfMetaObject (MOSLTypeName _ kind) = MTSLType kind
typeOfMetaObject (MOSLTypeApp fun _) = case typeOfMetaObject fun of
	MTSLType (SLKindFun _ retKind) -> MTSLType retKind
	_ -> error "bad meta-object: MOSLTypeApp of non-function kind"
typeOfMetaObject (MOSLTypeFun _ _) = MTSLType SLKindType
typeOfMetaObject (MOSLTypeLazy _) = MTSLType SLKindType
typeOfMetaObject (MOSLTermName _ subs type_) = substituteSLType (M.fromList subs) type_
typeOfMetaObject (MOSLTermApp fun _) = case typeOfMetaObject fun of
	MTSLTerm funSLType -> case reduceMetaObject slType of
		MOSLTypeFun _ retSLType -> MTSLTerm retSLType
		_ -> error "bad meta-object: MOSLTypeApp of SL non-function"
	_ -> error "bad meta-object: MOSLTypeApp of non-SL"
typeOfMetaObject (MOSLTermAbs (_, paramType) body) = case typeOfMetaObject body of
	MTSLTerm bodyType -> MTSLTerm (MTSLTypeFun paramType bodyType)
	_ -> error "bad meta-object: MOSLTermAbs of non-SL"
typeOfMetaObject (MOSLTermCase subject clauses) = case clauses of
	((_, _, first):_) -> typeOfMetaObject first
	_ -> error "bad meta-object: MOSLTermCase needs at least one clause"
typeOfMetaObject (MOSLTermWrap x) = case typeOfMetaObject x of
	MTSLTerm xType -> MTSLTerm (MOSLTypeLazy xType)
typeOfMetaObject (MOSLTermUnwrap x) = case typeOfMetaObject x of
	MTSLTerm xType -> case reduceMetaObject xType of
		MOSLTypeLazy xInnerType -> MTSLTerm xInnerType
		_ -> error "bad meta-object: MOSLTermUnwrap needs a MOSLTypeLazy"
{-
typeOfMetaObject (MOJSEquivExprLiteral slEquiv type_ _) = MTJSEquivExpr slEquiv type_
-}

data Visitor f = Visitor {
	visitMetaType :: MetaType -> f MetaType,
	visitMetaObject :: MetaObject -> f MetaObject
	}

defaultVisitor :: Applicative f => Visitor f -> Visitor f
defaultVisitor subVisitor = Visitor {
	visitMetaType = traverseMetaType subVisitor,
	visitMetaObject = traverseMetaObject subVisitor
	}

traverseMetaType :: Applicative f => Visitor f -> MetaType -> f MetaType
traverseMetaType v t = case t of
	MTFun (paramName, paramType) returnType -> liftA2 MTFun (liftA (paramName,) (visitT paramType)) (visitT returnType)
	MTSLType kind -> pure (MTSLType kind)
	MTSLTerm type_ -> liftA MTSLTerm (visitO type_)
	where
		visitT = visitMetaType v
		visitO = visitMetaObject v

traverseMetaObject :: Applicative f => Visitor f -> MetaObject -> f MetaObject
traverseMetaObject v t = case t of
	MOApp fun arg -> liftA2 MOApp (visitO fun) (visitO arg)
	MOAbs (paramName, paramType) body -> liftA2 MOAbs (liftA (paramName,) (visitT paramType)) (visitO body)
	MOName name type_ -> liftA (MOName name) (visitT type_)
	MOSLTypeName name kind -> pure (MOSLTypeName name kind)
	MOSLTypeApp fun arg -> liftA2 MOSLTypeApp (visitO fun) (visitO arg)
	MOSLTypeFun argType retType -> liftA2 MOSLTypeFun (visitO argType) (visitO retType)
	MOSLTypeLazy x -> liftA MOSLTypeLazy (visitO x)
	MOSLTermName name params type_ -> liftA2 (MOSLTermName name) (traverse visitO params) (visitO type_)
	MOSLTermApp fun arg -> liftA2 MOSLTermApp (visitO fun) (visitO arg)
	MOSLTermAbs (paramName, paramType) body -> liftA2 MOSLTermAbs (liftA (paramName,) (visitO paramType)) (visitO body)
	MOSLTermCase subject clauses -> liftA2 MOSLTermCase
		(visitO subject)
		(traverse (\(ctor, fields, body) -> liftA (ctor, fields,) (visitO body)) clauses)
	MOSLTermWrap x -> liftA MOSLTermWrap (visitO x)
	MOSLTermUnwrap x -> liftA MOSLTermUnwrap (visitO x)
	where
		visitT = visitMetaType v
		visitO = visitMetaObject v

-- `substituteNamesInMetaType` and `substituteNamesInMetaObject` traverse the given meta-type or meta-object; whenever
-- they encounter a reference to a free variable that appears in the given map, they replace it with its value from the
-- map. They correctly implement name shadowing and avoid capturing variables. For example, replacing `a` with `b` in
-- `fun (a :: ...) -> a` will leave it unchanged, and replacing `a` with `b` in `fun (b :: ...) -> a` will produce
-- `fun (b' :: ...) -> b`.

substituteNamesInMetaType :: M.Map Name MetaObject -> MetaType -> MetaType
substituteNamesInMetaType subs (MTFun (paramName, paramType) returnType) = let
	paramType' = substituteNamesInMetaType subs paramType
	(subs', paramName') = prepareForBinding (unboundNamesInMetaType returnType) paramType' (subs, paramName)
	returnType' = substituteNamesInMetaType subs' returnType
	in MTFun (paramName', paramType') returnType'
substituteNamesInMetaType subs other = runIdentity (traverseMetaType (makeSubstituteNamesVisitor subs) other)

substituteNamesInMetaObject :: M.Map Name MetaObject -> MetaObject -> MetaObject
substituteNamesInMetaObject subs (MOAbs (paramName, paramType) body) = let
	paramType' = substituteMetaType subs paramType
	(subs', paramName') = prepareForBinding (unboundNamesInMetaObject body) paramType' (subs, paramName)
	body' = substituteMetaObject subs' body
	in MOAbs (paramName', paramType') body'
substituteNamesInMetaObject subs (MOName name type_) = case M.lookup name subs of
	Just value -> value
	Nothing -> MOName name (substituteNamesInMetaType subs type_)
substituteNamesInMetaObject subs other = runIdentity (traverseMetaObject (makeSubstituteNamesVisitor subs) other)

makeSubstituteNamesVisitor :: M.Map Name MetaObject -> Visitor Identity
makeSubstituteNamesVisitor subs = Visitor {
	visitMetaType = Identity . substituteNamesInMetaType subs
	visitMetaObject = Identity . substituteNamesInMetaObject subs
	}

-- `unboundNamesInMetaType` and `unboundNamesInMetaObject` return sets of all unbound variables that appear in the
-- given meta-type or meta-object.

unboundNamesInMetaType :: MetaType -> S.Set Name
unboundNamesInMetaType = execWriter . traverseMetaType unboundNamesVisitor

unboundNamesInMetaObject :: MetaObject -> S.Set Name
unboundNamesInMetaObject (MOName n type_) = S.insert n (unboundNamesInMetaType type_)
unboundNamesInMetaObject other = execWriter (traverseMetaType unboundNamesVisitor other)

unboundNamesVisitor :: Visitor (Writer (S.Set Name))
unboundNamesVisitor = Visitor {
	visitMetaType = \mt -> writer (mt, unboundNamesInMetaType mt),
	visitMetaObject = \mt -> writer (mt, unboundNamesInMetaObject mt)
	}

-- `prepareForBindingName` is a helper function used when performing substitutions on a meta-type or meta-object which
-- introduces a new variable into scope. `name` is the new variable being introduced into scope, and `nameType` is its
-- meta-type. `freeNamesWithin` are the variables that are free in the part of the term where `name` is in scope.
-- `subs` are the substitutions to be performed. The return value is new values for `subs` and `name`. It performs two
-- jobs:
--  1. It correctly implements name shadowing by removing `name` from `subs` if it appears there
--  2. It implements capture-avoiding substitution by checking whether any of the values of the `subs` map contain
--     `name`, and changing `name` to an unused variable if so. If it is necessary to change `name`, it will also make
--     a new entry in `subs` to perform the change.
prepareForBindingName :: S.Set Name -> MetaType -> (M.Map Name MetaObject, Name) -> (M.Map Name MetaObject, Name)
prepareForBindingName freeNamesWithin nameType (subs, name) = if name `S.member` incomingNames then let
			candidates = [name ++ replicate n '\'' | n <- [1..]]
			forbidden = incomingNames `S.union` ((S.\\) returnTypeNames (S.fromList (M.keys subs)))
			Just name' = find (`S.notMember` forbidden) candidates
			in (name', M.insert name (MOName name' nameType) subs')
		else (name, subs')
	where
		subs' = M.delete name subs
		incomingNames = S.unions [
			maybe S.empty (\mo -> unboundNamesInMetaObject mo) (M.lookup name subs')
			| name <- S.toList freeNamesWithin]

reduceMetaType :: MetaType -> MetaType
...

reduceMetaObject :: MetaObject -> MetaObject
...

