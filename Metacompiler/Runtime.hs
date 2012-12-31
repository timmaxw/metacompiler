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

-- `traverseMetaType` and `traverseMetaObject` invoke `visitMetaType` or `visitMetaObject` of the given visitor on each
-- sub-node of the given meta-type or meta-object, then combine the results using an applicative functor. They are a
-- generic way to implement many different things with a minimum of boilerplate.

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

-- `substituteMetaType` and `substituteMetaObject` traverse the given meta-type or meta-object; whenever they encounter
-- a reference to a free variable that appears in the given map, they replace it with its value from the map. They
-- operate on both TL names and SL names simultaneously. They correctly implement name shadowing and avoid capturing
-- variables. For example, replacing `a` with `b` in `fun (a :: ...) -> a` will leave it unchanged, and replacing `a`
-- with `b` in `fun (b :: ...) -> a` will produce `fun (b' :: ...) -> b`.

data Substitutions = Substitutions {
	nameSubstitutions :: M.Map Name MetaObject,
	nameOfSLTypeSubstitutions :: M.Map NameOfSLType MetaObject,
	nameOfSLTermSubstitutions :: M.Map NameOfSLTerm MetaObject
	}

substituteMetaType :: Substitutions -> MetaType -> MetaType
substituteMetaType subs (MTFun (paramName, paramType) returnType) = let
	paramType' = substituteMetaType subs paramType
	(subs', paramName') = prepareForBindingName (freeNamesInMetaType returnType) paramType' (subs, paramName)
	returnType' = substituteMetaType subs' returnType
	in MTFun (paramName', paramType') returnType'
substituteMetaType subs other = runIdentity (traverseMetaType (makeSubstitutionVisitor subs) other)

substituteMetaObject :: Substitutions -> MetaObject -> MetaObject
substituteMetaObject subs (MOAbs (paramName, paramType) body) = let
	paramType' = substituteMetaType subs paramType
	(subs', paramName') = prepareForBindingName (freeNamesInMetaObject body) paramType' (subs, paramName)
	body' = substituteMetaObject subs' body
	in MOAbs (paramName', paramType') body'
substituteMetaObject subs (MOName name type_) = case M.lookup name (nameSubstitutions subs) of
	Just value -> value
	Nothing -> MOName name (substituteMetaType subs type_)
substituteMetaObject subs (MOSLTypeName name kind) = case M.lookup name (nameOfSLTypeSubstitutions subs) of
	Just value -> value
	Nothing -> MOSLTypeName name kind
substituteMetaObject subs (MOSLTermName name type_) = case M.lookup name (nameOfSLTermSubstitutions subs) of
	Just value -> value
	Nothing -> MOSLTermName name (substituteMetaObject subs type_)
substituteMetaObject subs (MOSLTermAbs (paramName, paramType) body) = let
	paramType' = substituteMetaObject subs paramType
	(subs', paramName') = prepareForBindingNameOfSLTerm (freeNamesOfSLTermsInMetaObject body) paramType' (subs, paramName)
	body' = substituteMetaObject subs' body
	in MOSLTermAbs (paramName', paramType') body'
substituteMetaObject subs other = runIdentity (traverseMetaObject (makeSubstitutionVisitor subs) other)

makeSubstitutionVisitor :: Substitutions -> Visitor Identity
makeSubstitutionVisitor subs = Visitor {
	visitMetaType = Identity . substituteMetaType subs
	visitMetaObject = Identity . substituteMetaObject subs
	}

-- `freeNamesInMetaType` and `freeNamesInMetaObject` return sets of all unbound TL and SL variables that appear in the
-- given meta-type or meta-object.

data FreeNames = {
	namesInFreeNames :: S.Set Name,
	namesOfSLTypesInFreeNames :: S.Set NameOfSLType,
	namesOfSLTermsInFreeNames :: S.Set NameOfSLTerm
	}

instance Monoid FreeNames where
	mzero = FreeNames S.empty S.empty S.empty
	mappend (FreeNames a1 b1 c1) (FreeNames a2 b2 c2) = FreeNames (S.union a1 a2) (S.union b1 b2) (S.union c1 c2)

freeNamesInMetaType :: MetaType -> FreeNames
freeNamesInMetaType (MTFun (paramName, paramType) resultType) = let
	paramNames = freeNamesInMetaType paramType
	resultNames = freeNamesInMetaType resultType
	resultNames' = resultNames { namesInFreeNames = S.delete paramName (namesInFreeNames resultNames) }
	in paramNames `mappend` resultNames
freeNamesInMetaType other =
	execWriter (traverseMetaType freeNamesVisitor other)

freeNamesInMetaObject :: MetaObject -> FreeNames
freeNamesInMetaObject (MOAbs (paramName, paramType) body) = let
	paramNames = freeNamesInMetaType paramType
	bodyNames = freeNamesInMetaObject body
	bodyNames' = bodyNames { namesInFreeNames = S.delete paramName (namesInFreeNames bodyName) }
	in paramNames `mappend` bodyNames'
freeNamesInMetaObject (MOName n type_) =
	FreeNames (S.singleton n) S.empty S.empty `mappend` freeNamesInMetaType type_
freeNamesInMetaObject (MOSLTypeName n _) =
	FreeNames S.empty (S.singleton n) S.empty
freeNamesInMetaObject (MOSLTermName n type_) =
	FreeNames S.empty S.empty (S.singleton n) `mappend` freeNamesInMetaObject type_
freeNamesInMetaObject (MOSLTermAbs (paramName, paramType) body) = let
	paramNames = freeNamesInMetaObject paramType
	bodyNames = freeNamesInMetaObject body
	bodyNames' = bodyNames { namesOfSLTermsInFreeNames = S.delete paramName (namesOfSLTermsInFreeNames bodyNames) }
	in paramNames `mappend` bodyNames'
freeNamesInMetaObject other =
	execWriter (traverseMetaType freeNamesVisitor other)

freeNamesVisitor :: Visitor (Writer FreeNames)
freeNamesVisitor = Visitor {
	visitMetaType = \mt -> writer (mt, freeNamesInMetaType mt),
	visitMetaObject = \mt -> writer (mt, freeNamesInMetaObject mt)
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

prepareForBindingName :: FreeNames -> MetaType -> (M.Map Name MetaObject, Name) -> (M.Map Name MetaObject, Name)
prepareForBindingName freeNamesWithin nameType (subs, name) = let
	subs' = subs { nameSubstitutions = M.delete name (nameSubstitutions subs) }
	incomingNames =
			S.unions [
				maybe S.empty (\mo -> namesInFreeNames (freeNamesInMetaObject mo))
					(M.lookup name (nameSubstitutions subs'))
				| name <- S.toList (namesInFreeNames freeNamesWithin)]
		`S.union`
			S.unions [
				maybe S.empty (\mo -> namesInFreeNames (freeNamesInMetaObject mo))
					(M.lookup name (nameOfSLTermSubstitutions subs'))
				| name <- S.toList (namesOfSLTermsInFreeNames freeNamesWithin)]
	candidates = [Name (unName name ++ replicate n '\'') | n <- [1..]]
	forbidden = namesInFreeNames incomingNames
		`S.union` ((S.\\) (namesInFreeNames freeNamesWithin) (S.fromList (M.keys (nameSubstitutions subs'))))
	Just name' = find (`S.notMember` forbidden) candidates
	in if name `S.member` incomingNames
		then (name', subs' { nameSubstitutions = M.insert name (MOName name' nameType) (nameSubstitutions subs') })
		else (name, subs')

-- `prepareForBindingNameOfSLTerm` is like `prepareForBindingName` except that it's for terms which introduce a new
-- name into SL's term scope.

prepareForBindingNameOfSLTerm :: FreeNames -> MetaType -> (M.Map Name MetaObject, Name) -> (M.Map Name MetaObject, Name)
prepareForBindingNameOfSLTerm freeNamesWithin nameType (subs, name) = let
	subs' = subs { nameOfSLTermSubstitutions = M.delete name (nameOfSLTermSubstitutions subs) }
	incomingNames =
			S.unions [
				maybe S.empty (\mo -> namesOfSLTermsInFreeNames (freeNamesInMetaObject mo))
					(M.lookup name (nameSubstitutions subs'))
				| name <- S.toList (namesInFreeNames freeNamesWithin)]
		`S.union`
			S.unions [
				maybe S.empty (\mo -> namesOfSLTermsInFreeNames (freeNamesInMetaObject mo))
					(M.lookup name (nameOfSLTermSubstitutions subs'))
				| name <- S.toList (namesOfSLTermsInFreeNames freeNamesWithin)]
	candidates = [NameOfSLTerm (unNameOfSLTerm name ++ replicate n '\'') | n <- [1..]]
	forbidden = namesOfSLTermsInFreeNames incomingNames
		`S.union` ((S.\\) (namesOfSLTermsInFreeNames freeNamesWithin) (S.fromList (M.keys (nameOfSLTermSubstitutions subs'))))
	Just name' = find (`S.notMember` forbidden) candidates
	in if name `S.member` incomingNames
		then (name', subs' { nameOfSLTermSubstitutions = M.insert name (MOSLTermName name' nameType) (nameOfSLTermSubstitutions subs') })
		else (name, subs')

reduceMetaType :: MetaType -> MetaType
...

reduceMetaObject :: MetaObject -> MetaObject
...

