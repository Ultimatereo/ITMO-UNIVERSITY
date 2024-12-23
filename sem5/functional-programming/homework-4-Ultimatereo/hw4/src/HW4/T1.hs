module HW4.T1
  ( EvaluationError (..),
    ExceptState (..),
    mapExceptState,
    wrapExceptState,
    joinExceptState,
    modifyExceptState,
    throwExceptState,
    eval,
  )
where

import HW4.Types

newtype ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f es = ES $ \s ->
  case runES es s of
    Error e -> Error e
    Success (x :# ann) -> Success (f x :# ann)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES $ \s -> Success (x :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState ess = ES $ \s ->
  case runES ess s of
    Error e -> Error e
    Success (es' :# ann) ->
      let res = runES es' ann
       in case res of
            Error e -> Error e
            _ -> res

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  ef <*> ex = do
    f <- ef
    f <$> ex

instance Monad (ExceptState e s) where
  return = pure
  es >>= f = ES $ \s ->
    case runES es s of
      Error e -> Error e
      Success (x :# ann) -> runES (f x) ann

data EvaluationError = DivideByZero
  deriving (Show)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val v) = wrapExceptState v
eval (Op (Add left right)) = evalBinary Add (+) left right
eval (Op (Sub left right)) = evalBinary Sub (-) left right
eval (Op (Mul left right)) = evalBinary Mul (*) left right
eval (Op (Div left right)) = evalBinary Div (/) left right
eval (Op (Abs value)) = evalUnary Abs abs value
eval (Op (Sgn value)) = evalUnary Sgn signum value

evalBinary ::
  (Double -> Double -> Prim Double) ->
  (Double -> Double -> Double) ->
  Expr ->
  Expr ->
  ExceptState EvaluationError [Prim Double] Double
evalBinary exprOp doubleOp left right = do
  x <- eval left
  y <- eval right
  runOp (exprOp x y)
  return $ doubleOp x y

runOp :: Prim Double -> ExceptState EvaluationError [Prim Double] ()
runOp (Div _ 0) = throwExceptState DivideByZero
runOp expr = modifyExceptState (expr :)

evalUnary ::
  (Double -> Prim Double) ->
  (Double -> Double) ->
  Expr ->
  ExceptState EvaluationError [Prim Double] Double
evalUnary exprOp doubleOp value = do
  x <- eval value
  modifyExceptState (\s -> exprOp x : s)
  return $ doubleOp x
