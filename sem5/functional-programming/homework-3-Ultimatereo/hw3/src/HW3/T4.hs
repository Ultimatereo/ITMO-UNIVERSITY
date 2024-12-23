module HW3.T4
  ( State (..),
    Prim (..),
    Expr (..),
    mapState,
    wrapState,
    joinState,
    modifyState,
    eval,
  )
where

import HW3.T1

newtype State s a = S {runS :: s -> Annotated s a}

-- Functor instance for State
instance Functor (State s) where
  fmap = mapState

-- Applicative instance for State
instance Applicative (State s) where
  pure = wrapState
  stateF <*> stateA = S $ \s ->
    let f :# s2 = runS stateF s
        stateB = fmap f stateA
     in runS stateB s2

-- Monad instance for State
instance Monad (State s) where
  stateA >>= f = S $ \s ->
    let a :# s2 = runS stateA s
     in (runS $ f a) s2

-- mapState implementation
mapState :: (a -> b) -> State s a -> State s b
mapState f (S run) = S $ \s -> mapAnnotated f (run s)

-- wrapState implementation
wrapState :: a -> State s a
wrapState x = S $ \s -> x :# s

-- joinState implementation
joinState :: State s (State s a) -> State s a
joinState state = S $ \s ->
  let state2 :# s2 = runS state s
   in runS state2 s2

-- modifyState implementation
modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Show)

data Expr = Val Double | Op (Prim Expr)
  deriving (Show)

instance Num Expr where
  x + y = Op $ Add x y
  x - y = Op $ Sub x y
  x * y = Op $ Mul x y
  abs = Op . Abs
  signum = Op . Sgn
  fromInteger = Val . fromInteger

instance Fractional Expr where
  x / y = Op $ Div x y
  fromRational = Val . fromRational

eval :: Expr -> State [Prim Double] Double
eval (Val v) = wrapState v
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
  State [Prim Double] Double
evalBinary exprOp doubleOp left right = do
  x <- eval left
  y <- eval right
  modifyState (\s -> exprOp x y : s)
  return $ doubleOp x y

evalUnary ::
  (Double -> Prim Double) ->
  (Double -> Double) ->
  Expr ->
  State [Prim Double] Double
evalUnary exprOp doubleOp value = do
  x <- eval value
  modifyState (\s -> exprOp x : s)
  return $ doubleOp x
