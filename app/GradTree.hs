module GradTree where

data OperationType = Constant | Add | Multiply | Relu | Output deriving Show
data GradTree = Value Float Float | Operation GradTree OperationType GradTree GradTree deriving Show

-- Structure sketch
--
-- Binary tree structure, every operation is a node, every value is a leaf.

-- HEAD: LEAF
-- OP: Operation type
-- LEFT: leaf
-- RIGHT: leaf

-- LEAF Value Grad

--test values
--ta = newValue 5
--tb = newValue 2
--tc = newValue (-3)
--op1 = newOperation Add ta tb
--op2 = newOperation Relu op1 op1

newValue :: Float -> GradTree
newValue a = Value a 0

newOperation :: OperationType -> GradTree -> GradTree -> GradTree
newOperation Add a b = Operation (Value (va+vb) 0) Add a b
  where va = getValue a
        vb = getValue b
newOperation Multiply a b = Operation (Value (va*vb) 0) Multiply a b
  where va = getValue a
        vb = getValue b
newOperation Relu a b = Operation (Value (if va >= 0 then va else 0) 0) Relu a (Value 0 0)
  where va = getValue a

getValue :: GradTree -> Float
getValue (Value value _) = value
getValue (Operation (Value value _) _ _ _) = value

getGrad :: GradTree -> Float
getGrad (Value _ grad) = grad
getGrad (Operation (Value _ grad ) _ _ _) = grad

updateValue :: Float -> GradTree -> GradTree
updateValue value (Value _ grad) = Value value grad
updateValue value (Operation (Value _ grad) op a b) = Operation (Value value grad) op a b

updateGrad :: Float -> GradTree -> GradTree
updateGrad grad (Value value _) = Value value grad
updateGrad grad (Operation (Value value _) op a b) = Operation (Value value grad) op a b

backward :: GradTree -> GradTree
backward (Value x xg) = Value x xg
backward (Operation out Add a b) = Operation out Add (backward $ (updateGrad (backAdd out a) a)) (backward $ (updateGrad (backAdd out b) b))
backward (Operation out Multiply a b) = Operation out Multiply (backward $ (updateGrad (backMultiply out b) a)) (backward $ (updateGrad (backMultiply out a) b))
backward (Operation out Relu a b) = Operation out Relu (backward $ (updateGrad (backRelu out) a)) b

backAdd :: GradTree -> GradTree -> Float
backAdd out value = (getValue value) + (getGrad out)

backMultiply :: GradTree -> GradTree -> Float
backMultiply out value = (getValue value) * (getGrad out)

backRelu :: GradTree -> Float
backRelu out = (getGrad out) * (fromIntegral $ fromEnum ((getValue out) > 0.0))

sumGradsToValues :: GradTree -> GradTree
sumGradsToValues (Value x xg) = Value x+xg xg
sumGradsToValues (Operations out op a b) = Operations out op (sumGradsToValues a) (sumGradsToValues b)

forward :: GradTree -> GradTree
forward (Value x xg) = Value x xg
forward (Operation out op a b) = Operation (getValue $ newOperation op ua ub) op ua ub
  where ua = forward a
		      ub = forward b
