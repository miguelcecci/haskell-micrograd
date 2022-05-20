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
ta = newValue 5
tb = newValue 2
tc = newValue (-3)
op1 = newOperation Add ta tb
op2 = newOperation Relu op1 op1

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

getBoth :: GradTree -> GradTree 
getBoth (Operation a _ _ _) = a
getBoth a = a

getValue :: GradTree -> Float
getValue (Value value _) = value
getValue (Operation (Value value _) _ _ _) = value

getGrad :: GradTree -> Float
getGrad (Value _ grad) = grad
getGrad (Operation (Value _ grad ) _ _ _) = grad

setValue :: Float -> GradTree -> GradTree
setValue value (Value _ grad) = Value value grad
setValue value (Operation (Value _ grad) op a b) = Operation (Value value grad) op a b

setGrad :: Float -> GradTree -> GradTree
setGrad grad (Value value _) = Value value grad
setGrad grad (Operation (Value value _) op a b) = Operation (Value value grad) op a b

backward :: GradTree -> GradTree
backward (Value x xg) = Value x xg
backward (Operation out Add a b) = Operation out Add (backward $ (setGrad (backAdd out a) a)) (backward $ (setGrad (backAdd out b) b))
backward (Operation out Multiply a b) = Operation out Multiply (backward $ (setGrad (backMultiply out b) a)) (backward $ (setGrad (backMultiply out a) b))
backward (Operation out Relu a b) = Operation out Relu (backward $ (setGrad (backRelu out) a)) b

backAdd :: GradTree -> GradTree -> Float
backAdd out value = (getValue value) + (getGrad out)

backMultiply :: GradTree -> GradTree -> Float
backMultiply out value = (getValue value) * (getGrad out)

backRelu :: GradTree -> Float
backRelu out = (getGrad out) * (fromIntegral $ fromEnum ((getValue out) > 0.0))

updateValues :: Float -> GradTree -> GradTree
updateValues learningRate (Value x xg) = Value (x-learningRate*xg) xg
updateValues learningRate (Operation out op a b) = Operation out op (updateValues learningRate a) (updateValues learningRate b)

forward :: GradTree -> GradTree
forward (Operation out op a b) = Operation (getBoth $ newOperation op ua ub) op ua ub
  where ua = forward a
        ub = forward b
forward value = value

