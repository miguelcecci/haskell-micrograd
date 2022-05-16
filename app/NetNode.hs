module GradTree where


data OperationType = Constant | Add | Multiply | Relu | Output deriving Show

-- Structure sketch
-- HEAD: LEAF
-- OP: Operation type
-- LEFT: leaf
-- RIGHT: leaf

-- LEAF Value Grad

data GradTree = Value Float Float | Operation GradTree OperationType GradTree GradTree deriving Show

newValue :: Float -> GradTree
newValue a = Value a 0

getValue :: GradTree -> Float
getValue (Value value _) = value
getValue (Operation (Value value _) _ _ _) = value

newOperation :: OperationType -> GradTree -> GradTree -> GradTree
newOperation Add a b = Operation (Value (va+vb) 0) Add a b
  where va = getValue a
        vb = getValue b
newOperation Multiply a b = Operation (Value (va*vb) 0) Multiply a b
  where va = getValue a
        vb = getValue b
newOperation Relu a b = Operation (Value (if va >= 0 then va else 0) 0) Relu a (Value 0 0)
  where va = getValue a

ta = newValue 5
tb = newValue 2
op1 = newOperation Add ta tb

getGrad :: GradTree -> Float
getGrad (Value _ grad) = grad
getGrad (Operation (Value _ grad ) _ _ _) = grad

updateValue :: Float -> GradTree -> GradTree
updateValue value (Value _ grad) = Value value grad
updateValue value (Operation (Value _ grad) op a b) = Operation (Value value grad) op a b

updateGrad :: Float -> GradTree -> GradTree
updateGrad grad (Value value _) = Value value grad
updateGrad grad (Operation (Value value _) op a b) = Operation (Value value grad) op a b

--backward :: Float -> Float -> Float -> GradTree > GradTree
backward (Value x xg) = Value x xg
backward (Operation out Add a b) = Operation out Add (backward $ (updateGrad (backAdd out a) a)) (backward $ (updateGrad (backAdd out b) b))
backward (Operation out Multiply a b) = Operation out Multiply (backward $ (updateGrad (backMultiply out b) a)) (backward $ (updateGrad (backMultiply out a) b))
-- backward (Operation out Relu a b) =

backAdd :: GradTree -> GradTree -> Float
backAdd out value = (getValue value) + (getGrad out)

backMultiply :: GradTree -> GradTree -> Float
backMultiply out value = (getValue value) * (getGrad out)

backRelu :: GradTree -> Float
backRelu out = (getGrad out) * (fromIntegral $ fromEnum ((getValue out) > 0.0))
