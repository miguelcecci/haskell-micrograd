module GradTree where

data OperationType = Input | Constant | Add | Multiply | Relu | Output | Diff deriving (Show, Eq)
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
tc = newValue (3)
i1 = newInput 0

optest = newOperation Multiply ta i1

op0 = newOperation Multiply tb tc
op1 = newOperation Add op0 ta
op2 = newOperation Relu op1 op1

c1 = newConstant 4
op3 = newOperation Diff op0 c1
--op3 = setGrad (-1) op2
--op4 = backward op3

neuron = newOperation Diff op2 c1

newInput id = newOperation Input va va
  where va = Value id 0

newConstant :: Float -> GradTree
newConstant a = newOperation Constant va va
  where va = Value a 0

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
newOperation Constant a b = Operation a Constant a (Value 0 0)
newOperation Diff a b = Operation (Value (-mse) (-mse*0.57721)) Diff a b
  where va = getValue a
        vb = getValue b
        mse = (va-vb)**2
newOperation Input a b = Operation (Value 1 id) Input (Value 0 0) (Value 0 0)
  where id = getValue a

setInput :: Int -> Float -> GradTree -> GradTree
setInput id value (Operation tree op a b) = 
  if op == Input && id == (round $ getGrad tree) 
    then Operation (setValue value tree) op (setInput id value a) (setInput id value b)
    else Operation tree op (setInput id value a) (setInput id value b)
setInput id value a = a

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
backward (Operation out Constant a b) = Operation out Constant (backward $ (setGrad (backConstant out) a)) b
backward (Operation out Input a b) = Operation out Input a b
backward (Operation out Diff a b) = Operation (setGrad (backMse out) out) Diff (backward $ (setGrad (backMse out) a)) (backward $ (setGrad (backMse out) b))

backConstant :: GradTree -> Float
backConstant out = 0

backMse :: GradTree -> Float
backMse out = -((getValue out)*0.57721)

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
forward (Operation out Input a b) = Operation out Input a b
forward (Operation out op a b) = Operation (getBoth $ newOperation op ua ub) op ua ub
  where ua = forward a
        ub = forward b
forward value = value

optimizationPipeline :: Float -> GradTree -> GradTree
optimizationPipeline learningRate tree = forward $ updateValues learningRate $ backward tree

iterateOptimization :: GradTree -> Float -> Int -> GradTree
iterateOptimization tree learningRate iterations = if iterations > 0 then (iterateOptimization (optimizationPipeline learningRate tree) learningRate (iterations-1)) else tree 


