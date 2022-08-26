module GradTree where

import Data.List

data OperationType = Input | Constant | Add | Multiply | Pow | Relu | Sigmoid | Variable | Mae | Mse deriving (Show, Eq)
data GradTree = Empty | Value Float Float | Operation GradTree OperationType GradTree GradTree deriving Show

pretty :: GradTree -> String
pretty (Value a b) = " V(" ++ show a ++","++ show b ++") "
pretty (Operation (Value a b) Constant _ _) = " C("++ show a ++") "
pretty (Operation _ Input (Value a b) _) = " I(" ++ show a ++","++ show b ++") "
pretty (Operation (Value a b) Multiply x y) = "  ("++ pretty x ++" Multiply(" ++ show a ++","++ show b ++") "++ pretty y ++")  "
pretty (Operation (Value a b) Add x y) = "  ("++ pretty x ++" Add(" ++ show a ++","++ show b ++") "++ pretty y ++")  "
pretty (Operation (Value a b) Relu x y) = " (Relu(" ++ show a ++","++ show b ++") "++ pretty x ++")  "
pretty (Operation (Value a b) Mse x y) = "  ("++ pretty x ++" Mse(" ++ show a ++","++ show b ++") "++ pretty y ++")  "
pretty (Operation (Value a b) Mae x y) = "  ("++ pretty x ++" Mae(" ++ show a ++","++ show b ++") "++ pretty y ++")  "

-- Structure sketch
--
-- Binary tree structure, every operation is a node, every value is a leaf.

-- HEAD: LEAF
-- OP: Operation type
-- LEFT: leaf
-- RIGHT: leaf

-- LEAF Value Grad

newInput id = newOperation Input va va
  where va = Value 0 id

newConstant :: Float -> GradTree
newConstant a = newOperation Constant va va
  where va = Value a 0

newValue :: Float -> GradTree
newValue a = Value a 0

newActivation :: OperationType -> GradTree -> GradTree
newActivation Relu a = newOperation Relu a Empty
newActivation Sigmoid a = newOperation Sigmoid a Empty

newOperation :: OperationType -> GradTree -> GradTree -> GradTree
newOperation Add a b = Operation (Value (va+vb) 0) Add a b
  where va = getValue a
        vb = getValue b
newOperation Multiply a b = Operation (Value (va*vb) 0) Multiply a b
  where va = getValue a
        vb = getValue b
newOperation Pow a b = Operation (Value (va**vb) 0) Pow a b
  where va = getValue a
        vb = getValue b
newOperation Relu a Empty = Operation (Value (if va >= 0 then va else 0) 0) Relu a Empty
  where va = getValue a
newOperation Sigmoid a Empty = Operation (Value (1/(1+2.718281**(-va))) 0) Sigmoid a Empty
  where va = getValue a
newOperation Constant a b = Operation a Constant a Empty
newOperation Mae a b = Operation (Value (abs dif) (-dif)) Mae a b
  where va = getValue a
        vb = getValue b
        dif = (va-vb)
newOperation Mse a b = Operation (Value (dif**2) (-dif*2)) Mse a b
  where va = getValue a
        vb = getValue b
        dif = (va-vb)
newOperation Input a b = Operation (Value 0 id) Input (Value 0 id) Empty
  where id = getGrad a

setInput :: Int -> Float -> GradTree -> GradTree
setInput id value (Operation tree op a b) = 
  if op == Input && id == (fromIntegral $ round $ getGrad a) 
    then Operation (setValue value tree) op (setValue value a) Empty
    else Operation tree op (setInput id value a) (setInput id value b)
setInput id value a = a

reversalInsertInputs :: [Float] -> GradTree -> GradTree
reversalInsertInputs (x:inputList) tree = if ((==) 0 $ length inputList) then newTree else insertInputs inputList newTree 
  where newTree = setInput (length inputList) x tree

insertInputs :: [Float] -> GradTree -> GradTree
insertInputs inputList tree = forward $ reversalInsertInputs (reverse inputList) tree --executing forward propagation after insert new inputs


-- Manipulating leaves
--
--

getOutput :: GradTree -> Float
getOutput (Operation out Mse a b) = if (elemIndex (getOp a) [Input, Constant]) == Nothing then getValue a else getValue b
getOutput (Operation out Mae a b) = if (elemIndex (getOp a) [Input, Constant]) == Nothing then getValue a else getValue b
getOutput value = 0.0000

getOp :: GradTree -> OperationType
getOp (Operation _ op _ _) = op
getOp a = Variable

getOperationResult :: GradTree -> GradTree 
getOperationResult (Operation a _ _ _) = a
getOperationResult a = a

getValue :: GradTree -> Float
getValue Empty = 0
getValue (Value value _) = value
getValue (Operation (Value value _) _ _ _) = value

getGrad :: GradTree -> Float
getGrad Empty = 0
getGrad (Value _ grad) = grad
getGrad (Operation (Value _ grad ) _ _ _) = grad

setValue :: Float -> GradTree -> GradTree
setValue value Empty = Empty
setValue value (Value _ grad) = Value value grad
setValue value (Operation (Value _ grad) op a b) = Operation (Value value grad) op a b

setGrad :: Float -> GradTree -> GradTree
setGrad value Empty = Empty
setGrad grad (Value value _) = Value value grad
setGrad grad (Operation (Value value _) op a b) = Operation (Value value grad) op a b

addGrad :: Float -> GradTree -> GradTree
addGrad value Empty = Empty
addGrad newGrad (Value value grad) = Value value (newGrad+grad)
addGrad newGrad (Operation (Value value grad) op a b) = Operation (Value value (newGrad+grad)) op a b

resetAllGrads :: GradTree -> GradTree
resetAllGrads Empty = Empty
resetAllGrads (Value value grad) = Value value 0
resetAllGrads (Operation (Value value grad) Input a b) = Operation (Value value grad) Input a b
resetAllGrads (Operation (Value value grad) op a b) = Operation (Value value 0) op (resetAllGrads a) (resetAllGrads b)

-- Backpropagation
--
--

backward :: GradTree -> GradTree
backward Empty = Empty
backward (Value x xg) = Value x xg
backward (Operation out Add a b) = Operation out Add (backward $ (addGrad (backAdd out) a)) (backward $ (addGrad (backAdd out) b))
backward (Operation out Multiply a b) = Operation out Multiply (backward $ (addGrad (backMultiply out b) a)) (backward $ (addGrad (backMultiply out a) b))
backward (Operation out Pow a b) = Operation out Pow (backward $ (addGrad (backPow out a b) a)) (backward $ (addGrad (backPow out b a) b))
backward (Operation out Relu a b) = Operation out Relu (backward $ (addGrad (backRelu out) a)) b
backward (Operation out Constant a b) = Operation out Constant (backward $ (addGrad (backConstant out) a)) b
backward (Operation out Input a b) = Operation out Input a b
backward (Operation out Mse a b) = Operation (setGrad 0 out) Mse (backward $ (addGrad (backMse b a) a)) (backward $ (addGrad (backMse a b) b))
backward (Operation out Mae a b) = Operation (setGrad 0 out) Mae (backward $ (addGrad (backMae b a) a)) (backward $ (addGrad (backMae a b) b))

backConstant :: GradTree -> Float
backConstant out = 0

backMse :: GradTree -> GradTree -> Float
backMse a b = -((getValue a) - (getValue b))*2

backMae :: GradTree -> GradTree -> Float
backMae a b = -((getValue a) - (getValue b))

backAdd :: GradTree -> Float
backAdd out = (getGrad out)

backMultiply :: GradTree -> GradTree -> Float
backMultiply out value = (getValue value) * (getGrad out)

backPow :: GradTree -> GradTree -> GradTree -> Float
backPow out a b = (vb * va**(vb-1))*go
  where va = getValue a
        vb = getValue b
        go = getGrad out

backRelu :: GradTree -> Float
backRelu out = (getGrad out) * (fromIntegral $ fromEnum ((getValue out) > 0.0))

-- Update weights
--
--

subtractGradients :: Float -> GradTree -> GradTree
subtractGradients learningRate (Value x xg) = Value (x-learningRate*xg) xg -- subtracting learningRate*grad from weights to get closer to minima
subtractGradients learningRate (Operation out Constant a b) = Operation out Constant a b
subtractGradients learningRate (Operation out Input a b) = Operation out Input a b
subtractGradients learningRate (Operation out op a b) = Operation out op (subtractGradients learningRate a) (subtractGradients learningRate b)
subtractGradients learningRate Empty = Empty


-- Forward Propagation
--
--

forward :: GradTree -> GradTree
forward Empty = Empty
forward (Operation out Input a b) = Operation out Input a b
forward (Operation out op a b) = Operation (setGrad (getGrad out) $ getOperationResult $ newOperation op ua ub) op ua ub
  where ua = forward a
        ub = forward b
forward value = value

-- OptimizationPipeline
--
--

singleIterationOptimizationPipeline :: Float -> GradTree -> GradTree
singleIterationOptimizationPipeline learningRate tree = resetAllGrads $ forward $ subtractGradients learningRate $ backward tree

iterateOptimization :: GradTree -> Float -> Int -> GradTree
iterateOptimization tree learningRate iterations = if iterations > 0 then (iterateOptimization (singleIterationOptimizationPipeline learningRate tree) learningRate (iterations-1)) else tree 

