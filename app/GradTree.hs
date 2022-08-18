module GradTree where

data OperationType = Input | Constant | Add | Multiply | Relu | Sigmoid | Output | Diff deriving (Show, Eq)
data GradTree = Empty | Value Float Float | Operation GradTree OperationType GradTree GradTree deriving Show

pretty :: GradTree -> String
pretty (Value a b) = " V(" ++ show a ++","++ show b ++") "
pretty (Operation (Value a b) Constant _ _) = " C("++ show a ++") "
pretty (Operation (Value a b) Input _ _) = " I(" ++ show a ++","++ show b ++") "
pretty (Operation (Value a b) Multiply x y) = "  ("++ pretty x ++" Multiply(" ++ show a ++","++ show b ++") "++ pretty y ++")  "
pretty (Operation (Value a b) Add x y) = "  ("++ pretty x ++" Add(" ++ show a ++","++ show b ++") "++ pretty y ++")  "
pretty (Operation (Value a b) Relu x y) = " (Relu(" ++ show a ++","++ show b ++") "++ pretty x ++")  "
pretty (Operation (Value a b) Diff x y) = "  ("++ pretty x ++" Diff(" ++ show a ++","++ show b ++") "++ pretty y ++")  "

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
newValue a = Value a 1

newActivation :: OperationType -> GradTree -> GradTree
newActivation Relu a = newOperation Relu a Empty
newActivation Sigmoid a = newOperation Sigmoid a Empty

newOperation :: OperationType -> GradTree -> GradTree -> GradTree
newOperation Add a b = Operation (Value (va+vb) 1) Add a b
  where va = getValue a
        vb = getValue b
newOperation Multiply a b = Operation (Value (va*vb) 1) Multiply a b
  where va = getValue a
        vb = getValue b
newOperation Relu a Empty = Operation (Value (if va >= 0 then va else 0) 1) Relu a Empty
  where va = getValue a
newOperation Sigmoid a Empty = Operation (Value (1/(1+2.718281**(-va))) 1) Sigmoid a Empty
  where va = getValue a
newOperation Constant a b = Operation a Constant a Empty
newOperation Diff a b = Operation (Value (dif**2) (-dif*2)) Diff a b
  where va = getValue a
        vb = getValue b
        dif = (va-vb)
newOperation Input a b = Operation (Value 1 id) Input Empty Empty
  where id = getGrad a

setInput :: Int -> Float -> GradTree -> GradTree
setInput id value (Operation tree op a b) = 
  if op == Input && id == (round $ getGrad tree) 
    then Operation (setValue value tree) op (setInput id value a) (setInput id value b)
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

getBoth :: GradTree -> GradTree 
getBoth (Operation a _ _ _) = a
getBoth a = a

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

-- Backpropagation
--
--

backward :: GradTree -> GradTree
backward Empty = Empty
backward (Value x xg) = Value x xg
backward (Operation out Add a b) = Operation out Add (backward $ (setGrad (backAdd out) a)) (backward $ (setGrad (backAdd out) b))
backward (Operation out Multiply a b) = Operation out Multiply (backward $ (setGrad (backMultiply out b) a)) (backward $ (setGrad (backMultiply out a) b))
backward (Operation out Relu a b) = Operation out Relu (backward $ (setGrad (backRelu out) a)) b
backward (Operation out Constant a b) = Operation out Constant (backward $ (setGrad (backConstant out) a)) b
backward (Operation out Input a b) = Operation out Input a b
backward (Operation out Diff a b) = Operation (setGrad 0 out) Diff (backward $ (setGrad (backMse b a) a)) (backward $ (setGrad (backMse a b) b))

backConstant :: GradTree -> Float
backConstant out = 0

backMse :: GradTree -> GradTree -> Float
backMse a b = -((getValue a) - (getValue b))*2

backAdd :: GradTree -> Float
backAdd out = (getGrad out)

backMultiply :: GradTree -> GradTree -> Float
backMultiply out value = (getValue value) * (getGrad out)

backRelu :: GradTree -> Float
backRelu out = (getGrad out) * (fromIntegral $ fromEnum ((getValue out) > 0.0))

-- Update weights
--
--

updateValues :: Float -> GradTree -> GradTree
updateValues learningRate (Value x xg) = Value (x-learningRate*xg) xg -- subtracting learningRate*grad from weights to get closer to minima
updateValues learningRate (Operation out Constant a b) = Operation out Constant a b
updateValues learningRate (Operation out op a b) = Operation out op (updateValues learningRate a) (updateValues learningRate b)
updateValues learningRate Empty = Empty


-- Forward Propagation
--
--

forward :: GradTree -> GradTree
forward Empty = Empty
forward (Operation out Input a b) = Operation out Input a b
forward (Operation out op a b) = Operation (getBoth $ newOperation op ua ub) op ua ub
  where ua = forward a
        ub = forward b
forward value = value

-- OptimizationPipeline
--
--

optimizationPipeline :: Float -> GradTree -> GradTree
optimizationPipeline learningRate tree = forward $ updateValues learningRate $ backward tree

iterateOptimization :: GradTree -> Float -> Int -> GradTree
iterateOptimization tree learningRate iterations = if iterations > 0 then (iterateOptimization (optimizationPipeline learningRate tree) learningRate (iterations-1)) else tree 

