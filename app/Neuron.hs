module Neuron where

import GradTree
import System.Random
import GHC.Float
import System.IO.Unsafe
import Data.List

drawDouble :: Double -> Double  -> Double
drawDouble x y = unsafePerformIO $ getStdRandom (randomR (x,y))

newNeuron :: GradTree -> GradTree
newNeuron input = newOperation Relu (newOperation Add (newOperation Multiply input (newValue $ realToFrac $ drawDouble 0.0 1.0)) (newValue $ realToFrac $ drawDouble 0.0 1.0)) Empty

new2dPerceptron :: GradTree
new2dPerceptron = 
  newOperation Mse (newInput 2) $ newOperation Add neuron1 neuron2
  where neuron1 = newNeuron (newInput 0)
        neuron2 = newNeuron (newInput 1)

--wrong
--trainEpoch :: GradTree -> Float -> [[Float]] -> GradTree
--trainEpoch model learningRate dataset = if (length dataset) == 0 then trainEpoch (iterateOptimization (insertInputs (head dataset) model) learningRate 1) learningRate (tail dataset) else model

runPrediction :: GradTree -> [Float] -> Float
runPrediction model input = getOutput $ insertInputs input model

--tests --------

ds1 = map double2Float [0.6, 0.7, 0.8, 0.9, 0.2, 0.3, 0.4, 0.1]
ds2 = map double2Float [0.9, 0.8, 0.8, 0.5, 0.4, 0.2, 0.2, 0.2]
ds3 = map double2Float [0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0]

dataset = transpose [ds1, ds2, ds3]

const_neuron = newConstant 1
neuron_test = newNeuron const_neuron
const_output = newConstant 0
test_diff = newOperation Mse const_output neuron_test

-- test run prediction ---
--

ds4 = map double2Float [0.0, 0.1, 0.0, 0.1, 0.1, 0.0, 1.0, 0.9, 1.0, 0.9, 0.9, 1.0]
ds5 = map double2Float [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

dataset1d = transpose [ds4, ds5]

input0 = newInput 0
var0 = newValue 0.5
var1 = newValue 0.7
mul0 = newOperation Multiply input0 var0
add0 = newOperation Add mul0 var1
input1 = newInput 1
mse = newOperation Mse input1 add0


