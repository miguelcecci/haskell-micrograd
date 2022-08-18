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
  newOperation Diff (newInput 2) $ newOperation Add neuron1 neuron2
  where neuron1 = newNeuron (newInput 0)
        neuron2 = newNeuron (newInput 1)

--trainEpoch :: GradTree -> Float -> [[Float]]
trainEpoch model learningRate dataset = if (length dataset) == 0 then trainEpoch (iterateOptimization (insertInputs (head dataset) model) learningRate 1) learningRate (tail dataset) else model

--tests --------

ds1 = map double2Float [0.6, 0.7, 0.8, 0.9, 0.2, 0.3, 0.4, 0.1]
ds2 = map double2Float [0.9, 0.8, 0.8, 0.5, 0.4, 0.2, 0.2, 0.2]
ds3 = map double2Float [0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0]

dataset = transpose [ds1, ds2, ds3]

const_neuron = newConstant 1
neuron_test = newNeuron const_neuron
const_output = newConstant 0
test_diff = newOperation Diff const_output neuron_test
