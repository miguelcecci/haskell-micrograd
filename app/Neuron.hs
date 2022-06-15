module Neuron where

import GradTree
import System.Random
import System.IO.Unsafe

drawDouble :: Double -> Double  -> Double
drawDouble x y = unsafePerformIO $ getStdRandom (randomR (x,y))

newNeuron :: GradTree -> GradTree
newNeuron input = newOperation Relu (newOperation Add (newOperation Multiply input (newValue $ realToFrac $ drawDouble 0.0 1.0)) (newValue $ realToFrac $ drawDouble 0.0 1.0)) Empty

new2dPerceptron = 
  newOperation Diff (newInput 2) $ newOperation Add neuron1 neuron1
  where neuron1 = newNeuron (newInput 0)
        neuron2 = newNeuron (newInput 1)
