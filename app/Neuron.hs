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

trainBatch :: GradTree -> Float -> [[Float]] -> GradTree
trainBatch model learningRate [] = resetAllGrads $ subtractGradients learningRate model
trainBatch model learningRate batch = trainBatch (backward $ forward $ insertInputs (head batch) model) learningRate (tail batch)

runPrediction :: GradTree -> [Float] -> Float
runPrediction model input = getOutput $ insertInputs input model

