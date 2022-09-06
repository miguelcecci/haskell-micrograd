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
  newOperation SquaredError (newInput 2) $ newOperation Add neuron1 neuron2
  where neuron1 = newNeuron (newInput 0)
        neuron2 = newNeuron (newInput 1)

-- wrong
-- trainBatch :: GradTree -> Float -> [[Float]] -> GradTree
-- trainBatch model learningRate [] = resetAllGrads $ subtractGradients learningRate model
-- trainBatch model learningRate batch = trainBatch (backward $ forward $ insertInputs (head batch) model) learningRate (tail batch)

runPrediction :: GradTree -> [Float] -> Float
runPrediction model input = getOutput $ insertInputs input model


-- to do
-- batchGradientDescent :: GradTree -> [[Float]] -> Float -> GradTree
-- batchGradientDescent model batch learningRate = subtractGradients learningRate $ backward $ (insertError model (calculateMeanBatchError model batch) )

-- check model: to do
-- verify if model is a neural net
-- verify if model have error function in the end

calculateMeanBatchError :: GradTree -> [[Float]] -> Float
calculateMeanBatchError model batch = (calculateMeanBatchError' model batch 0.0) / (fromIntegral $ length batch)

calculateMeanBatchError' :: GradTree -> [[Float]] -> Float -> Float
calculateMeanBatchError' model [] error = error
calculateMeanBatchError' model batch error = calculateMeanBatchError' model (tail batch) ((getError $ forward $ insertInputs (head batch) model) + error)

-- minibatchGradientDescent: to do

stochasticGradientDescentOptimization :: GradTree -> Float -> [[Float]] -> GradTree
stochasticGradientDescentOptimization model learningRate [] = resetAllGrads $ subtractGradients learningRate model
stochasticGradientDescentOptimization model learningRate dataset = stochasticGradientDescentOptimization (singleIterationOptimizationPipeline learningRate $ insertInputs (head dataset) model) learningRate (tail dataset)

trainEpochs :: GradTree -> Float -> [[Float]] -> Int -> GradTree
trainEpochs model learningRate dataset 0 = model
trainEpochs model learningRate dataset epochs = trainEpochs (stochasticGradientDescentOptimization model learningRate dataset) learningRate dataset (epochs-1)


