# Recursive grads

Implementation of gradient optmization micro framework in haskell

The framwork uses binary tree for the operations, and performs forward
and backprop recursively.

Example:
```haskell

dataset = map double2Float [0.0, 0.1, 0.0, 0.1, 0.1, 0.0, 1.0, 0.9, 1.0, 0.9, 0.9, 1.0]
labels = map double2Float [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

dataset1d = transpose [ds4, ds5]

-- Buiding a neuron

input0 = newInput 0
var0 = newValue 0.1 -- should converge to -1
var1 = newValue 0.0 -- should converge to 1
mul0 = newOperation Multiply input0 var0
add0 = newOperation Add mul0 var1
relu0 = newActivation Relu add0
input1 = newInput 1
ae = newOperation AbsoluteError input1 relu0

-- Training model
-- the train epochs function automaticaly inserts inputs and labels
-- then executes optimization pipelines

trainedModel = trainEpochs ae 0.01 dataset1d 100

runPrediction trainedModel [0.8]
>0.20757

runPrediction trainedModel [0.1]
>0.83773
```



