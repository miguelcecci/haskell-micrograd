module Main where

import Neuron
import GradTree
import GHC.Float
import System.IO.Unsafe
import Data.List

main :: IO ()
main = putStrLn "Hello, Haskell!"


--tests --------

-- Example 1
-- Reducing mean absolute error
--

var2 = newValue 10
var3 = newValue 0
ex1 = newOperation Mae var2 var3

optimized1 = iterateOptimization ex1 0.01 20

-- Example 2
-- Reducing Mean squared error on a neuron with fixed inputs
-- to do
-- ...

-- Example 3
-- Single input neuron
--

ds4 = map double2Float [0.0, 0.1, 0.0, 0.1, 0.1, 0.0, 1.0, 0.9, 1.0, 0.9, 0.9, 1.0]
ds5 = map double2Float [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

dataset1d = transpose [ds4, ds5]

input0 = newInput 0
var0 = newValue 0.5 -- should converge to -1
var1 = newValue 0.7 -- should converge to 1
mul0 = newOperation Multiply input0 var0
add0 = newOperation Add mul0 var1
input1 = newInput 1
mse = newOperation Mae input1 add0


-- Example 4
-- 2d Perceptron
--
ds1 = map double2Float [0.6, 0.7, 0.8, 0.9, 0.2, 0.3, 0.4, 0.1]
ds2 = map double2Float [0.9, 0.8, 0.8, 0.5, 0.4, 0.2, 0.2, 0.2]
ds3 = map double2Float [0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0]

dataset = transpose [ds1, ds2, ds3]

const_neuron = newConstant 1
neuron_test = newNeuron const_neuron
const_output = newConstant 0
test_diff = newOperation Mse const_output neuron_test

-- Example 5
-- 
