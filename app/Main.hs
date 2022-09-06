module Main where

import Neuron
import GradTree
import GHC.Float
import System.IO.Unsafe
import Data.List

main :: IO ()
main = putStrLn ""


-- Example 1
-- Reducing mean absolute error
--

var2 = newValue 10
var3 = newValue 0
ex1 = newOperation AbsoluteError var2 var3

optimized1 = iterateOptimization ex1 0.01 90

-- Example 2
-- Single input neuron
--

ds4 = map double2Float [0.0, 0.1, 0.0, 0.1, 0.1, 0.0, 1.0, 0.9, 1.0, 0.9, 0.9, 1.0]
ds5 = map double2Float [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

dataset1d = transpose [ds4, ds5]

input0 = newInput 0
var0 = newValue 0.1 -- should converge to -1
var1 = newValue 0.0 -- should converge to 1
mul0 = newOperation Multiply input0 var0
add0 = newOperation Add mul0 var1
relu0 = newActivation Relu add0
input1 = newInput 1
ae = newOperation AbsoluteError input1 relu0


