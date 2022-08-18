# haskell-micrograd
implementation of karpathy/micrograd in haskell

to do
Fix names:
Diff to mse

Implement:
cross entropy
backwards of sigmoid
matrix operations

Write:
readme

Functional implementation of a gradient optimization framework

The objective of this project is implement a functional framework for gradient optimization,
and be able to easily train a model with it.



## Structure

### Binary Trees

Binary trees is the perfect structure for this project, for the folowing reasons:
> Values can be accessed using recursive functions.
> All the operations will be applied to two or less values.

```
data GradTree = Empty | Value Float Float | Operation GradTree OperationType GradTree GradTree deriving Show
```

### Leaves

```
Empty | Value Float Float
```

A leaf of the structure can be Empty or Value Float Float, the first Float represents the value of the leaf,
and the second Float represents the gradient for this value.

Nodes

The objective of each node is to perform a operation between two leaves.

Operation GradTree OperationType GradTree GradTre

The first GradTree will always hold a (Value Float Float) inside, which represents the result of
second and third GradTree's 

OperationType

```
data OperationType = Input | Constant | Add | Multiply | Relu | Sigmoid | Output | Mse deriving (Show, Eq)
```

Each operation type will dictate how the values will flow between the nodes either forward or backward propagation.


Lets take a look at this example:

> var1 = newValue 5
> var2 = newValue 2
> op1 = newOperation Add var1 var2

(insert image here)

two values were created and op1 holds the result of the Add operation.

Why Input and Constant are OperationTypes?

Instead of creating another type for constant and input, is much easier to create an operation that block the propagation
of the gradient to the leaf which belongs to it.

Relu and Sigmoid

Those operations can't take more than a single input so, when you create an operation with ReLU, the leaves will be, your
input and another Empty leaf.


