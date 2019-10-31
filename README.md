# ad-lens
These blog posts try to explain a bit about what is going on here

http://www.philipzucker.com/reverse-mode-auto-differentiation-kind-like-lens/

http://www.philipzucker.com/reverse-mode-differentiation-is-kind-of-like-a-lens-ii/

http://www.philipzucker.com/bidirectional-applicative-programming-and-automatic-differentation/

http://www.philipzucker.com/neural-networks-with-weighty-lenses-dioptics/

Check out the test directory for some usage examples

A lens is a getter and setter pair ```a -> (b, (b->a))```. This is also the signature of reverse mode automatic differentiation. The function ```(b->a)``` is the transposed Jacobian (using a function representation of Matrices. Matrices are a certain kind of Vector -> Vector functions).

We use the standard Lens trick to make these functions composable via ```(.)```.


TODO:
Higher order derivatives
ReExport safe pieces of standard Lens library
Try Accelerate GPU lib
More Combinators
Recursion Schemes

# ad-lens
