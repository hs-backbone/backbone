# backbone-linear
A backbone for all your linear, numerical computations based on [numhask](https://github.com/tonyday567/numhask)! Some classes were adapted from the great [subhask-library](https://github.com/mikeizbicki/subhask), but most were rewritten.

Linear focuses on a more rigorous and flexible approach to linear algebra. Linear provides, for example, the ability to abstract over a Vector-Space with some metric, some basis etc.

Another important aim of linear is to be completely "representation-polymorphic", so that you could write your code once and later run it nativly, on the gpu or in specialised frameworks like hasktorch.