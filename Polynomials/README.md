# Polynomials

## Problem Statement
Write a module that defines an algebraic data type of polynomials and implements various functions on
the members of the type including symbolic differentiation.

### Background
_What do you call a ring theorist's parrot when it hasn't been fed?_
_Polynomial!_ (Poly-"no meal")

Polynomials can be defined as: _a<sub>0</sub> + a<sub>1</sub> * x<sup>2</sup> + ... + a<sub>m</sub> * x<sup>m</sup>_.

But sometimes its easier with your own data type! To that end this module implements it's own definition and definitions to work over polynomials.

### Modules
Included are two modules:
* Assign_3 deals with a traditional representation of polynomials
* Assign_3_ExtraCredit deals with a more functional representation of polynomials

Functions over polynomials included are:
* Evaluation 
* Degree
* Symbolic differentiation
* Product & Summation
* Conversion between types

### Testing
Included at the end of the file are test cases on various input and output of the program. 