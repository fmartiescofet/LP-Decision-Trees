# Haskell Decision Tree for Mushroom Dataset

Project of Programming Languages subject in Barcelona School of Informatics, FIB - UPC.
Fall 2020-2021.

This project consists of an implementation of a decision tree for the [Mushroom Dataset](https://archive.ics.uci.edu/ml/datasets/Mushroom) to decide if a mushroom is *edible* or *poisonous*.
The data used for the decision tree construction is in `agaricus-lepiota.data`and information about the meaning of each attribute can be found in `agaricus-lepiota.names`.

## Getting started
To run this program you only need `ghc` installed. You can check [here](https://www.haskell.org/platform/) for installation instructions.

To compile the code:
```bash
ghc dts.hs
```

To run the code:
```bash
./dts
```

## About the implementation
The decision tree is implemented as an algebraic `data` type which can either be a `Node String [(Char,DTree)]` which contains the decision attribute and all the possibilities or a `Leaf String` which contains if the mushroom is *edible* or *poisonous*.

I've decided to work with the datset as a matrix where each row is an attribute and each column is a mushroom so it's easier to deal with it.

The used algorithm to build the decision tree is specified in the following document:

Gerard Escudero, 2020. [Machine Learning(p.35-40)](https://gebakx.github.io/ml/#35)

If there is a tie of the accuracy between different attributes I choose the one which classifies more examples in poisonous or edible by using the `computeNFiltered` function, and if there's still a tie I choose the one with bigger index.

As we can see there are missing values in the *stalk-root* attribute that are "?" in the input file. We're going to deal with them as if "?" was another possible value of the attribute.

All used functions are documented in the code.
## Author
- [Francesc Martí Escofet](https://github.com/fmartiescofet)