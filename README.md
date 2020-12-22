<p align="center">
  <img width="150" height="150" src='./logo.png'>
</p>

[![Build Status](https://travis-ci.org/FP-BAUM/nuflo.svg?branch=master)](https://travis-ci.org/FP-BAUM/nuflo)
# Ñuflo language (~)

## Introduction

**Ñuflo** (~) is an interpreted functional-logic programming language build as part of a research work to study the properties of languages combine features from the logic and functional paradigms. **Ñuflo** is based on the **λU-Calculus**, a calculus extends the well know **λ-Calculus** with terms from the logic paradigm, like `non deterministic choice`, `unificacion` and `fresh variable introduction`.

To deepen more about **λU-Calculus** you can read our [paper](./doc/paper-ictac2020.pdf) where we introduce the **λU-Calculus** terms, his current small step semantic, a small type system and current research of denotational semantic.

## Overview

The **Ñuflo** programming language is has a stronge influence by know languages as [Haskell](https://www.haskell.org/), [Prolog](https://www.swi-prolog.org/) and [Agda](https://agda.readthedocs.io/en/v2.6.0.1/index.html). Counting with features like `mixfix operators`, `unification operator`, `non determisnistic choice operator`, `high order`, `functions as relations` between others. Moreover, **Ñuflo** has a `Hindley–Milner` type system with polimorfism and type classes.

To deepen more about the specification of **Ñuflo** programming language you can read the section `Implementation` of this [report]() (**NOTE**: this report is in spanish) where is explained each part of the language implementation.

**NOTE**: In the future, we are going to move all the document details to different sections in the wiki page.


## Mixfix operators

How was mentioned before, **Ñuflo** is inspired in part from **Adga**, where we take the concept of `mixfix operators`. The way to define mixfix operators in **Ñuflo** is at follow:

```(Haskell)
infixr 50 _(^)_ : * -> * -> *
```

Where the operator **(^)** receive two params and return one, in this case  how  the keyword used was `infixr` means the operator is associative to **right** and the number **50** represent the precedence level. Other examples could be:

```(Haskell)

infixl 30 _(-)_ : * -> * -> *    -- with left association
infix  20 if_then_else_          -- the if then else operator
```

Afther that you can define the semantic of this operators as folllow:

```(Haskell)
infix 20 _::_

x = case list of
    NIL       -> True
    (x :: xs) -> False
```

## Unificacion operator

The unification operator in **Ñuflo** is represented by the symbol (**~**) doing honor to the language name. The unification operator is defined as:

```(Haskell)
infixr 80 _~_ : a -> a -> ()
```

where takes two params and produces an effect on the state of his arguments, trying unificate them and introducing new restrictions about the possible values. Finally, if the unification success, the operator are going to instantiate the arguments to have the same value. For example:

```(Haskell)
(S (S Z) + x) ~ S (S (S Z))
```

where the unification instantiates the variable `x` as `(S Z)`.

## Alternative operator

Equals to the unification operator, the alternative is built as a mixfix operator:

```(Haskell)
infixr 60 _|_ : a -> a -> a
```

for example, we define the function `coin` as follow:

```(Haskell)
coin () = S Z | S (S Z)
```

where the function `coin` can returns both `S Z` and `S (S Z)`. The internal **Ñuflo** engine explores whole the possible values for a program. In this case, run the program:

```(Haskell)
main () = print (coin () + coin ()) end
```

will have the next output:

```(Haskell)
S (S Z)         -- (1 + 1) = 2
S (S (S Z))     -- (1 + 2) = 3
S (S (S Z))     -- (2 + 1) = 3
S (S (S (S Z))) -- (2 + 2) = 4
```

## Pattern matching

Similar to **Haskell**, the language **Ñuflo** supports pattern matching feature. The principal distincion with Haskell is that, using the internal unification meachanism, Ñuflo also allows use the pattern matching concept apply it to functions, for example:

```(Haskell)
prefix (xs ++ ys) = xs
```

note that the function `prefix` is defined using pattern matching over the function `concat` (**++**). Other example could be:

```(Haskell)
last (_ ++ (x : [])) = x
```

where we define the function `last` doing pattern matching over the function `concat` and `cons`.

## Type system

The type system of **Ñuflo** is a version of the Hindley–Milner type system extended with polymorfism and typeclasses, inspired on the Haskell type system.

Before to defined the mixfix operators, was used the kind type annotation:

```(Haskell)
f : * -> * -> *
```
that allow us specify the type of the types.

For other hand, is possible to define any type definition:
```(Haskell)

_++_ : List a -> List a -> List a    -- Simple function signature

type Id = String                     -- Type alias signature

data Bool where                      -- Data type signature
  True : Bool
  False : Bool

class Eq a where                     -- Class type signature
  == : a -> a -> Bool

instance Eq Bool where               -- Instance type signature
  == True True   = True
  == False False = True
  == a b         = False
 
```

## IO types

To execute programs in **Ñuflo** is necessary to return a type IO, the type IO is build as follow:

```(Haskell)
data IO where
  end : IO                        -- Ends the IO function call
  print : a -> IO -> IO           -- Shows an output value on the screen with his respective type format
  put : String -> IO -> IO        -- Shows an output string on the screen
  get : (String -> IO) -> IO      -- Waits for an IO input string value
  getChar : (Char -> IO) -> IO    -- Waits for an IO input char value
  getLine : (String -> IO) -> IO  -- Waits for an IO input string line value
```

For example, the next expression get an string input, captures the input and print the value on the screen:

```(Haskell)
put “Ingress a string”
  get (\ str -> put str end)
end
```

## Examples

To see other examples, you can go to the folder `./doc/examples/`.
### Some Nat operations
```
data Nat where
  Z : Nat
  S  : Nat -> Nat

_+_ : Nat -> Nat -> Nat
Z + n = n
S n + m = S (n + m)

_-_ : Nat -> Nat -> Nat
x - y = fresh z in
              (y + z) ~ x
              & z

coin : () -> Nat
coin () = S Z | S (S Z)

main () = print (coin () + coin ()) end
```

### Some List operations

```(Haskell)
_++_ : List a -> List a -> List a
[] ++ ys       = ys
(x : xs) ++ ys = x : (xs ++ ys)

prefix : List a -> List a
prefix (xs ++ ys) = xs

last : List a -> a
last (_ ++ (x : [])) = x

foldr : (a -> b -> b) -> b -> List a -> b
foldr _ z []       = z
foldr f z (x : xs) = f x (foldr f z xs)

inter : a -> List a -> List a
inter x []       = x : []
inter x (y : ys) = x : (y : ys)
inter x (y : ys) = y : inter x ys

perm : List a -> List a
perm xs = foldr inter [] xs

main () = print (perm (1 : 2 : 3 : 4 : [])) end
```

### Relational examples
```(Haskell)
data Simpson where
  Abe    : Simpson
  Homero : Simpson
  Bart   : Simpson
  Lisa   : Simpson
  Maggie : Simpson

father : Simpson -> Simpson                      -- Pattern matching
father Homero = Abe
father Bart   = Homero
father Lisa   = Homero
father Maggie = Homero
-- father (Bart | Lisa | Maggie) = Homero        -- Optional

_∘_ f g x = f (g x)                              -- Functions composition

child (father x) = x                             -- Funtioncal pattern matching

inv : (a -> b) -> b -> a                         -- Inverse function
inv f (f x) = x                                  -- Using pattern matching

main () = print (inv (father ∘ father) Abe) end  -- Print each grandchild of Abe
```

### Simple type inference system
```(Haskell)
data Id where
  Z : Id
  S  : Id -> Id

data Type where
  BOOL : Type
  _=>_ : Type -> Type -> Type

data Ctx where
  Ec : Ctx
  _,_ : Ctx -> Type -> Ctx

data Term where
  var : Id -> Term
  lam : Term -> Term
  app : Term -> Term -> Term

_Ê_::_ : Ctx -> Id -> Type -> ()
(gamma , A) Ê Z   :: A = ()
(gamma , A) Ê S x :: B = gamma Ê x :: B

_|-_::_ : Ctx -> Term -> Type -> ()
gamma |- var x   :: A = gamma Ê x :: A
gamma |- lam t   :: (A => B) = (gamma , A) |- t :: B
gamma |- app t s :: B = fresh A in
                          gamma |- t :: (A => B)
                        & gamma |- s :: A

main () = print (
                fresh A in
                  (Ec |- lam (var Z) :: A)
                & A
                ) end

```

## Setup
First you need have `ghc` installed:

### Mac OS
```bash
brew install ghc
``` 

## Build
To generates a binary `nuflo`:
```bash
cd .../nuflo/

make
```

## Run tests
```bash
make test
```

## Clean
To remove auxiliary and binary files:
```bash
make clean
```

## Development guide

### Add new test cases:
  To do this, you need:
  - Go to `.../test/TestMain.hs`
  - Import your new test file
  - Add it to `runAllTests` definition
