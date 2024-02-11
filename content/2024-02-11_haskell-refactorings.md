+++
title = "Haskell refactorings"
date = 2024-02-11
draft = false
path = "2024-02/haskell-refactorings"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "design", "haskell"]
+++

I was looking through [Martin Fowler's refactorings list](https://refactoring.com/catalog/)
which is heavily OOP/Imperative-oriented.

There are currently 66 refactorings:

* 27 are focused on local rearrangement (locally move piece of code, renaming)
* 4 are focused on pure encapsulation
* 35 are focused on responsibilities distribution amongst OOP concepts

Note: responsibilities distribution is often involving encapsulation, but I have
done my best to make a distinction between refactorings which are more agnostic
and those working through OOP concepts.

If I try to summarize which refactorings I use, there are far less.

* Rename (also called [Alpha conversion](https://wiki.haskell.org/Alpha_conversion))

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie = pour milk . runBlender . map (cut . peel)
```

Becomes

```haskell
mkSmoothie :: [Fruit] -> Smoothie
mkSmoothie = pour milk . runBlender . map (cut . peel)
```

* Extract binding

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie = pour milk . runBlender . map (cut . peel)
```

Becomes

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie = pour milk . runBlender . map prepareFruit
  where prepareFruit = cut . peel
```

* Inline binding (also called [Beta reduction](https://wiki.haskell.org/Beta_reduction))

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie = pour milk . runBlender . map prepareFruit
  where prepareFruit = cut . peel
```

Becomes

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie = pour milk . runBlender . map (cut . peel)
```

* [Eta abstraction](https://wiki.haskell.org/Eta_conversion) / uncurry / make last parameter explicit

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie = pour milk . runBlender . map (cut . peel)
```

Becomes

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie fruits = pour milk $ runBlender $ map (cut . peel) fruits
```

* [Eta reduction](https://wiki.haskell.org/Eta_conversion) / curry / make last parameter implicit

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie fruits = pour milk $ runBlender $ map (cut . peel) fruits
```

Becomes

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie = pour milk . runBlender . map (cut . peel)
```

* Parameterizing

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie = pour milk . runBlender . map (cut . peel)
```

Becomes

```haskell
smoothie :: Liquid -> [Fruit] -> Smoothie
smoothie liquid = pour liquid . runBlender . map (cut . peel)
```

Alternatively, we can keep the function as a default implementation:

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie = smoothieWith liquid

smoothieWith :: Liquid -> [Fruit] -> Smoothie
smoothieWith liquid = pour liquid . runBlender . map (cut . peel)
```

* [Parameteric](https://wiki.haskell.org/Polymorphism#Parametric_polymorphism) "polymorphization"

```haskell
smoothie :: [Fruit] -> Smoothie
smoothie = pour milk . runBlender . map (cut . peel)
```

Becomes

```haskell
smoothie :: ([Slice] -> a) -> [Fruit] -> a
smoothie mix = mix . map (cut . peel)
```

That roughly 7 refactorings for local rearrangements, and these are probably
90%-95% of the refactorings I use daily (and I use them a lot).

Sure, there are few others at type-level but despite that, there are far less
than in the OOP ecosystem.

Which does not seem intuitive to me as Haskell (at least GHC's version) is a
quite large programming language compared to other OOP programming languages.
I guess FP simpler core plays a role in that.
