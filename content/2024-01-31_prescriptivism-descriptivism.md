+++
title = "Prescriptivism and Descriptivism"
date = 2024-01-31
draft = false
path = "2024-01/prescriptivism-descriptivism"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "design", "haskell", "functional programming", "object-oriented programming"]
+++

A long time ago I was watching another version of [Romeu Moura's Prescriptivism vs Descriptivism](https://www.youtube.com/watch?v=rljUZcqzfq8).

The idea is illustrated by the point of view you can have on a "list":

- Prescriptive: as things should be (as in a specification)
- Descriptive: as things are (or at least, how things are observed)

Sometimes, in Haskell tutorials you have many references to [Category Theory](https://en.wikipedia.org/wiki/Category_theory),
they are often compared to [Design Patterns](https://en.wikipedia.org/wiki/Software_design_pattern)
and beginners report that they are way harder to grasp.

The thing is, whenever your define some types, you start by defining the datatype
_then_ you define the _type class_ to characterize its property:

```haskell
data AvlTree a = Node (AvlTree a) a (AvlTree a) | Leaf

instance Ord a => Semigroup (AvlTree a) where
  (<>) = mergeAndBalance

instance Monoid (AvlTree a) where
  mempty = Leaf
```

We can say that _Category Theory_ is _descriptive_.

On another hand, when it comes to object-oriented programming, Design Patterns are
_the way to go_.

The thing is, most of the time, they are implemented to solve a technical issue,
they are implemented to be implemented.

Usually, they are named in order to reflect that:

```java
interface IOrderFactory {
    Order startOrder(Item firstItem);
}
```

or tagged:

```java
@Factory
interface IOrderCreator {
    Order startOrder(Item firstItem);
}
```

Design Patterns in (OOP at least) are _prescriptive_.

Design Patterns exist in OOP to solve a lack of [Higher-order constructions](https://en.wikipedia.org/wiki/Higher-order_function).

Another form of _descriptive_ constructions are duck typing (at runtime) and
structural typing (at compile time).

The duck typing in ruby:

```ruby
class Glass
    def refill
      # ...
    end
end

class Car
    def refill
      # ...
    end
end

def gas_station(x)
  x.refill
end
```

You can refill your `Glass` in a gas station, but petrol is only good for some
cars.

The structural typing in go:

```go
type Glass struct {}
type Car struct {}

type Refillable interface {
    Refill()
}
func (g Glass) Refill() {}
func (c Car) Refill() {}

func GasStation(x Refillable) {
  x.Refill()
}
```
