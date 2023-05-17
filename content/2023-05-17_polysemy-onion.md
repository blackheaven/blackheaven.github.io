+++
title = "Polysemy: Onion architecture"
date = 2023-05-17
draft = false
path = "2023-05/polysemy-onion"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

Until now we only seen "low-level" effects (either classical Monad, or technical effects).

On of my main motivation with effects systems is to be used in an [Onion architecture](https://jeffreypalermo.com/2008/07/the-onion-architecture-part-1/).

Doing so, we can imagine to modelise a car return within a car rental business.

For example, last year we introduced a [Cache effect](@/2022-12-18_polysemy-interpretation-effects-inline-injection.md):

```haskell
returnCar :: Members '[Bookings, Cars, DamageReferences] r => BookingRef -> [CarObservations] -> Sem r Bill
returnCar ref observations = do
  booking <- fetchBooking ref
  car <- fetchCar booking.carRef
  let newDamages = inducedDamages car observations
  registerReturnedCar car.ref newDamages
  pricedDamages <- mapM (fetchPricedDamage car.model) newDamages
  return $ createBill booking car pricedDamages
```

with the following effects for Repositories:

```haskell
data Bookings (m :: Type -> Type) a where
  FetchBooking :: BookingRef -> Bookings m Booking

makeSem ''Bookings

data Cars (m :: Type -> Type) a where
  FetchCar :: CarRef -> Cars m Car
  RegisterReturnedCar :: CarRef -> [CarDamage] -> Cars m ()

makeSem ''Cars

data DamageReferences (m :: Type -> Type) a where
  FetchPricedDamage :: () -> CarDamage -> DamageReferences m CarPricedDamage

makeSem ''DamageReferences
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Onion.hs).

