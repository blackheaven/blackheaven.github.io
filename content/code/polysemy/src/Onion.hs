{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Onion where

import Data.Kind
import Polysemy

newtype BookingRef = BookingRef {getBookingRef :: Int}
  deriving stock (Eq, Ord, Show)

newtype Booking = Booking {carRef :: CarRef}
  deriving stock (Eq, Ord, Show)

newtype CarRef = CarRef {getCarRef :: Int}
  deriving stock (Eq, Ord, Show)

data Car = Car {ref :: CarRef, model :: ()}
  deriving stock (Eq, Ord, Show)

newtype CarDamage = CarDamage {getCarDamage :: Int}
  deriving stock (Eq, Ord, Show)

newtype CarPricedDamage = CarPricedDamage {getCarPricedDamage :: Int}
  deriving stock (Eq, Ord, Show)

newtype CarObservations = CarObservations {getCarObservations :: Int}
  deriving stock (Eq, Ord, Show)

newtype Bill = Bill {getBill :: Int}
  deriving stock (Eq, Ord, Show)

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

inducedDamages :: Car -> [CarObservations] -> [CarDamage]
inducedDamages _ _ = error "-"

createBill :: Booking -> Car -> [CarPricedDamage] -> Bill
createBill _ _ = error "-"

returnCar :: Members '[Bookings, Cars, DamageReferences] r => BookingRef -> [CarObservations] -> Sem r Bill
returnCar ref observations = do
  booking <- fetchBooking ref
  car <- fetchCar booking.carRef
  let newDamages = inducedDamages car observations
  registerReturnedCar car.ref newDamages
  pricedDamages <- mapM (fetchPricedDamage car.model) newDamages
  return $ createBill booking car pricedDamages
