{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}

module Spec (main, spec) where

import Test.Hspec
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Game of life" $ do
    describe "Outputs" $ do
      describe "Next generation" $ do
        it "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction" $
          nextGen dead 3 `shouldBe` alive
        it "Any live cell with more than three live neighbours dies, as if by overpopulation" $
          nextGen alive 4 `shouldBe` dead
        it "Any live cell with two live neighbours lives on to the next generation" $
          nextGen alive 2 `shouldBe` alive
        it "Any live cell with three live neighbours lives on to the next generation" $
          nextGen alive 3 `shouldBe` alive
        it "Any dead cell with fewer than three (2) live neighbours stays dead on to the next generation" $
          nextGen dead 2 `shouldBe` dead
        it "Any live cell with fewer than two (1) live neighbours dies, as if by underpopulation" $
          nextGen alive 1 `shouldBe` dead
        it "Any live cell with fewer than two (0) live neighbours dies, as if by underpopulation" $
          nextGen alive 0 `shouldBe` dead
    describe "Inputs" $ do
      describe "Next generation" $ do
        describe "Reproduction (three live neighbours)" $ do
          it "Any dead cell with exactly three live neighbours becomes a live cell" $
            reproduction.next dead `shouldBe` alive
          it "Any live cell with three live neighbours lives on to the next generation" $
            reproduction.next alive `shouldBe` alive
        describe "Overpopulation (more than three live neighbours)" $ do
          it "Any live cell with more than three live neighbours dies" $
            overpopulation.next alive `shouldBe` dead
        describe "Survive (two live neighbours)" $ do
          it "Any dead cell with fewer than three live neighbours stays dead on to the next generation" $
            survive.next dead `shouldBe` dead
          it "Any dead cell with exactly three live neighbours becomes a live cell" $
            survive.next alive `shouldBe` alive
        describe "Underpopulation (zero live neighbours)" $ do
          it "Any dead cell with fewer than three live neighbours stays dead on to the next generation" $
            underpopulation0.next dead `shouldBe` dead
          it "Any live cell with fewer than two live neighbours dies" $
            underpopulation0.next alive `shouldBe` dead
        describe "Underpopulation (one live neighbours)" $ do
          it "Any dead cell with fewer than three live neighbours stays dead on to the next generation" $
            underpopulation1.next dead `shouldBe` dead
          it "Any live cell with fewer than two live neighbours dies" $
            underpopulation1.next alive `shouldBe` dead
      describe "Neighbourhood selection" $ do
        it "No alive neighbours should be 'underpopulation'" $
          (neighbourhood []).name `shouldBe` "underpopulation"
        it "One alive neighbours should be 'underpopulation'" $
          (neighbourhood [alive]).name `shouldBe` "underpopulation"
        it "Two alive neighbours should be 'survive'" $
          (neighbourhood [alive, alive]).name `shouldBe` "survive"
        it "Three alive neighbours should be 'reproduction'" $
          (neighbourhood [alive, alive, alive]).name `shouldBe` "reproduction"
        it "Four alive neighbours should be 'overpopulation'" $
          (neighbourhood [alive, alive, alive, alive]).name `shouldBe` "overpopulation"
        it "Two alive neighbours and three dead should be 'survive'" $
          (neighbourhood [dead, alive, dead, alive, dead]).name `shouldBe` "survive"
      describe "Grid" $ do
        let verticalBlinkerCells = [Pos 1 0, Pos 1 1, Pos 1 2]
        it "Vertical blinker should become horizontal" $
          aliveCells (nextGrid $ mkGrid (Pos 2 2) verticalBlinkerCells)
            `shouldBe` [Pos 0 1, Pos 1 1, Pos 2 1]
        it "Vertical blinker should become vertical after two generations" $
          aliveCells (nextGrid $ nextGrid $ mkGrid (Pos 2 2) verticalBlinkerCells)
            `shouldBe` verticalBlinkerCells

newtype Cell = Cell (forall a. forall a. a -> a -> a)

instance Show Cell where
  show = runCell (WhenDead "Dead") (WhenAlive "Alive")

instance Eq Cell where
  x == y =
    runCell (WhenDead $ Left ()) (WhenAlive $ Right ()) x == runCell (WhenDead $ Left ()) (WhenAlive $ Right ()) y

newtype WhenDead a = WhenDead a

newtype WhenAlive a = WhenAlive a

runCell :: WhenDead a -> WhenAlive a -> Cell -> a
runCell (WhenDead d) (WhenAlive a) (Cell f) = f d a

alive :: Cell
alive = Cell $ \_ a -> a

dead :: Cell
dead = Cell const

newtype Neighbours = Neighbours {getNeighbours :: Int}
  deriving newtype (Eq, Ord, Show, Num)

nextGen :: Cell -> Neighbours -> Cell
nextGen x =
  \case
    2 -> x
    3 -> alive
    _ -> dead

data Neighbourhood = Neighbourhood
  { name :: String,
    next :: Cell -> Cell,
    nextNeighbourhood :: Neighbourhood
  }

underpopulation0 :: Neighbourhood
underpopulation0 =
  Neighbourhood
    { name = "underpopulation",
      next = const dead,
      nextNeighbourhood = underpopulation1
    }

underpopulation1 :: Neighbourhood
underpopulation1 =
  Neighbourhood
    { name = "underpopulation",
      next = const dead,
      nextNeighbourhood = survive
    }

survive :: Neighbourhood
survive =
  Neighbourhood
    { name = "survive",
      next = id,
      nextNeighbourhood = reproduction
    }

reproduction :: Neighbourhood
reproduction =
  Neighbourhood
    { name = "reproduction",
      next = const alive,
      nextNeighbourhood = overpopulation
    }

overpopulation :: Neighbourhood
overpopulation =
  Neighbourhood
    { name = "overpopulation",
      next = const dead,
      nextNeighbourhood = overpopulation
    }

neighbourhood :: [Cell] -> Neighbourhood
neighbourhood = foldr (runCell (WhenDead id) (WhenAlive (.nextNeighbourhood))) underpopulation0

data Pos = Pos
  { posX :: Int,
    posY :: Int
  }
  deriving stock (Eq, Ord, Show)

newtype Grid
  = Grid { getGrid :: Map.Map Pos Cell }
  deriving stock (Eq, Show)

mkGrid :: Pos -> Set.Set Pos -> Grid
mkGrid limits alives =
  Grid $
    Map.fromList
    [ let p = Pos x y in (p, if Set.member p alives then alive else dead)
      | x <- [0 .. limits.posX]
      , y <- [0 .. limits.posY]
    ]

aliveCells :: Grid -> Set.Set Pos
aliveCells =
  Set.fromList
  . mapMaybe (\(p, c) -> runCell (WhenDead Nothing) (WhenAlive $ Just p) c)
  . Map.toList
  . getGrid

nextGrid :: Grid -> Grid
nextGrid (Grid grid) =
  Grid $
    Map.mapWithKey (\p -> (neighbourhood $ neighbours p).next) grid
  where neighbours (Pos x y) =
          [ Map.findWithDefault dead (Pos (x + dx) (y + dy)) grid
            | dx <- [(-1) .. 1]
            , dy <- [(-1) .. 1]
            , dx /= 0 || dy /= 0
          ]