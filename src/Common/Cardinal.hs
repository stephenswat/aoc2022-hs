module Common.Cardinal (Direction (..), Rotation (..), rotate) where

data Direction = North | West | South | East deriving (Eq, Show)

data Rotation = RotateLeft | RotateRight deriving (Eq, Show)

rotate :: Rotation -> Direction -> Direction
rotate RotateLeft North = West
rotate RotateLeft West = South
rotate RotateLeft South = East
rotate RotateLeft East = North
rotate RotateRight North = East
rotate RotateRight West = North
rotate RotateRight South = West
rotate RotateRight East = South
