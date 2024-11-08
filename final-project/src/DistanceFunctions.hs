module DistanceFunctions where

import TspTypes

nint :: Double -> Int
nint = floor . (+ 0.5)

euclideanDistance :: Point2D -> Point2D -> Int
euclideanDistance (Point2D x1 y1) (Point2D x2 y2) =
  nint $ sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

euclideanDistance3D :: Point3D -> Point3D -> Int
euclideanDistance3D (Point3D x1 y1 z1) (Point3D x2 y2 z2) =
  nint $ sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2

ceilEuclideanDistance :: Point2D -> Point2D -> Int
ceilEuclideanDistance (Point2D x1 y1) (Point2D x2 y2) =
  ceiling $ sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 + 0.5

ceilEuclideanDistance3D :: Point3D -> Point3D -> Int
ceilEuclideanDistance3D (Point3D x1 y1 z1) (Point3D x2 y2 z2) =
  ceiling $ sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2 + 0.5

maxDistance :: Point2D -> Point2D -> Int
maxDistance (Point2D x1 y1) (Point2D x2 y2) =
  max (foo $ x1 - x2) (foo $ y1 - y2)
  where
    foo = nint . abs

maxDistance3D :: Point3D -> Point3D -> Int
maxDistance3D (Point3D x1 y1 z1) (Point3D x2 y2 z2) =
  maximum [foo $ x1 - x2, foo $ y1 - y2, foo $ z1 - z2]
  where
    foo = nint . abs

manhattanDistance :: Point2D -> Point2D -> Int
manhattanDistance (Point2D x1 y1) (Point2D x2 y2) =
  nint $ abs (x1 - x2) + abs (y1 - y2)

manhattanDistance3D :: Point3D -> Point3D -> Int
manhattanDistance3D (Point3D x1 y1 z1) (Point3D x2 y2 z2) =
  nint $ abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

geoDistance :: Point2D -> Point2D -> Int
geoDistance (Point2D lat1 long1) (Point2D lat2 long2) =
  floor $ rrr * acos (0.5 * ((1.0 + q1) * q2 - (1.0 - q1) * q3)) + 1.0
  where
    p = 3.141592 :: Double
    rrr = 6378.388 :: Double
    foo x = p * (inint x + 5.0 * (x - inint x) / 3.0) / 180.0
    q1 = cos (foo long1 - foo long2)
    q2 = cos (foo lat1 - foo lat2)
    q3 = cos (foo lat1 + foo lat2)
    inint = fromIntegral . nint

attDistance :: Point2D -> Point2D -> Int
attDistance (Point2D x1 y1) (Point2D x2 y2)
  | itij < rij = tij + 1
  | otherwise = tij
  where
    xd = x1 - x2
    yd = y1 - y2
    rij = sqrt $ (xd ** 2 + yd ** 2) / 10.0
    tij = nint rij
    itij = fromIntegral tij
