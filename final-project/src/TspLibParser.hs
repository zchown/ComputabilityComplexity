{-# LANGUAGE OverloadedStrings #-}

module TspLibParser where

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (transpose)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Text.Read (readMaybe)
import TspTypes

parseTspFile :: String -> IO (Either TspParseError TspProblem)
parseTspFile filename = do
  contents <- fmap (unpack . decompress) (LBS.readFile filename)
  let linesOfFile = lines contents
  let (headerLines, remainingLines) = break isSectionMarker linesOfFile
  return $ do
    header <- parseHeader headerLines
    body <- parseBody header remainingLines
    Right $ tspProblemFromHeaderBody header body
  where
    isSectionMarker =
      (`elem` [ "NODE_COORD_SECTION"
              , "EDGE_WEIGHT_SECTION"
              , "FIXED_EDGES_SECTION"
              ]) .
      unwords . words

parseHeaderLine :: String -> Either TspParseError (String, String)
parseHeaderLine line =
  case splitOn ":" (filter (/= ' ') line) of
    [k, v] -> Right (k, trimStr v)
    _ -> Left $ InvalidFormat $ "Invalid header line format: " ++ line
  where
    trimStr = filter (/= ' ')

parseHeader :: [String] -> Either TspParseError TspProblemHeader
parseHeader linesOfFile = do
  let nonEmptyLines = filter (not . null) linesOfFile
  headerPairs <- traverse parseHeaderLine nonEmptyLines
  let headerMap = Map.fromList headerPairs
  name <- getRequired "NAME" headerMap
  dataType <- getRequiredParsed "TYPE" tspDataTypeFromStr headerMap
  let comment = Map.lookup "COMMENT" headerMap
  dimension <- getRequiredParsed "DIMENSION" readMaybe headerMap
  edgeWeightType <-
    getOptionalParsed "EDGE_WEIGHT_TYPE" edgeWeightTypeFromStr headerMap
  edgeWeightFormat <-
    getOptionalParsed "EDGE_WEIGHT_FORMAT" edgeWeightFormatFromStr headerMap
  edgeDataFormat <-
    getOptionalParsed "EDGE_DATA_FORMAT" edgeDataFormatFromStr headerMap
  nodeCoordType <-
    getOptionalParsed "NODE_COORD_TYPE" nodeCoordTypeFromStr headerMap
  let initialData =
        case edgeWeightType of
          Just EXPLICIT -> EdgeWeightData []
          _ -> NodeCoordData []
  Right $
    TspProblemHeader
      { name = name
      , dataType = dataType
      , comment = comment
      , dimension = dimension
      , edgeWeightType = edgeWeightType
      , edgeWeightFormat = edgeWeightFormat
      , edgeDataFormat = edgeDataFormat
      , nodeCoordType = nodeCoordType
      , tData = initialData
      }
  where
    getRequired field map =
      case Map.lookup field map of
        Nothing -> Left $ MissingRequiredField field
        Just v -> Right v
    getRequiredParsed field parser map = do
      value <- getRequired field map
      case parser value of
        Nothing -> Left $ InvalidFieldValue field value
        Just v -> Right v
    getOptionalParsed field parser map =
      case Map.lookup field map of
        Nothing -> Right Nothing
        Just value ->
          case parser value of
            Nothing -> Left $ InvalidFieldValue field value
            Just v -> Right (Just v)

parseNodeLine :: String -> Either TspParseError Node
parseNodeLine line =
  case words line of
    [idStr, xStr, yStr] -> do
      nodeId <-
        maybe (Left $ InvalidNodeData "Invalid node ID") Right $ readMaybe idStr
      x <-
        maybe (Left $ InvalidNodeData "Invalid x coordinate") Right $
        readMaybe xStr
      y <-
        maybe (Left $ InvalidNodeData "Invalid y coordinate") Right $
        readMaybe yStr
      Right $ Node2D nodeId (Point2D x y)
    [idStr, xStr, yStr, zStr] -> do
      nodeId <-
        maybe (Left $ InvalidNodeData "Invalid node ID") Right $ readMaybe idStr
      x <-
        maybe (Left $ InvalidNodeData "Invalid x coordinate") Right $
        readMaybe xStr
      y <-
        maybe (Left $ InvalidNodeData "Invalid y coordinate") Right $
        readMaybe yStr
      z <-
        maybe (Left $ InvalidNodeData "Invalid z coordinate") Right $
        readMaybe zStr
      Right $ Node3D nodeId (Point3D x y z)
    _ -> Left $ InvalidNodeData $ "Invalid node data format: " ++ line

parseBody :: TspProblemHeader -> [String] -> Either TspParseError TspProblemBody
parseBody header [] = Left $ InvalidFormat "Missing data section"
parseBody header allLines = do
  let sections = splitSections allLines
  parsedSections <- traverse (parseSectionData header) sections
  let combinedData = combineSectionData parsedSections
  Right $ TspProblemBody combinedData

splitSections :: [String] -> [(String, [String])]
splitSections [] = []
splitSections (line:lines)
  | isSectionMarker line =
    let (sectionData, rest) = break isSectionMarker lines
        remainingSections = splitSections rest
     in (strip line, takeWhile (/= "EOF") sectionData) : remainingSections
  | otherwise = splitSections lines
  where
    isSectionMarker =
      (`elem` [ "NODE_COORD_SECTION"
              , "EDGE_WEIGHT_SECTION"
              , "FIXED_EDGES_SECTION"
              ]) .
      unwords . words

parseSectionData ::
     TspProblemHeader -> (String, [String]) -> Either TspParseError TspData
parseSectionData header ("NODE_COORD_SECTION", lines) = do
  nodes <- traverse parseNodeLine (filter (not . null) lines)
  Right $ NodeCoordData nodes
parseSectionData header ("EDGE_WEIGHT_SECTION", lines) = do
  weights <- parseEdgeWeightSection header lines
  Right $
    case weights of
      TspProblemBody (EdgeWeightData matrix) -> EdgeWeightData matrix
      _ -> EdgeWeightData []
parseSectionData header ("FIXED_EDGES_SECTION", lines) = do
  edges <- parseFixedEdgesSection (filter (not . null) lines)
  Right $ FixedEdgesData edges
parseSectionData _ (section, _) =
  Left $ UnsupportedFormat $ "Unknown section: " ++ section

parseFixedEdgesSection :: [String] -> Either TspParseError [FixedEdge]
parseFixedEdgesSection lines = do
  let edgeLines = takeWhile (/= "-1") lines
  traverse parseFixedEdgeLine edgeLines

parseFixedEdgeLine :: String -> Either TspParseError FixedEdge
parseFixedEdgeLine line =
  case words line of
    [fromStr, toStr] -> do
      fromNode <-
        maybe (Left $ InvalidFixedEdge "Invalid from node") Right $
        readMaybe fromStr
      toNode <-
        maybe (Left $ InvalidFixedEdge "Invalid to node") Right $
        readMaybe toStr
      Right $ FixedEdge fromNode toNode
    _ -> Left $ InvalidFixedEdge $ "Invalid fixed edge format: " ++ line

combineSectionData :: [TspData] -> TspData
combineSectionData sections =
  CombinedData
    { nodeCoords = getNodeCoords sections
    , edgeWeights = getEdgeWeights sections
    , fixedEdges = getFixedEdges sections
    }
  where
    getNodeCoords = foldr collectNodeCoords Nothing
    getEdgeWeights = foldr collectEdgeWeights Nothing
    getFixedEdges = foldr collectFixedEdges Nothing
    collectNodeCoords (NodeCoordData nodes) _ = Just nodes
    collectNodeCoords _ acc = acc
    collectEdgeWeights (EdgeWeightData weights) _ = Just weights
    collectEdgeWeights _ acc = acc
    collectFixedEdges (FixedEdgesData edges) _ = Just edges
    collectFixedEdges _ acc = acc

parseNodeCoordSection :: [String] -> Either TspParseError TspProblemBody
parseNodeCoordSection lines = do
  let (coordLines, _) = break (== "EOF") lines
  nodes <- traverse parseNodeLine (filter (not . null) coordLines)
  Right $ TspProblemBody $ NodeCoordData nodes

parseEdgeWeightSection ::
     TspProblemHeader -> [String] -> Either TspParseError TspProblemBody
parseEdgeWeightSection header lines = do
  let (weightLines, _) = break (== "EOF") lines
      dim = dimension header
      numbers = concat $ map words $ filter (not . null) weightLines
  weights <-
    case traverse readMaybe numbers of
      Nothing -> Left $ InvalidFormat "Invalid number in weight matrix"
      Just nums -> Right nums
  matrix <-
    case edgeWeightFormat header of
      Just FULL_MATRIX -> parseFullMatrix dim weights
      Just UPPER_ROW -> parseUpperRow dim weights
      Just LOWER_ROW -> parseLowerRow dim weights
      Just UPPER_DIAG_ROW -> parseUpperDiagRow dim weights
      Just LOWER_DIAG_ROW -> parseLowerDiagRow dim weights
      Just UPPER_COL -> parseUpperCol dim weights
      Just LOWER_COL -> parseLowerCol dim weights
      Just UPPER_DIAG_COL -> parseUpperDiagCol dim weights
      Just LOWER_DIAG_COL -> parseLowerDiagCol dim weights
      _ -> Left $ InvalidFormat "Unsupported or missing edge weight format"
  Right $ TspProblemBody $ EdgeWeightData matrix

parseFullMatrix :: Int -> [Double] -> Either TspParseError [[Double]]
parseFullMatrix dim weights
  | length weights == dim * dim = Right $ chunksOf dim weights
  | otherwise =
    Left $ InvalidFormat "Incorrect number of weights for full matrix"
  where
    chunksOf n = takeWhile (not . null) . map (take n) . iterate (drop n)

parseUpperRow :: Int -> [Double] -> Either TspParseError [[Double]]
parseUpperRow dim weights
  | length weights == ((dim - 1) * dim) `div` 2 =
    Right $ constructMatrix dim weights Upper False
  | otherwise = Left $ InvalidFormat "Incorrect number of weights for upper row"

parseLowerRow :: Int -> [Double] -> Either TspParseError [[Double]]
parseLowerRow dim weights
  | length weights == ((dim - 1) * dim) `div` 2 =
    Right $ constructMatrix dim weights Lower False
  | otherwise = Left $ InvalidFormat "Incorrect number of weights for lower row"

parseUpperDiagRow :: Int -> [Double] -> Either TspParseError [[Double]]
parseUpperDiagRow dim weights
  | length weights == (dim * (dim + 1)) `div` 2 =
    Right $ constructMatrix dim weights Upper True
  | otherwise =
    Left $ InvalidFormat "Incorrect number of weights for upper diagonal row"

parseLowerDiagRow :: Int -> [Double] -> Either TspParseError [[Double]]
parseLowerDiagRow dim weights
  | length weights == (dim * (dim + 1)) `div` 2 =
    Right $ constructMatrix dim weights Lower True
  | otherwise =
    Left $ InvalidFormat "Incorrect number of weights for lower diagonal row"

parseUpperCol :: Int -> [Double] -> Either TspParseError [[Double]]
parseUpperCol dim weights
  | length weights == ((dim - 1) * dim) `div` 2 =
    Right $ transpose $ constructMatrix dim weights Upper False
  | otherwise = Left $ InvalidFormat "Incorrect number of weights for upper col"

parseLowerCol :: Int -> [Double] -> Either TspParseError [[Double]]
parseLowerCol dim weights
  | length weights == ((dim - 1) * dim) `div` 2 =
    Right $ transpose $ constructMatrix dim weights Lower False
  | otherwise = Left $ InvalidFormat "Incorrect number of weights for lower col"

parseUpperDiagCol :: Int -> [Double] -> Either TspParseError [[Double]]
parseUpperDiagCol dim weights
  | length weights == (dim * (dim + 1)) `div` 2 =
    Right $ transpose $ constructMatrix dim weights Upper True
  | otherwise =
    Left $ InvalidFormat "Incorrect number of weights for upper diagonal col"

parseLowerDiagCol :: Int -> [Double] -> Either TspParseError [[Double]]
parseLowerDiagCol dim weights
  | length weights == (dim * (dim + 1)) `div` 2 =
    Right $ transpose $ constructMatrix dim weights Lower True
  | otherwise =
    Left $ InvalidFormat "Incorrect number of weights for lower diagonal col"

data TriangleType
  = Upper
  | Lower

constructMatrix :: Int -> [Double] -> TriangleType -> Bool -> [[Double]]
constructMatrix dim weights triangleType includeDiagonal =
  [[getWeight i j | j <- [0 .. dim - 1]] | i <- [0 .. dim - 1]]
  where
    getWeight i j
      | i == j =
        if includeDiagonal
          then getDiagonalWeight i
          else 0
      | isValidPosition i j = getStoredWeight i j
      | otherwise = getStoredWeight j i
    isValidPosition i j =
      case triangleType of
        Upper -> i < j
        Lower -> i > j
    getStoredWeight i j = weights !! getIndex i j
    getDiagonalWeight i = weights !! (diagonalIndex i)
    getIndex i j =
      case triangleType of
        Upper -> upperTriangularIndex (min i j) (max i j) dim includeDiagonal
        Lower -> lowerTriangularIndex (min i j) (max i j) dim includeDiagonal
    diagonalIndex i = i

upperTriangularIndex :: Int -> Int -> Int -> Bool -> Int
upperTriangularIndex i j dim includeDiagonal
  | includeDiagonal =
    let row = i
        offset = sum [dim - k | k <- [0 .. row - 1]]
     in offset + (j - row)
  | otherwise =
    let row = i
        offset = sum [dim - k - 1 | k <- [0 .. row - 1]]
     in offset + (j - row - 1)

lowerTriangularIndex :: Int -> Int -> Int -> Bool -> Int
lowerTriangularIndex i j dim includeDiagonal
  | includeDiagonal =
    let row = j
        offset = sum [k + 1 | k <- [0 .. row - 1]]
     in offset + i - row
  | otherwise =
    let row = j
        offset = sum [k | k <- [0 .. row - 1]]
     in offset + i - row

expectedWeights :: Int -> EdgeWeightFormat -> Int
expectedWeights dim format =
  case format of
    FULL_MATRIX -> dim * dim
    UPPER_ROW -> ((dim - 1) * dim) `div` 2
    LOWER_ROW -> ((dim - 1) * dim) `div` 2
    UPPER_DIAG_ROW -> (dim * (dim + 1)) `div` 2
    LOWER_DIAG_ROW -> (dim * (dim + 1)) `div` 2
    UPPER_COL -> ((dim - 1) * dim) `div` 2
    LOWER_COL -> ((dim - 1) * dim) `div` 2
    UPPER_DIAG_COL -> (dim * (dim + 1)) `div` 2
    LOWER_DIAG_COL -> (dim * (dim + 1)) `div` 2
    _ -> 0
