module TspLibParser where

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Text.Read (readMaybe)
import TspTypes

data TspParseError
  = MissingRequiredField String
  | InvalidFieldValue String String
  | InvalidFormat String
  | InvalidNodeData String
  deriving (Show, Eq)

parseTspFile :: String -> IO (Either TspParseError TspProblem)
parseTspFile filename = do
  contents <- fmap (unpack . decompress) (LBS.readFile filename)
  let linesOfFile = lines contents
  let (headerLines, bodyLines) = break (== "NODE_COORD_SECTION") linesOfFile
  return $ do
    header <- parseHeader headerLines
    body <- parseBody bodyLines
    if length (nodes body) /= dimension header
      then Left $ InvalidFormat "Number of nodes doesn't match dimension"
      else Right $ tspProblemFromHeaderBody header body

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
  let getRequired field =
        case Map.lookup field headerMap of
          Nothing -> Left $ MissingRequiredField field
          Just v -> Right v
  let getRequiredParsed field parser = do
        value <- getRequired field
        case parser value of
          Nothing -> Left $ InvalidFieldValue field value
          Just v -> Right v
  name <- getRequired "NAME"
  dataType <- getRequiredParsed "TYPE" tspDataTypeFromStr
  comment <- getRequired "COMMENT"
  dimension <- getRequiredParsed "DIMENSION" readMaybe
  let getOptionalParsed field parser =
        case Map.lookup field headerMap of
          Nothing -> Right Nothing
          Just value ->
            case parser value of
              Nothing -> Left $ InvalidFieldValue field value
              Just v -> Right (Just v)
  edgeWeightType <- getOptionalParsed "EDGE_WEIGHT_TYPE" edgeWeightTypeFromStr
  edgeWeightFormat <-
    getOptionalParsed "EDGE_WEIGHT_FORMAT" edgeWeightFormatFromStr
  edgeDataFormat <- getOptionalParsed "EDGE_DATA_FORMAT" edgeDataFormatFromStr
  nodeCoordType <- getOptionalParsed "NODE_COORD_TYPE" nodeCoordTypeFromStr
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
      }

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

parseBody :: [String] -> Either TspParseError TspProblemBody
parseBody [] = Right $ TspProblemBody []
parseBody ("NODE_COORD_SECTION":nodeLines) = do
  let (coordLines, _) = break (== "EOF") nodeLines
  nodes <- traverse parseNodeLine (filter (not . null) coordLines)
  Right $ TspProblemBody nodes
parseBody _ = Left $ InvalidFormat "Missing NODE_COORD_SECTION"


