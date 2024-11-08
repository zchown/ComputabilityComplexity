module TspTypes where

data Point2D = Point2D
  { xPos :: Double
  , yPos :: Double
  } deriving (Show, Eq)

data Point3D = Point3D
  { x3 :: Double
  , y3 :: Double
  , zPos :: Double
  } deriving (Show, Eq)

-- Add FixedEdge type
data FixedEdge = FixedEdge
  { fromNode :: Int
  , toNode :: Int
  } deriving (Show, Eq)

data TspDataTypes
  = TSP
  | ATSP
  | SOP
  | HCP
  | CVRP
  | TOUR
  deriving (Show, Eq)

tspDataTypeFromStr :: String -> Maybe TspDataTypes
tspDataTypeFromStr str =
  case strip str of
    "TSP" -> Just TSP
    "TSP(M.~Hofmeister)" -> Just TSP
    "ATSP" -> Just ATSP
    "SOP" -> Just SOP
    "HCP" -> Just HCP
    "CVRP" -> Just CVRP
    "TOUR" -> Just TOUR
    _ -> Nothing

data TspEdgeWeightType
  = EXPLICIT
  | EUC_2D
  | EUC_3D
  | MAX_2D
  | MAX_3D
  | MAN_2D
  | MAN_3D
  | CEIL_2D
  | GEO
  | ATT
  | XRAY1
  | XRAY2
  | SPECIAL
  deriving (Show, Eq)

edgeWeightTypeFromStr :: String -> Maybe TspEdgeWeightType
edgeWeightTypeFromStr str =
  case strip str of
    "EXPLICIT" -> Just EXPLICIT
    "EUC_2D" -> Just EUC_2D
    "EUC_3D" -> Just EUC_3D
    "MAX_2D" -> Just MAX_2D
    "MAX_3D" -> Just MAX_3D
    "MAN_2D" -> Just MAN_2D
    "MAN_3D" -> Just MAN_3D
    "CEIL_2D" -> Just CEIL_2D
    "GEO" -> Just GEO
    "ATT" -> Just ATT
    "XRAY1" -> Just XRAY1
    "XRAY2" -> Just XRAY2
    "SPECIAL" -> Just SPECIAL
    _ -> Nothing

data EdgeWeightFormat
  = FUNCTION
  | FULL_MATRIX
  | UPPER_ROW
  | LOWER_ROW
  | UPPER_DIAG_ROW
  | LOWER_DIAG_ROW
  | UPPER_COL
  | LOWER_COL
  | UPPER_DIAG_COL
  | LOWER_DIAG_COL
  deriving (Show, Eq)

edgeWeightFormatFromStr :: String -> Maybe EdgeWeightFormat
edgeWeightFormatFromStr str =
  case strip str of
    "FUNCTION" -> Just FUNCTION
    "FULL_MATRIX" -> Just FULL_MATRIX
    "UPPER_ROW" -> Just UPPER_ROW
    "LOWER_ROW" -> Just LOWER_ROW
    "UPPER_DIAG_ROW" -> Just UPPER_DIAG_ROW
    "LOWER_DIAG_ROW" -> Just LOWER_DIAG_ROW
    "UPPER_COL" -> Just UPPER_COL
    "LOWER_COL" -> Just LOWER_COL
    "UPPER_DIAG_COL" -> Just UPPER_DIAG_COL
    "LOWER_DIAG_COL" -> Just LOWER_DIAG_COL
    _ -> Nothing

data TspEdgeDataFormat
  = EDGE_LIST
  | ADJ_LIST
  deriving (Show, Eq)

edgeDataFormatFromStr :: String -> Maybe TspEdgeDataFormat
edgeDataFormatFromStr str =
  case strip str of
    "EDGE_LIST" -> Just EDGE_LIST
    "ADJ_LIST" -> Just ADJ_LIST
    _ -> Nothing

data TspNodeCoordType
  = TWOD_COORDS
  | THREED_COORDS
  | NO_COORDS
  deriving (Show, Eq)

nodeCoordTypeFromStr :: String -> Maybe TspNodeCoordType
nodeCoordTypeFromStr str =
  case strip str of
    "TWOD_COORDS" -> Just TWOD_COORDS
    "THREED_COORDS" -> Just THREED_COORDS
    "NO_COORDS" -> Just NO_COORDS
    _ -> Nothing

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
  where
    isSpace c = c == ' ' || c == '\t'

data Node
  = Node2D
      { nodeId :: Int
      , node2D :: Point2D
      }
  | Node3D
      { nodeId :: Int
      , node3D :: Point3D
      }
  deriving (Show, Eq)

data TspData
  = NodeCoordData [Node]
  | EdgeWeightData [[Double]]
  | FixedEdgesData [FixedEdge]
  | CombinedData
      { nodeCoords :: Maybe [Node]
      , edgeWeights :: Maybe [[Double]]
      , fixedEdges :: Maybe [FixedEdge]
      }
  deriving (Show, Eq)

data TspProblem = TspProblem
  { tspName :: String
  , tspType :: TspDataTypes
  , tspComment :: Maybe String
  , tspDimension :: Int
  , tspEdgeWeightType :: Maybe TspEdgeWeightType
  , tspEdgeWeightFormat :: Maybe EdgeWeightFormat
  , tspEdgeDataFormat :: Maybe TspEdgeDataFormat
  , tspNodeCoordType :: Maybe TspNodeCoordType
  , tspData :: TspData
  } deriving (Show, Eq)

data TspProblemHeader = TspProblemHeader
  { name :: String
  , dataType :: TspDataTypes
  , comment :: Maybe String
  , dimension :: Int
  , edgeWeightType :: Maybe TspEdgeWeightType
  , edgeWeightFormat :: Maybe EdgeWeightFormat
  , edgeDataFormat :: Maybe TspEdgeDataFormat
  , nodeCoordType :: Maybe TspNodeCoordType
  , tData :: TspData
  } deriving (Show, Eq)

data TspProblemBody = TspProblemBody
  { btData :: TspData
  } deriving (Show, Eq)

tspProblemFromHeaderBody :: TspProblemHeader -> TspProblemBody -> TspProblem
tspProblemFromHeaderBody header body =
  TspProblem
    { tspName = name header
    , tspType = dataType header
    , tspComment = comment header
    , tspDimension = dimension header
    , tspEdgeWeightType = edgeWeightType header
    , tspEdgeWeightFormat = edgeWeightFormat header
    , tspEdgeDataFormat = edgeDataFormat header
    , tspNodeCoordType = nodeCoordType header
    , tspData = btData body
    }

data TspParseError
  = MissingRequiredField String
  | InvalidFieldValue String String
  | InvalidFormat String
  | InvalidNodeData String
  | InvalidEdgeWeight String
  | InvalidFixedEdge String
  | InvalidDimension Int Int
  | InvalidWeightCount EdgeWeightFormat Int Int
  | ParseError String
  | UnsupportedFormat String
  deriving (Show, Eq)

formatTspError :: TspParseError -> String
formatTspError err =
  case err of
    MissingRequiredField field -> "Missing required field: " ++ field
    InvalidFieldValue field value ->
      "Invalid value '" ++ value ++ "' for field: " ++ field
    InvalidFormat msg -> "Invalid format: " ++ msg
    InvalidNodeData msg -> "Invalid node data: " ++ msg
    InvalidEdgeWeight msg -> "Invalid edge weight: " ++ msg
    InvalidFixedEdge msg -> "Invalid fixed edge: " ++ msg
    InvalidDimension expected actual ->
      "Dimension mismatch: expected " ++
      show expected ++ " but found " ++ show actual
    InvalidWeightCount format expected actual ->
      "Weight count mismatch for " ++
      show format ++
      ": expected " ++ show expected ++ " weights but found " ++ show actual
    ParseError msg -> "Parse error: " ++ msg
    UnsupportedFormat fmt -> "Unsupported format: " ++ fmt

checkWeightCount ::
     EdgeWeightFormat -> Int -> [Double] -> Either TspParseError [Double]
checkWeightCount format dim weights =
  let expected = expectedWeightCount format dim
   in if length weights == expected
        then Right weights
        else Left $ InvalidWeightCount format expected (length weights)

expectedWeightCount :: EdgeWeightFormat -> Int -> Int
expectedWeightCount format dim =
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
    FUNCTION -> 0

validateNodeCoords :: [Node] -> Int -> Either TspParseError [Node]
validateNodeCoords nodes expectedDim =
  if length nodes == expectedDim
    then Right nodes
    else Left $ InvalidDimension expectedDim (length nodes)
