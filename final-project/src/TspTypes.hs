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
  case str of
    "TSP" -> Just TSP
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
  case str of
    "EXPLICIT" -> Just EXPLICIT
    "EUC_2D" -> Just EUC_2D
    "EUC_3D" -> Just EUC_3D
    "MAX_2D" -> Just MAX_2D
    "MAX_3D" -> Just MAX_3D
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
  case str of
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
  case str of
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
  case str of
    "TWOD_COORDS" -> Just TWOD_COORDS
    "THREED_COORDS" -> Just THREED_COORDS
    "NO_COORDS" -> Just NO_COORDS
    _ -> Nothing

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

data TspProblem = TspProblem
  { tspName :: String
  , tspType :: TspDataTypes
  , tspComment :: String
  , tspDimension :: Int
  , tspEdgeWeightType :: Maybe TspEdgeWeightType
  , tspEdgeWeightFormat :: Maybe EdgeWeightFormat
  , tspEdgeDataFormat :: Maybe TspEdgeDataFormat
  , tspNodeCoordType :: Maybe TspNodeCoordType
  , tspNodes :: [Node]
  } deriving (Show, Eq)

data TspProblemHeader = TspProblemHeader
  { name :: String
  , dataType :: TspDataTypes
  , comment :: String
  , dimension :: Int
  , edgeWeightType :: Maybe TspEdgeWeightType
  , edgeWeightFormat :: Maybe EdgeWeightFormat
  , edgeDataFormat :: Maybe TspEdgeDataFormat
  , nodeCoordType :: Maybe TspNodeCoordType
  } deriving (Show, Eq)

data TspProblemBody = TspProblemBody
  { nodes :: [Node]
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
    , tspNodes = nodes body
    }
