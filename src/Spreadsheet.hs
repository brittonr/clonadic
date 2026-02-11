module Spreadsheet
  ( -- * Types
    Cell (..),
    CellValue (..),
    Grid,
    Coord,
    Stats (..),

    -- * Cell Constructors
    emptyCell,
    mkFormulaCell,
    mkLiteralCell,

    -- * Grid Operations
    emptyGrid,
    getCell,
    setCell,
    gridToList,
    gridFromList,

    -- * Coordinate Helpers
    coordToRef,
    refToCoord,
    colToLetter,

    -- * Formula Evaluation (via Claude)
    evaluateFormula,
    isFormula,

    -- * Stats
    emptyStats,

    -- * Autocomplete
    FormulaFunction (..),
    Suggestion (..),
    SuggestionType (..),
    formulaFunctions,
    getAutocompleteSuggestions,

    -- * Dependency Tracking
    extractCellRefs,
    findDependentCells,

    -- * Display Helpers
    cellValueToDisplay,
    showNumber,

    -- * Debug helpers
    gridContextText,
  )
where

import Clonad
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiUpper, isDigit, ord)
import Data.List (unfoldr)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import GHC.Generics (Generic)

type Coord = (Int, Int) -- (row, col), 1-indexed

data CellValue
  = CellEmpty
  | CellNumber !Double
  | CellText !Text
  | CellBoolean !Bool
  | CellError !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Cell = Cell
  { cellValue :: !CellValue,
    cellFormula :: !(Maybe Text), -- Raw formula if this is a formula cell
    cellDisplay :: !Text -- What to show in the UI
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An empty cell with no value, formula, or display text
emptyCell :: Cell
emptyCell = Cell CellEmpty Nothing T.empty

-- | Create a formula cell with the given formula, computed value, and display text
mkFormulaCell :: Text -> CellValue -> Text -> Cell
mkFormulaCell formula value = Cell value (Just formula)

-- | Create a literal cell (no formula) with the given value and display text
mkLiteralCell :: CellValue -> Text -> Cell
mkLiteralCell value = Cell value Nothing

type Grid = Map Coord Cell

data Stats = Stats
  { statsOperations :: !Int,
    statsTokensEstimate :: !Int,
    statsCostEstimate :: !Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyStats :: Stats
emptyStats = Stats 0 0 0.0

emptyGrid :: Grid
emptyGrid = Map.empty

getCell :: Coord -> Grid -> Cell
getCell = Map.findWithDefault (Cell CellEmpty Nothing "")

setCell :: Coord -> Cell -> Grid -> Grid
setCell = Map.insert

gridToList :: Grid -> [(Coord, Cell)]
gridToList = Map.toList

gridFromList :: [(Coord, Cell)] -> Grid
gridFromList = Map.fromList

-- Convert (1, 1) to "A1", (1, 2) to "B1", etc.
coordToRef :: Coord -> Text
coordToRef (row, col) = colToLetter col <> T.pack (show row)

colToLetter :: Int -> Text
colToLetter n
  | n <= 0 = T.empty
  | otherwise = T.pack . reverse $ unfoldr step n
  where
    step 0 = Nothing
    step c =
      let (q, r) = (c - 1) `divMod` 26
       in Just (toEnum (ord 'A' + r), q)

-- Convert "A1" to (1, 1), "B2" to (2, 2), etc.
refToCoord :: Text -> Maybe Coord
refToCoord ref = (,) <$> parseRow digits <*> parseCol letters
  where
    (letters, digits) = T.span (not . isDigit) ref

    parseCol :: Text -> Maybe Int
    parseCol t
      | T.null t = Nothing
      | otherwise = Just $ T.foldl' (\acc c -> acc * 26 + (ord c - ord 'A' + 1)) 0 (T.toUpper t)

    parseRow :: Text -> Maybe Int
    parseRow t = case TR.decimal t of
      Right (n, rest) | T.null rest && n > 0 -> Just n
      _ -> Nothing

isFormula :: Text -> Bool
isFormula t = T.isPrefixOf "=" (T.strip t)

-- The cursed part: Claude evaluates spreadsheet formulas
evaluateFormula :: Text -> Grid -> Clonad CellValue
evaluateFormula formulaText grid = do
  let formula = T.strip formulaText
      -- Build a clearer context: substitute values directly into explanation
      refs = extractCellRefs formula
      nonEmpty = mapMaybe (\coord -> (coordToRef coord,) <$> getCellNumericValue coord grid) refs
      valuesText =
        if null nonEmpty
          then "All cells are empty (value 0)"
          else T.intercalate ", " [ref <> "=" <> T.pack (show v) | (ref, v) <- nonEmpty]
  result <-
    clonad @(Text, Text) @Text
      ( T.unlines
          [ "Evaluate this spreadsheet formula. Return ONLY the numeric answer.",
            "",
            "Rules:",
            "- Empty cells = 0",
            "- SUM adds all values",
            "- Return just the number, nothing else",
            "",
            "Examples:",
            "=SUM(A1:A3) with A1=1, A2=2, A3=3 -> 6",
            "=SUM(A1:A3) with A1=5 -> 5",
            "=2+2 -> 4",
            "=A1*B1 with A1=10, B1=5 -> 50"
          ]
      )
      (formula <> " with " <> valuesText, "")
  pure $ parseResult result
  where
    getCellNumericValue :: Coord -> Grid -> Maybe Double
    getCellNumericValue coord g = case cellValue (getCell coord g) of
      CellNumber n -> Just n
      _ -> Nothing

gridContext :: Grid -> Text
gridContext grid = case mapMaybe formatValidCell (Map.toList grid) of
  [] -> "No cells have values yet."
  xs -> T.intercalate ", " xs
  where
    formatValidCell (coord, cell) = case cellValue cell of
      CellNumber n -> Just $ coordToRef coord <> "=" <> T.pack (show n)
      CellText t -> Just $ coordToRef coord <> "=\"" <> t <> "\""
      CellBoolean b -> Just $ coordToRef coord <> "=" <> if b then "TRUE" else "FALSE"
      _ -> Nothing

-- Exported version of gridContext for debugging
gridContextText :: Grid -> Text
gridContextText = gridContext

parseResult :: Text -> CellValue
parseResult t
  | T.null stripped = CellEmpty
  | "ERROR:" `T.isPrefixOf` upper || "ERROR " `T.isPrefixOf` upper =
      CellError (T.strip $ T.drop 6 stripped)
  | upper == "TRUE" = CellBoolean True
  | upper == "FALSE" = CellBoolean False
  | Right (n, rest) <- TR.double stripped, T.null rest = CellNumber n
  | otherwise = CellText stripped
  where
    stripped = T.strip t
    upper = T.toUpper stripped

-- Autocomplete functionality

data FormulaFunction = FormulaFunction
  { funcName :: !Text,
    funcSignature :: !Text,
    funcDescription :: !Text,
    funcCategory :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SuggestionType
  = SuggestionFunction
  | SuggestionCell
  | SuggestionRange
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Suggestion = Suggestion
  { suggestionText :: !Text,
    suggestionDisplay :: !Text,
    suggestionDescription :: !Text,
    suggestionType :: !SuggestionType,
    suggestionInsert :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

formulaFunctions :: [FormulaFunction]
formulaFunctions =
  [ -- Math functions
    FormulaFunction "SUM" "SUM(range)" "Adds all numbers in a range" "Math",
    FormulaFunction "AVERAGE" "AVERAGE(range)" "Returns the average of numbers" "Math",
    FormulaFunction "MIN" "MIN(range)" "Returns the minimum value" "Math",
    FormulaFunction "MAX" "MAX(range)" "Returns the maximum value" "Math",
    FormulaFunction "COUNT" "COUNT(range)" "Counts cells with numbers" "Math",
    FormulaFunction "ABS" "ABS(number)" "Returns absolute value" "Math",
    FormulaFunction "ROUND" "ROUND(number, digits)" "Rounds to specified digits" "Math",
    FormulaFunction "FLOOR" "FLOOR(number)" "Rounds down to nearest integer" "Math",
    FormulaFunction "CEILING" "CEILING(number)" "Rounds up to nearest integer" "Math",
    FormulaFunction "SQRT" "SQRT(number)" "Returns square root" "Math",
    FormulaFunction "POWER" "POWER(base, exponent)" "Returns base raised to power" "Math",
    FormulaFunction "MOD" "MOD(number, divisor)" "Returns remainder after division" "Math",
    FormulaFunction "PRODUCT" "PRODUCT(range)" "Multiplies all numbers in range" "Math",
    FormulaFunction "MEDIAN" "MEDIAN(range)" "Returns the median value" "Math",
    -- Logical functions
    FormulaFunction "IF" "IF(condition, true_val, false_val)" "Conditional evaluation" "Logic",
    FormulaFunction "AND" "AND(cond1, cond2, ...)" "Returns TRUE if all are true" "Logic",
    FormulaFunction "OR" "OR(cond1, cond2, ...)" "Returns TRUE if any is true" "Logic",
    FormulaFunction "NOT" "NOT(condition)" "Reverses logical value" "Logic",
    FormulaFunction "IFERROR" "IFERROR(value, error_val)" "Returns error_val if value is error" "Logic",
    FormulaFunction "ISBLANK" "ISBLANK(cell)" "Returns TRUE if cell is empty" "Logic",
    FormulaFunction "ISNUMBER" "ISNUMBER(value)" "Returns TRUE if value is number" "Logic",
    -- Text functions
    FormulaFunction "CONCAT" "CONCAT(text1, text2, ...)" "Joins text strings" "Text",
    FormulaFunction "LEFT" "LEFT(text, num_chars)" "Returns leftmost characters" "Text",
    FormulaFunction "RIGHT" "RIGHT(text, num_chars)" "Returns rightmost characters" "Text",
    FormulaFunction "MID" "MID(text, start, num_chars)" "Returns substring" "Text",
    FormulaFunction "LEN" "LEN(text)" "Returns length of text" "Text",
    FormulaFunction "UPPER" "UPPER(text)" "Converts to uppercase" "Text",
    FormulaFunction "LOWER" "LOWER(text)" "Converts to lowercase" "Text",
    FormulaFunction "TRIM" "TRIM(text)" "Removes extra spaces" "Text",
    FormulaFunction "TEXT" "TEXT(value, format)" "Formats number as text" "Text",
    -- Lookup functions
    FormulaFunction "VLOOKUP" "VLOOKUP(key, range, col, exact)" "Vertical lookup" "Lookup",
    FormulaFunction "HLOOKUP" "HLOOKUP(key, range, row, exact)" "Horizontal lookup" "Lookup",
    FormulaFunction "INDEX" "INDEX(range, row, col)" "Returns value at position" "Lookup",
    FormulaFunction "MATCH" "MATCH(value, range, type)" "Returns position of value" "Lookup",
    -- Date functions
    FormulaFunction "TODAY" "TODAY()" "Returns current date" "Date",
    FormulaFunction "NOW" "NOW()" "Returns current date and time" "Date",
    FormulaFunction "YEAR" "YEAR(date)" "Extracts year from date" "Date",
    FormulaFunction "MONTH" "MONTH(date)" "Extracts month from date" "Date",
    FormulaFunction "DAY" "DAY(date)" "Extracts day from date" "Date"
  ]

getAutocompleteSuggestions :: Text -> Grid -> Int -> Int -> [Suggestion]
getAutocompleteSuggestions input grid maxRows maxCols =
  case T.uncons (T.strip input) of
    Just ('=', rest) ->
      getSuggestionsForToken (getLastToken rest) grid maxRows maxCols
    _ -> []

getLastToken :: Text -> Text
getLastToken input =
  let delimiters = "(),+-*/^&=<>: "
      reversed = T.reverse input
      token = T.takeWhile (`notElem` T.unpack delimiters) reversed
   in T.reverse token

getSuggestionsForToken :: Text -> Grid -> Int -> Int -> [Suggestion]
getSuggestionsForToken token grid maxRows maxCols
  | T.null token = functionSuggestions ""
  | isLikelyColLetter token = cellSuggestions token grid maxRows maxCols
  | otherwise = functionSuggestions token <> cellSuggestions token grid maxRows maxCols

isLikelyColLetter :: Text -> Bool
isLikelyColLetter t =
  T.length t <= 2 && T.all isAsciiUpper (T.toUpper t)

functionSuggestions :: Text -> [Suggestion]
functionSuggestions prefix =
  map functionToSuggestion . take 8 . filter (T.isPrefixOf (T.toUpper prefix) . funcName) $ formulaFunctions

functionToSuggestion :: FormulaFunction -> Suggestion
functionToSuggestion FormulaFunction {..} =
  Suggestion
    { suggestionText = funcName,
      suggestionDisplay = funcSignature,
      suggestionDescription = funcDescription,
      suggestionType = SuggestionFunction,
      suggestionInsert = funcName <> "("
    }

cellSuggestions :: Text -> Grid -> Int -> Int -> [Suggestion]
cellSuggestions prefix grid maxRows maxCols =
  let upper = T.toUpper prefix
      nonEmptyCells = getNonEmptyCellRefs grid
      nonEmptySet = Set.fromList nonEmptyCells
      allCells = generateCellRefs maxRows maxCols
      prioritizedCells = nonEmptyCells <> filter (`Set.notMember` nonEmptySet) allCells
      matches = filter (T.isPrefixOf upper . T.toUpper) prioritizedCells
   in map cellToSuggestion (take 8 matches)

getNonEmptyCellRefs :: Grid -> [Text]
getNonEmptyCellRefs grid =
  [coordToRef coord | (coord, cell) <- Map.toList grid, cellValue cell /= CellEmpty]

generateCellRefs :: Int -> Int -> [Text]
generateCellRefs maxRows maxCols =
  [coordToRef (row, col) | row <- [1 .. maxRows], col <- [1 .. maxCols]]

cellToSuggestion :: Text -> Suggestion
cellToSuggestion ref =
  Suggestion
    { suggestionText = ref,
      suggestionDisplay = ref,
      suggestionDescription = "Cell reference",
      suggestionType = SuggestionCell,
      suggestionInsert = ref
    }

-- Dependency tracking

-- Extract all cell references from a formula text (e.g., "=A1+B2" -> [(1,1), (2,2)])
-- Also expands ranges like A1:A3 -> [A1, A2, A3]
extractCellRefs :: Text -> [Coord]
extractCellRefs = concatMap parseToken . tokenize . T.toUpper
  where
    tokenize :: Text -> [Text]
    tokenize t =
      let delims :: String
          delims = "(),+-*/^&=<> "
       in filter (not . T.null) $ T.split (`elem` delims) t

    parseToken :: Text -> [Coord]
    parseToken token
      | T.isInfixOf ":" token =
          -- Range like A1:A3
          case T.splitOn ":" token of
            [start, end] -> expandRange start end
            _ -> []
      | otherwise = maybeToList (refToCoord token)

    expandRange :: Text -> Text -> [Coord]
    expandRange start end =
      case (refToCoord start, refToCoord end) of
        (Just (r1, c1), Just (r2, c2)) ->
          [(r, c) | r <- [min r1 r2 .. max r1 r2], c <- [min c1 c2 .. max c1 c2]]
        _ -> []

-- Find all cells that depend on a given coordinate (directly or indirectly)
-- Uses Set for visited tracking to prevent infinite loops on circular references
findDependentCells :: Coord -> Grid -> [Coord]
findDependentCells changedCoord grid = go Set.empty [changedCoord]
  where
    go _ [] = []
    go visited (c : cs)
      | Set.member c visited = go visited cs
      | otherwise =
          let directDeps = findDirectDependents c grid
              newDeps = filter (`Set.notMember` visited) directDeps
           in directDeps ++ go (Set.insert c visited) (newDeps ++ cs)

-- Find cells that directly reference the given coordinate
findDirectDependents :: Coord -> Grid -> [Coord]
findDirectDependents targetCoord grid =
  [ coord
  | (coord, cell) <- Map.toList grid,
    coord /= targetCoord,
    maybe False (elem targetCoord . extractCellRefs) (cellFormula cell)
  ]

-- | Format a number for display, showing integers without decimal places
showNumber :: Double -> String
showNumber n
  | n == fromIntegral (round n :: Integer) = show (round n :: Integer)
  | otherwise = show n

-- | Convert a CellValue to its display text representation
cellValueToDisplay :: CellValue -> Text
cellValueToDisplay = \case
  CellEmpty -> T.empty
  CellNumber n -> T.pack $ showNumber n
  CellText t -> t
  CellBoolean b -> if b then "TRUE" else "FALSE"
  CellError e -> "ERROR: " <> e
