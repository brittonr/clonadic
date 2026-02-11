module Spreadsheet
  ( -- * Coordinate Types
    Row (..),
    Col (..),
    Coord (..),
    mkRow,
    mkCol,
    mkCoord,

    -- * Cell Types
    Cell (..),
    CellValue (..),
    FormulaError (..),
    Grid (..),
    Stats (..),

    -- * Error Formatting
    formatFormulaError,

    -- * Cell Constructors
    emptyCell,
    mkFormulaCell,
    mkLiteralCell,

    -- * Grid Operations
    emptyGrid,
    getCellOrEmpty,
    setCell,
    deleteCell,
    gridFilter,
    gridLookup,
    gridKeys,
    gridToList,
    gridFromList,

    -- * Coordinate Helpers
    coordToRef,
    refToCoord,
    colToLetter,

    -- * Formula Evaluation (via Claude)
    evaluateFormula,
    isFormula,
    isNaturalLanguage,

    -- * Stats
    emptyStats,
    singleOpStats,

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
import Control.Applicative (some, (<|>))
import Control.Monad (guard)
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..), ToJSONKey (..), ToJSONKeyFunction (..))
import Data.Aeson.Encoding qualified as E
import Data.Aeson.Key qualified as Key
import Data.Attoparsec.Text qualified as A
import Data.Char (isAsciiUpper, ord)
import Data.Functor (($>))
import Data.List qualified as List (unfoldr)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import GHC.Generics (Generic)

-- | A 1-indexed row number in the spreadsheet
newtype Row = Row {unRow :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Enum)
  deriving anyclass (ToJSON, FromJSON)

-- | A 1-indexed column number in the spreadsheet
newtype Col = Col {unCol :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Enum)
  deriving anyclass (ToJSON, FromJSON)

-- | A cell coordinate combining Row and Col
data Coord = Coord
  { coordRow :: !Row,
    coordCol :: !Col
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Smart constructor for Row (must be >= 1)
mkRow :: Int -> Maybe Row
mkRow n = guard (n >= 1) $> Row n

-- | Smart constructor for Col (must be >= 1)
mkCol :: Int -> Maybe Col
mkCol n = guard (n >= 1) $> Col n

-- | Smart constructor for Coord with validation
mkCoord :: Int -> Int -> Maybe Coord
mkCoord r c = Coord <$> mkRow r <*> mkCol c

-- JSON key instances for Map Coord usage
instance ToJSONKey Coord where
  toJSONKey = ToJSONKeyText (Key.fromText . render) (E.text . render)
    where
      render (Coord (Row r) (Col c)) = T.pack (show r <> "," <> show c)

instance FromJSONKey Coord where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case T.splitOn "," t of
      [rText, cText] -> do
        r <- parseIntText rText
        c <- parseIntText cText
        case mkCoord r c of
          Just coord -> pure coord
          Nothing -> fail "Coordinates must be >= 1"
      _ -> fail "Expected 'row,col' format"
    where
      parseIntText txt = case TR.decimal txt of
        Right (n, "") -> pure n
        _ -> fail $ "Invalid integer: " <> T.unpack txt

-- | Structured formula error types for better error categorization
data FormulaError
  = InvalidReference !Text
  | CircularDependency ![Coord]
  | DivisionByZero
  | TypeError !Text !Text -- expected, actual
  | SyntaxError !Text
  | EvaluationError !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Format a FormulaError for display
formatFormulaError :: FormulaError -> Text
formatFormulaError = \case
  InvalidReference ref -> "Invalid reference: " <> ref
  CircularDependency coords -> "Circular dependency: " <> T.intercalate " -> " (map coordToRef coords)
  DivisionByZero -> "Division by zero"
  TypeError expected actual -> "Type error: expected " <> expected <> ", got " <> actual
  SyntaxError msg -> "Syntax error: " <> msg
  EvaluationError msg -> msg

data CellValue
  = CellEmpty
  | CellNumber !Double
  | CellText !Text
  | CellBoolean !Bool
  | CellError !FormulaError
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Cell = Cell
  { cellValue :: !CellValue,
    cellFormula :: !(Maybe Text),
    cellDisplay :: !Text
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

-- | Grid is a newtype wrapper around Map to provide encapsulation
-- and enable type-safe operations with derived instances
newtype Grid = Grid {unGrid :: Map Coord Cell}
  deriving stock (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (ToJSON, FromJSON)

data Stats = Stats
  { statsOperations :: !Int,
    statsTokensEstimate :: !Int,
    statsCostEstimate :: !Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup Stats where
  Stats o1 t1 c1 <> Stats o2 t2 c2 = Stats (o1 + o2) (t1 + t2) (c1 + c2)

instance Monoid Stats where
  mempty = Stats 0 0 0.0

emptyStats :: Stats
emptyStats = mempty

-- | Create stats for a single operation with the given tokens and cost
singleOpStats :: Int -> Double -> Stats
singleOpStats = Stats 1

emptyGrid :: Grid
emptyGrid = Grid Map.empty

-- | Get cell at coordinate, returning emptyCell if not present
-- For explicit Maybe behavior, use gridLookup instead
getCellOrEmpty :: Coord -> Grid -> Cell
getCellOrEmpty coord = fromMaybe emptyCell . gridLookup coord

setCell :: Coord -> Cell -> Grid -> Grid
setCell coord cell = Grid . Map.insert coord cell . unGrid

-- | Delete a cell from the grid
deleteCell :: Coord -> Grid -> Grid
deleteCell coord = Grid . Map.delete coord . unGrid

-- | Filter cells in the grid by predicate
gridFilter :: (Cell -> Bool) -> Grid -> Grid
gridFilter p = Grid . Map.filter p . unGrid

-- | Lookup a cell in the grid (returns Nothing if not present)
gridLookup :: Coord -> Grid -> Maybe Cell
gridLookup coord = Map.lookup coord . unGrid

-- | Get all coordinates in the grid
gridKeys :: Grid -> [Coord]
gridKeys = Map.keys . unGrid

gridToList :: Grid -> [(Coord, Cell)]
gridToList = Map.toList . unGrid

gridFromList :: [(Coord, Cell)] -> Grid
gridFromList = Grid . Map.fromList

-- | Convert Coord to spreadsheet reference (e.g., Coord 1 1 -> "A1")
coordToRef :: Coord -> Text
coordToRef (Coord (Row row) col) = colToLetter col <> T.pack (show row)

-- | Convert column number to letter(s) (e.g., 1 -> "A", 27 -> "AA")
colToLetter :: Col -> Text
colToLetter (Col n)
  | n <= 0 = T.empty
  | otherwise = T.pack . reverse $ List.unfoldr step n
  where
    step 0 = Nothing
    step c =
      let (q, r) = (c - 1) `divMod` 26
       in Just (toEnum (ord 'A' + r), q)

-- Attoparsec parsers for cell references

-- | Parse column letters (A, B, ..., Z, AA, AB, ...)
columnP :: A.Parser Col
columnP = do
  letters <- some (A.satisfy isAsciiUpper)
  pure . Col $ foldl' (\acc c -> acc * 26 + (ord c - ord 'A' + 1)) 0 letters

-- | Parse row number (positive integer >= 1)
rowP :: A.Parser Row
rowP = do
  n <- A.decimal
  guard (n >= 1)
  pure (Row n)

-- | Parse a cell coordinate (e.g., A1, AA99)
coordP :: A.Parser Coord
coordP = flip Coord <$> columnP <*> rowP

-- | Parse a range (e.g., A1:B3)
rangeP :: A.Parser [Coord]
rangeP = expandRangeCoords <$> coordP <* A.char ':' <*> coordP

-- | Expand a range from start to end coordinates
expandRangeCoords :: Coord -> Coord -> [Coord]
expandRangeCoords (Coord (Row r1) (Col c1)) (Coord (Row r2) (Col c2)) =
  [Coord (Row r) (Col c) | r <- [min r1 r2 .. max r1 r2], c <- [min c1 c2 .. max c1 c2]]

-- | Parse spreadsheet reference to Coord (e.g., "A1" -> Just (Coord 1 1))
refToCoord :: Text -> Maybe Coord
refToCoord = either (const Nothing) Just . A.parseOnly (coordP <* A.endOfInput) . T.toUpper

isFormula :: Text -> Bool
isFormula t = T.isPrefixOf "=" (T.strip t)

-- | Detect if a formula appears to be natural language rather than traditional spreadsheet syntax
isNaturalLanguage :: Text -> Bool
isNaturalLanguage formula =
  let stripped = T.toLower $ T.strip formula
      hasQuestionWord =
        any
          (`T.isPrefixOf` stripped)
          ["what", "how", "calculate", "compute", "find", "is ", "are ", "convert", "show", "tell", "give"]
      hasTraditionalPattern =
        any
          (\f -> T.toUpper f `T.isInfixOf` T.toUpper stripped)
          ["SUM(", "AVERAGE(", "IF(", "VLOOKUP(", "COUNT(", "MIN(", "MAX(", "CONCAT("]
      wordCount = length $ T.words stripped
   in (hasQuestionWord || wordCount > 4) && not hasTraditionalPattern

-- | Parse Excel-style quoted string literal: ="text" -> Just text
-- Returns the unquoted content if the formula is a simple quoted string
parseQuotedLiteral :: Text -> Maybe Text
parseQuotedLiteral t
  | T.length stripped >= 2,
    T.head stripped == '"',
    T.last stripped == '"' =
      Just $ T.init $ T.tail stripped
  | otherwise = Nothing
  where
    stripped = T.strip t

-- | Evaluate a formula using the LLM
evaluateFormula :: Text -> Grid -> Clonad CellValue
evaluateFormula formulaText grid = do
  let formula = T.drop 1 $ T.strip formulaText -- Remove leading =
  -- Check for quoted string literal first (Excel-style ="text")
  case parseQuotedLiteral formula of
    Just literal -> pure $ CellText literal
    Nothing ->
      if isNaturalLanguage formula
        then evaluateNaturalLanguage formula grid
        else evaluateTraditionalFormula formulaText grid

-- | Check if a natural language query references spreadsheet cells
referencesSpreadsheetCells :: Text -> Bool
referencesSpreadsheetCells = not . null . extractCellRefs

-- | Evaluate a natural language request, routing to appropriate handler
evaluateNaturalLanguage :: Text -> Grid -> Clonad CellValue
evaluateNaturalLanguage request grid
  | referencesSpreadsheetCells request = evaluateSpreadsheetNL request grid
  | otherwise = evaluateGeneralKnowledge request

-- | Evaluate a general knowledge question (no cell references)
evaluateGeneralKnowledge :: Text -> Clonad CellValue
evaluateGeneralKnowledge request = do
  result <-
    clonad @Text @Text
      ( T.unlines
          [ "Answer this question directly and concisely.",
            "",
            "Rules:",
            "- Return ONLY the answer (number, text, or TRUE/FALSE)",
            "- For measurements, ALWAYS include units (km, miles, kg, etc.)",
            "- For comparisons, include context (e.g., '109x larger')",
            "- For factual questions, give a brief factual answer",
            "- If the question cannot be answered, return ERROR: <reason>",
            "",
            "Examples:",
            "\"how big is the sun\" -> 1.39 million km diameter",
            "\"how big is the sun in miles\" -> 864,000 miles diameter",
            "\"what is the capital of France\" -> Paris",
            "\"is 17 a prime number\" -> TRUE",
            "\"what is 2+2\" -> 4",
            "\"how big is the sun vs earth\" -> 109x larger (sun is 1.39M km, earth is 12,742 km)"
          ]
      )
      request
  pure $ parseResult result

-- | Evaluate a natural language request that references spreadsheet cells
evaluateSpreadsheetNL :: Text -> Grid -> Clonad CellValue
evaluateSpreadsheetNL request grid = do
  let nlContext = buildNLGridContext grid
  result <-
    clonad @(Text, Text) @Text
      ( T.unlines
          [ "You are a spreadsheet assistant. Interpret this natural language request.",
            "Analyze the request and spreadsheet data, then compute the answer.",
            "",
            "Context format: CELL=VALUE (from: original formula)",
            "The '(from: ...)' shows how the value was calculated, preserving units and meaning.",
            "",
            "Rules:",
            "- Return ONLY the computed result (number, text, or TRUE/FALSE)",
            "- For numeric results, include units when the source has units",
            "- Use the '(from: ...)' provenance to understand what each cell represents",
            "- For meta-questions about units or meaning, look at the provenance",
            "- If unclear or impossible, return ERROR: <reason>",
            "",
            "Examples:",
            "\"what's the sum of all values?\" with A1=100, A2=200 -> 300",
            "\"is A1 greater than B1?\" with A1=50, B1=30 -> TRUE",
            "\"convert A1 to fahrenheit\" with A1=100 (from: celsius temp) -> 212",
            "\"what unit is A1\" with A1=1000000 (from: how big is the sun in km) -> km",
            "\"what does A1 represent\" with A1=870000 (from: convert sun diameter to miles) -> sun diameter in miles",
            "\"how big is A1 vs the earth\" with A1=870000 (from: sun in miles) -> about 109x larger (Earth is ~7,918 miles diameter)",
            "\"convert A1 to miles\" with A1=\"1.4 million km\" -> 870,000 miles",
            "\"what percent is A1 of B1?\" with A1=25, B1=100 -> 25%"
          ]
      )
      (request, nlContext)
  pure $ parseResult result

-- | Build a richer grid context for natural language evaluation
-- Includes both computed values and original formulas for provenance
buildNLGridContext :: Grid -> Text
buildNLGridContext grid
  | null cellInfo = "The spreadsheet is empty."
  | otherwise = T.intercalate ", " cellInfo
  where
    cellInfo =
      [ formatCellWithProvenance coord cell
      | (coord, cell) <- gridToList grid,
        cellValue cell /= CellEmpty
      ]

    formatCellWithProvenance coord cell =
      let ref = coordToRef coord
          valueStr = formatCellValue cell
          -- Include formula for provenance when it differs from value
          provenanceStr = case cellFormula cell of
            Just formula
              | isFormula formula ->
                  " (from: " <> T.drop 1 (T.strip formula) <> ")"
            _ -> ""
       in ref <> "=" <> valueStr <> provenanceStr

    formatCellValue cell = case cellValue cell of
      CellNumber n -> T.pack (showNumber n)
      CellText t -> "\"" <> t <> "\""
      CellBoolean b -> if b then "TRUE" else "FALSE"
      _ -> ""

-- | Evaluate a traditional spreadsheet formula
evaluateTraditionalFormula :: Text -> Grid -> Clonad CellValue
evaluateTraditionalFormula formulaText grid = do
  let formula = T.strip formulaText
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
    getCellNumericValue coord g = case cellValue (getCellOrEmpty coord g) of
      CellNumber n -> Just n
      _ -> Nothing

gridContext :: Grid -> Text
gridContext grid
  | null validCells = "No cells have values yet."
  | otherwise = T.intercalate ", " validCells
  where
    validCells = foldMap (maybeToList . formatCell) (gridToList grid)

    formatCell (coord, cell) = do
      valueText <- cellValueToContext (cellValue cell)
      pure $ coordToRef coord <> "=" <> valueText

    cellValueToContext = \case
      CellNumber n -> Just $ T.pack (show n)
      CellText t -> Just $ "\"" <> t <> "\""
      CellBoolean b -> Just $ if b then "TRUE" else "FALSE"
      _ -> Nothing

-- | Exported version of gridContext for debugging
gridContextText :: Grid -> Text
gridContextText = gridContext

-- | Parse LLM result text into a CellValue
parseResult :: Text -> CellValue
parseResult t
  | T.null stripped = CellEmpty
  | Just errMsg <- parseError stripped = CellError (classifyError errMsg)
  | upper == "TRUE" = CellBoolean True
  | upper == "FALSE" = CellBoolean False
  | Right (n, rest) <- TR.double stripped, T.null rest = CellNumber n
  | otherwise = CellText stripped
  where
    stripped = T.strip t
    upper = T.toUpper stripped
    parseError s = T.strip <$> (T.stripPrefix "ERROR:" s <|> T.stripPrefix "ERROR " s)

    -- Classify error message into structured FormulaError type
    classifyError :: Text -> FormulaError
    classifyError msg
      | T.isInfixOf "division by zero" lowerMsg = DivisionByZero
      | T.isInfixOf "divide by zero" lowerMsg = DivisionByZero
      | T.isInfixOf "circular" lowerMsg = CircularDependency []
      | T.isInfixOf "invalid reference" lowerMsg = InvalidReference msg
      | T.isInfixOf "type" lowerMsg = TypeError "number" "text"
      | T.isInfixOf "syntax" lowerMsg = SyntaxError msg
      | otherwise = EvaluationError msg
      where
        lowerMsg = T.toLower msg

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

-- | Math functions for numerical operations
mathFunctions :: [FormulaFunction]
mathFunctions =
  [ FormulaFunction "SUM" "SUM(range)" "Adds all numbers in a range" "Math",
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
    FormulaFunction "MEDIAN" "MEDIAN(range)" "Returns the median value" "Math"
  ]

-- | Logic functions for conditional operations
logicFunctions :: [FormulaFunction]
logicFunctions =
  [ FormulaFunction "IF" "IF(condition, true_val, false_val)" "Conditional evaluation" "Logic",
    FormulaFunction "AND" "AND(cond1, cond2, ...)" "Returns TRUE if all are true" "Logic",
    FormulaFunction "OR" "OR(cond1, cond2, ...)" "Returns TRUE if any is true" "Logic",
    FormulaFunction "NOT" "NOT(condition)" "Reverses logical value" "Logic",
    FormulaFunction "IFERROR" "IFERROR(value, error_val)" "Returns error_val if value is error" "Logic",
    FormulaFunction "ISBLANK" "ISBLANK(cell)" "Returns TRUE if cell is empty" "Logic",
    FormulaFunction "ISNUMBER" "ISNUMBER(value)" "Returns TRUE if value is number" "Logic"
  ]

-- | Text functions for string manipulation
textFunctions :: [FormulaFunction]
textFunctions =
  [ FormulaFunction "CONCAT" "CONCAT(text1, text2, ...)" "Joins text strings" "Text",
    FormulaFunction "LEFT" "LEFT(text, num_chars)" "Returns leftmost characters" "Text",
    FormulaFunction "RIGHT" "RIGHT(text, num_chars)" "Returns rightmost characters" "Text",
    FormulaFunction "MID" "MID(text, start, num_chars)" "Returns substring" "Text",
    FormulaFunction "LEN" "LEN(text)" "Returns length of text" "Text",
    FormulaFunction "UPPER" "UPPER(text)" "Converts to uppercase" "Text",
    FormulaFunction "LOWER" "LOWER(text)" "Converts to lowercase" "Text",
    FormulaFunction "TRIM" "TRIM(text)" "Removes extra spaces" "Text",
    FormulaFunction "TEXT" "TEXT(value, format)" "Formats number as text" "Text"
  ]

-- | Lookup functions for finding values in ranges
lookupFunctions :: [FormulaFunction]
lookupFunctions =
  [ FormulaFunction "VLOOKUP" "VLOOKUP(key, range, col, exact)" "Vertical lookup" "Lookup",
    FormulaFunction "HLOOKUP" "HLOOKUP(key, range, row, exact)" "Horizontal lookup" "Lookup",
    FormulaFunction "INDEX" "INDEX(range, row, col)" "Returns value at position" "Lookup",
    FormulaFunction "MATCH" "MATCH(value, range, type)" "Returns position of value" "Lookup"
  ]

-- | Date functions for date/time operations
dateFunctions :: [FormulaFunction]
dateFunctions =
  [ FormulaFunction "TODAY" "TODAY()" "Returns current date" "Date",
    FormulaFunction "NOW" "NOW()" "Returns current date and time" "Date",
    FormulaFunction "YEAR" "YEAR(date)" "Extracts year from date" "Date",
    FormulaFunction "MONTH" "MONTH(date)" "Extracts month from date" "Date",
    FormulaFunction "DAY" "DAY(date)" "Extracts day from date" "Date"
  ]

-- | Natural language patterns for intuitive formula entry
nlPatterns :: [FormulaFunction]
nlPatterns =
  [ FormulaFunction "what is" "what is..." "Ask a question about data" "Natural Language",
    FormulaFunction "sum of" "sum of A1 to A5" "Calculate sum naturally" "Natural Language",
    FormulaFunction "is...greater" "is A1 greater than B1?" "Compare values" "Natural Language",
    FormulaFunction "convert" "convert A1 from X to Y" "Unit conversion" "Natural Language",
    FormulaFunction "is...prime" "is A1 a prime number?" "Check if prime" "Natural Language",
    FormulaFunction "what percent" "what percent is A1 of B1?" "Calculate percentage" "Natural Language"
  ]

-- | All supported formula functions, combined using Semigroup
formulaFunctions :: [FormulaFunction]
formulaFunctions =
  nlPatterns -- Natural language patterns at top for visibility
    <> mathFunctions
    <> logicFunctions
    <> textFunctions
    <> lookupFunctions
    <> dateFunctions

getAutocompleteSuggestions :: Text -> Grid -> Int -> Int -> [Suggestion]
getAutocompleteSuggestions input grid maxRows maxCols =
  case T.uncons (T.strip input) of
    Just ('=', rest) ->
      getSuggestionsForToken (getLastToken rest) grid maxRows maxCols
    _ -> []

-- | Extract the last token from input using point-free composition
getLastToken :: Text -> Text
getLastToken = T.reverse . T.takeWhile (not . isDelimiter) . T.reverse
  where
    isDelimiter c = c `elem` ("(),+-*/^&=<>: " :: String)

getSuggestionsForToken :: Text -> Grid -> Int -> Int -> [Suggestion]
getSuggestionsForToken token grid maxRows maxCols
  | T.null token = functionSuggestions ""
  | isLikelyColLetter token = cellSuggestions token grid maxRows maxCols
  | otherwise = functionSuggestions token <> cellSuggestions token grid maxRows maxCols

isLikelyColLetter :: Text -> Bool
isLikelyColLetter t =
  T.length t <= 2 && T.all isAsciiUpper (T.toUpper t)

-- | Smart constructor for Suggestion to reduce boilerplate
mkSuggestion :: Text -> Text -> Text -> SuggestionType -> Text -> Suggestion
mkSuggestion text display desc typ insert =
  Suggestion
    { suggestionText = text,
      suggestionDisplay = display,
      suggestionDescription = desc,
      suggestionType = typ,
      suggestionInsert = insert
    }

-- | Maximum number of suggestions to return for autocomplete
maxSuggestions :: Int
maxSuggestions = 8

functionSuggestions :: Text -> [Suggestion]
functionSuggestions prefix =
  map functionToSuggestion . take maxSuggestions . filter (T.isPrefixOf (T.toUpper prefix) . funcName) $ formulaFunctions

functionToSuggestion :: FormulaFunction -> Suggestion
functionToSuggestion FormulaFunction {..} =
  mkSuggestion funcName funcSignature funcDescription SuggestionFunction (funcName <> "(")

cellSuggestions :: Text -> Grid -> Int -> Int -> [Suggestion]
cellSuggestions prefix grid maxRows maxCols =
  let upper = T.toUpper prefix
      nonEmptyCells = getNonEmptyCellRefs grid
      nonEmptySet = Set.fromList nonEmptyCells
      allCells = generateCellRefs maxRows maxCols
      prioritizedCells = nonEmptyCells <> filter (`Set.notMember` nonEmptySet) allCells
      matches = filter (T.isPrefixOf upper . T.toUpper) prioritizedCells
   in map cellToSuggestion (take maxSuggestions matches)

getNonEmptyCellRefs :: Grid -> [Text]
getNonEmptyCellRefs grid =
  [coordToRef coord | (coord, cell) <- gridToList grid, cellValue cell /= CellEmpty]

generateCellRefs :: Int -> Int -> [Text]
generateCellRefs maxRows maxCols =
  [coordToRef (Coord (Row row) (Col col)) | row <- [1 .. maxRows], col <- [1 .. maxCols]]

cellToSuggestion :: Text -> Suggestion
cellToSuggestion ref = mkSuggestion ref ref "Cell reference" SuggestionCell ref

-- Dependency tracking

-- | Extract all cell references from a formula text
-- Handles both single refs (A1) and ranges (A1:A3)
extractCellRefs :: Text -> [Coord]
extractCellRefs = concatMap parseToken . tokenize . T.toUpper
  where
    tokenize :: Text -> [Text]
    tokenize t =
      let delims :: String
          delims = "(),+-*/^&=<> "
       in filter (not . T.null) $ T.split (`elem` delims) t

    -- Use Attoparsec to parse tokens as ranges or single coords
    parseToken :: Text -> [Coord]
    parseToken token = case A.parseOnly (rangeP <* A.endOfInput) token of
      Right coords -> coords
      Left _ -> maybeToList (refToCoord token)

-- | Find all cells that depend on a given coordinate (directly or indirectly)
-- Uses Set for visited tracking to prevent infinite loops on circular references
-- Uses Data.Sequence for O(1) queue operations instead of O(n) list append
findDependentCells :: Coord -> Grid -> [Coord]
findDependentCells changedCoord grid = go Set.empty (Seq.singleton changedCoord) []
  where
    go _ Seq.Empty acc = acc
    go visited (c :<| cs) acc
      | Set.member c visited = go visited cs acc
      | otherwise =
          let directDeps = findDirectDependents c grid
              newVisited = Set.insert c visited
              unvisitedDeps = filter (`Set.notMember` newVisited) directDeps
              newQueue = foldl' (Seq.|>) cs unvisitedDeps
           in go newVisited newQueue (acc <> directDeps)

-- | Find cells that directly reference the given coordinate
findDirectDependents :: Coord -> Grid -> [Coord]
findDirectDependents targetCoord grid =
  Map.keys $ Map.filterWithKey isDependent (unGrid grid)
  where
    isDependent coord cell =
      coord /= targetCoord
        && maybe False (elem targetCoord . extractCellRefs) (cellFormula cell)

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
  CellError e -> "ERROR: " <> formatFormulaError e
