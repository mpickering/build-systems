{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Example.Spreadsheet where

import Control.Monad.State
import Data.GADT.Compare
import Control.Monad (forM_)
import Data.Type.Equality
import qualified Data.Map as Map
import Data.Char (isDigit)
import System.Shake

import Data.GADT.Show

-- | Cell reference (e.g., "A1", "B2")
type CellRef = String

-- | Cell value type (tagged union)
data CellValue =
    NumericValue Double
  | StringValue String
  deriving (Show, Eq)

-- | Expression ADT for representing spreadsheet formulas
data Expression =
    Literal Double                 -- A numeric literal (e.g., 42)
  | TextLiteral String             -- A text literal (e.g., "hello")
  | CellReference CellRef          -- Reference to another cell (e.g., A1)
  | BinaryOp Op Expression Expression  -- Binary operation (e.g., A1 + B2)
  | FunctionCall Function [Expression] -- Function call (e.g., SUM(A1:A3))
  | Range CellRef CellRef          -- Cell range (e.g., A1:A3)
  deriving (Show, Eq)

-- | Binary operations
data Op = Add | Subtract | Multiply | Divide
  deriving (Show, Eq)

-- | Supported functions
data Function = Sum | Average | Max | Indirect
  deriving (Show, Eq)

-- | Cell content types
data CellContent =
    ValueCell CellValue            -- A value (numeric or string)
  | FormulaCell Expression         -- A formula using the Expression ADT
  deriving (Show, Eq)

-- | Our spreadsheet structure
data Spreadsheet = Spreadsheet (Map.Map CellRef CellContent)
  deriving (Show, Eq)

-- | Key type for the build system
data Key a where
    CellKey :: CellRef -> Key CellValue      -- Key to get a cell's value
    SpreadsheetKey :: Key Spreadsheet     -- Key to get the whole spreadsheet

-- | GShow instance for Key to enable pretty printing
instance GShow Key where
    gshowsPrec p (CellKey ref) = showParen (p > 10) $
        showString "CellKey " . showsPrec 11 ref
    gshowsPrec _ SpreadsheetKey = showString "SpreadsheetKey"

instance GEq Key where
    geq (CellKey a) (CellKey b)
        | a == b    = Just Refl
        | otherwise = Nothing
    geq SpreadsheetKey SpreadsheetKey = Just Refl
    geq _ _ = Nothing

instance GCompare Key where
    gcompare (CellKey a) (CellKey b) = case compare a b of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT
    gcompare SpreadsheetKey SpreadsheetKey = GEQ
    gcompare SpreadsheetKey _ = GLT
    gcompare _ SpreadsheetKey = GGT

-- | Rule matching criteria
data RuleMatch o where
    MatchCell :: RuleMatch CellValue
    MatchSpreadsheet :: RuleMatch Spreadsheet

instance GEq RuleMatch where
    geq MatchCell MatchCell = Just Refl
    geq MatchSpreadsheet MatchSpreadsheet = Just Refl
    geq _ _ = Nothing

instance GCompare RuleMatch where
    gcompare MatchCell MatchCell = GEQ
    gcompare MatchSpreadsheet MatchSpreadsheet = GEQ
    gcompare MatchCell MatchSpreadsheet = GLT
    gcompare MatchSpreadsheet MatchCell = GGT

-- | Map a Key to its corresponding RuleMatch
keyToMatch :: Key a -> RuleMatch a
keyToMatch (CellKey _) = MatchCell
keyToMatch SpreadsheetKey = MatchSpreadsheet

-- | Convert a key to string for stats tracking
keyToString :: Key a -> String
keyToString (CellKey ref) = "Cell:" ++ ref
keyToString SpreadsheetKey = "Spreadsheet"

-- | Create an initial spreadsheet
initSpreadsheet :: Key Spreadsheet -> Build RuleMatch Key Spreadsheet
initSpreadsheet _ = do
    liftIO $ putStrLn "Initializing spreadsheet..."
    -- Define some cells with values and formulas
    let cells = Map.fromList
          [ ("A1", ValueCell (NumericValue 10))
          , ("A2", ValueCell (NumericValue 20))
          , ("A3", FormulaCell (BinaryOp Add (CellReference "A1") (CellReference "A2")))  -- A1+A2
          , ("B1", ValueCell (NumericValue 5))
          , ("B2", FormulaCell (BinaryOp Multiply (CellReference "A1") (CellReference "B1")))  -- A1*B1
          , ("B3", FormulaCell (BinaryOp Subtract (CellReference "A3") (CellReference "B2")))  -- A3-B2
          , ("C1", FormulaCell (BinaryOp Multiply (CellReference "B3") (Literal 2)))  -- B3*2
          , ("C2", FormulaCell (FunctionCall Sum [Range "A1" "A3"]))  -- SUM(A1:A3)
          , ("C3", FormulaCell (FunctionCall Average [Range "B1" "B3"]))  -- AVERAGE(B1:B3)
          , ("D1", FormulaCell (FunctionCall Max [CellReference "A1", CellReference "A2", CellReference "B1"]))  -- MAX(A1,A2,B1)
          , ("D2", ValueCell (StringValue "A1"))  -- This cell contains the value 1
          , ("D3", FormulaCell (FunctionCall Indirect [CellReference "D2"]))  -- INDIRECT(D2) which should resolve to A1
          , ("E1", ValueCell (StringValue "Hello"))  -- A string cell
          , ("E2", ValueCell (StringValue "World"))  -- Another string cell
          , ("E3", FormulaCell (BinaryOp Add (CellReference "E1") (CellReference "E2")))  -- E1+E2 (string concatenation)
          ]
    return $ Spreadsheet cells

-- | Evaluate a cell
evaluateCell :: Key CellValue -> Build RuleMatch Key CellValue
evaluateCell (CellKey ref) = do
    liftIO $ putStrLn $ "Evaluating cell: " ++ ref

    -- Get the spreadsheet
    Just (Spreadsheet cells) <- build keyToMatch SpreadsheetKey

    -- Look up the cell content
    case Map.lookup ref cells of
        Just (ValueCell value) -> do
            -- Simple value, just return it
            return value

        Just (FormulaCell expr) -> do
            -- Evaluate the formula
            liftIO $ putStrLn $ "  Formula: " ++ showExpression expr
            evalExpression expr

        Nothing -> do
            liftIO $ putStrLn $ "  Cell not found: " ++ ref
            return (NumericValue 0)  -- Default value for undefined cells

-- | Convert Expression to string for display
showExpression :: Expression -> String
showExpression (Literal n) = show n
showExpression (TextLiteral t) = t
showExpression (CellReference ref) = ref
showExpression (BinaryOp op e1 e2) =
    "(" ++ showExpression e1 ++ " " ++ showOp op ++ " " ++ showExpression e2 ++ ")"
  where
    showOp Add = "+"
    showOp Subtract = "-"
    showOp Multiply = "*"
    showOp Divide = "/"
showExpression (FunctionCall func args) =
    showFunc func ++ "(" ++ concatMap showFuncArg (zip args (repeat "," ++ repeat "")) ++ ")"
  where
    showFunc Sum = "SUM"
    showFunc Average = "AVERAGE"
    showFunc Max = "MAX"
    showFunc Indirect = "INDIRECT"
    showFuncArg (arg, sep) = showExpression arg ++ if sep == "" then "" else sep
showExpression (Range start end) = start ++ ":" ++ end

-- | Evaluate an expression
evalExpression :: Expression -> Build RuleMatch Key CellValue
evalExpression (Literal n) = return (NumericValue n)
evalExpression (TextLiteral s) = return (StringValue s)
evalExpression (CellReference ref) = do
    Just value <- build keyToMatch (CellKey ref)
    return value
evalExpression (BinaryOp op e1 e2) = do
    v1 <- evalExpression e1
    v2 <- evalExpression e2
    case (v1, v2) of
        (NumericValue n1, NumericValue n2) ->
            return $ NumericValue $ case op of
                Add      -> n1 + n2
                Subtract -> n1 - n2
                Multiply -> n1 * n2
                Divide   -> n1 / n2
        (StringValue s1, StringValue s2) ->
            -- For strings, only Add is supported (concatenation)
            case op of
                Add -> return $ StringValue (s1 ++ s2)
                _   -> return $ NumericValue 0 -- Error, return default
        (StringValue s, NumericValue n) ->
            -- Try to convert string to number for non-Add operations
            case op of
                Add -> return $ StringValue (s ++ show n)
                _   -> case reads s of
                        [(num, "")] -> evalExpression (BinaryOp op (Literal num) (Literal n))
                        _           -> return $ NumericValue 0 -- Error, return default
        (NumericValue n, StringValue s) ->
            -- Similar to above but reversed
            case op of
                Add -> return $ StringValue (show n ++ s)
                _   -> case reads s of
                        [(num, "")] -> evalExpression (BinaryOp op (Literal n) (Literal num))
                        _           -> return $ NumericValue 0 -- Error, return default
evalExpression (FunctionCall func args) = case func of
    Sum -> evalSum args
    Average -> evalAverage args
    Max -> evalMax args
    Indirect -> evalIndirect args
evalExpression (Range start end) = do
    -- When a range is evaluated directly, return the sum
    let cellRefs = expandRange start end
    values <- mapM (build keyToMatch . CellKey) cellRefs
    -- Sum only numeric values
    let numValues = [n | Just (NumericValue n) <- values]
    return $ NumericValue (sum numValues)

-- | Evaluate a SUM function
evalSum :: [Expression] -> Build RuleMatch Key CellValue
evalSum [] = return (NumericValue 0)
evalSum [Range start end] = do
    let cellRefs = expandRange start end
    values <- mapM (build keyToMatch . CellKey) cellRefs
    -- Extract numeric values only
    let numValues = [n | Just (NumericValue n) <- values]
    return $ NumericValue (sum numValues)
evalSum exprs = do
    values <- mapM evalExpression exprs
    -- Extract numeric values only
    let numValues = [n | NumericValue n <- values]
    return $ NumericValue (sum numValues)

-- | Evaluate an AVERAGE function
evalAverage :: [Expression] -> Build RuleMatch Key CellValue
evalAverage [] = return (NumericValue 0)
evalAverage [Range start end] = do
    let cellRefs = expandRange start end
    values <- mapM (build keyToMatch . CellKey) cellRefs
    -- Extract numeric values only
    let numValues = [n | Just (NumericValue n) <- values]
    if null numValues
        then return $ NumericValue 0
        else return $ NumericValue (sum numValues / fromIntegral (length numValues))
evalAverage exprs = do
    values <- mapM evalExpression exprs
    -- Extract numeric values only
    let numValues = [n | NumericValue n <- values]
    if null numValues
        then return $ NumericValue 0
        else return $ NumericValue (sum numValues / fromIntegral (length numValues))

-- | Evaluate a MAX function
evalMax :: [Expression] -> Build RuleMatch Key CellValue
evalMax [] = return (NumericValue 0)
evalMax [Range start end] = do
    let cellRefs = expandRange start end
    values <- mapM (build keyToMatch . CellKey) cellRefs
    -- Extract numeric values only
    let numValues = [n | Just (NumericValue n) <- values]
    if null numValues
        then return $ NumericValue 0
        else return $ NumericValue (maximum numValues)
evalMax exprs = do
    values <- mapM evalExpression exprs
    -- Extract numeric values only
    let numValues = [n | NumericValue n <- values]
    if null numValues
        then return $ NumericValue 0
        else return $ NumericValue (maximum numValues)

-- | Evaluate an INDIRECT function
evalIndirect :: [Expression] -> Build RuleMatch Key CellValue
evalIndirect [ref] = do
    -- Evaluate the reference expression to get the cell reference string
    refValue <- do
        val <- evalExpression ref
        case val of
            StringValue s -> return s  -- If it's already a string, use it
            NumericValue {} -> error "Numeric value not allowed in INDIRECT"

    -- Now that we have the reference string, evaluate the cell it points to
    liftIO $ putStrLn $ "  INDIRECT resolving reference: " ++ refValue
    Just result <- build keyToMatch (CellKey refValue)
    return result
evalIndirect _ = error "Invalid INDIRECT arguments"

-- | Extract a range like "A1:A3" into ["A1", "A2", "A3"]
expandRange :: CellRef -> CellRef -> [CellRef]
expandRange start end
    | colStart == colEnd =
        [colStart ++ show r | r <- [rowStart..rowEnd]]
    | rowStart == rowEnd =
        [col : show rowStart | col <- [head colStart..head colEnd]]
    | otherwise =
        []  -- For simplicity, we only handle single-dimension ranges
  where
    (colStart, rowStartStr) = span (not . isDigit) start
    (colEnd, rowEndStr) = span (not . isDigit) end
    rowStart = read rowStartStr :: Int
    rowEnd = read rowEndStr :: Int

-- | Count cells in a range
countCellsInRange :: [CellRef] -> Int
countCellsInRange = length

-- | Split a string by a delimiter
splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c (x:xs) | c == delimiter = []:x:xs
               | otherwise = (c:x):xs
    f _ [] = []

-- | Trim whitespace from a string
trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Format a numeric value to fit in a cell
formatValue :: CellValue -> String
formatValue (NumericValue n)
    | n == fromIntegral @Int (round n) =
        let str = show (round n :: Int)
        in padString 5 str
    | otherwise =
        let str = if n < 10 then show n else take 5 (show n)
        in padString 5 str
formatValue (StringValue t) = t

-- | Pad a string to a fixed width
padString :: Int -> String -> String
padString width str
    | length str > width = take width str
    | otherwise =
        let padLen = width - length str
            leftPad = padLen `div` 2
            rightPad = padLen - leftPad
        in replicate leftPad ' ' ++ str ++ replicate rightPad ' '

-- | Get a formatted display value for a cell
getCellDisplayValue :: CellRef -> Build RuleMatch Key String
getCellDisplayValue ref = do
    Just (Spreadsheet cells) <- build keyToMatch SpreadsheetKey

    case Map.lookup ref cells of
        Just (ValueCell (NumericValue val)) -> do
            -- Format numeric value to fit in cell
            return $ show val

        Just (ValueCell (StringValue str)) -> do
            -- Format string value to fit in cell
            return $ padString 8 str

        Just (FormulaCell _) -> do
            -- For formula cells, evaluate and show the result
            Just val <- build keyToMatch (CellKey ref)
            return $ formatValue val

        Nothing ->
            -- Empty cell
            return "        "

-- | Example to demonstrate the spreadsheet build system
exampleSpreadsheet :: Build RuleMatch Key ()
exampleSpreadsheet = do
    -- Register the rules
    rule MatchSpreadsheet initSpreadsheet
    rule MatchCell evaluateCell

    liftIO $ putStrLn "Spreadsheet Build System Example"
    liftIO $ putStrLn "================================"

    -- Print the spreadsheet

    -- Print info about each cell calculation
    liftIO $ putStrLn "\nCell Evaluations:"
    liftIO $ putStrLn "================="

    Just (Spreadsheet cells) <- build keyToMatch SpreadsheetKey
    forM_ (Map.toList cells) $ \(ref, content) -> do
        liftIO $ putStr $ ref ++ ": "
        case content of
            ValueCell expr -> do
                liftIO $ putStrLn $ "Value = " ++ show expr
            FormulaCell expr -> do
                liftIO $ putStrLn $ "Formula = " ++ showExpression expr
                Just result <- build keyToMatch (CellKey ref)
                liftIO $ putStrLn $ "Value = " ++ formatValue result

    -- Print build statistics
    printBuildStats

    return ()

main :: IO ()
main = do
    evalBuild exampleSpreadsheet
