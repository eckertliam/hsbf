module Parser (
  BFOp(..),
  Program,
  parseProgram
) where

data BFOp
  = IncrPtr -- >
  | DecrPtr -- <
  | IncrByte -- +
  | DecrByte -- -
  | OutputByte -- .
  | InputByte -- ,
  | LoopL -- [
  | LoopR -- ]
  | End -- $ program quit
  deriving (Show, Eq)

data ParseError = UnmatchedLeft Int | UnmatchedRight Int deriving (Show, Eq)

type Stack = [Int]

type ParseResult = Either ParseError Program

type Program = [BFOp]

unwrapParseResult :: ParseResult -> Program
unwrapParseResult (Right p) = p
unwrapParseResult (Left e) = error (show e)

parseProgram :: String -> Program
parseProgram input = unwrapParseResult (parse [] [] 0 (filter isBFChar input))
  where
    isBFChar c = c `elem` "><+-.,[]"

    parse :: Stack -> Program -> Int -> String -> ParseResult
    parse [] prog _ [] = Right (reverse prog)
    parse (s : ss) _ pos [] = Left (UnmatchedLeft s)
    parse stack prog pos (c : cs) = case c of
      '[' -> parse (pos : stack) (LoopL : prog) (pos + 1) cs
      ']' -> case stack of
        [] -> Left (UnmatchedRight pos)
        (_ : ss) -> parse ss (LoopR : prog) (pos + 1) cs
      '>' -> parse stack (IncrPtr : prog) (pos + 1) cs
      '<' -> parse stack (DecrPtr : prog) (pos + 1) cs
      '+' -> parse stack (IncrByte : prog) (pos + 1) cs
      '-' -> parse stack (DecrByte : prog) (pos + 1) cs
      '.' -> parse stack (OutputByte : prog) (pos + 1) cs
      ',' -> parse stack (InputByte : prog) (pos + 1) cs
      '$' -> parse stack (End : prog) (pos + 1) cs
      _ -> parse stack prog (pos + 1) cs
