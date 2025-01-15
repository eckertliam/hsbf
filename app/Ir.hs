module Ir where

import Parser

data BfInst
  = IncrPtr
  | DecrPtr
  | IncrByte
  | DecrByte
  | OutputByte
  | InputByte
  | Loop Instructions
  | End
  deriving (Show, Eq)

type Instructions = [BfInst]

progToIr :: Program -> Either String Instructions
progToIr prog = case toIr [] prog of
    (inst, []) -> Right (reverse inst)
    _ -> Left "Unmatched brackets in program"
  where
    toIr :: Instructions -> Program -> (Instructions, Program)
    toIr acc [] = (acc, [])
    toIr acc (c:cs) = case c of
        Parser.LoopL -> 
            let (loopBody, rest) = toIr [] cs
            in case rest of
                (Parser.LoopR:remainder) -> toIr (Loop loopBody : acc) remainder
                _ -> (acc, c:cs)  -- Error case, unmatched bracket
        Parser.LoopR -> (acc, c:cs)  -- Return to parent loop context
        _ -> toIr (convert c : acc) cs

    convert :: Parser.BFOp -> BfInst
    convert Parser.IncrByte = Ir.IncrByte
    convert Parser.DecrByte = Ir.DecrByte
    convert Parser.IncrPtr = Ir.IncrPtr
    convert Parser.DecrPtr = Ir.DecrPtr
    convert Parser.OutputByte = Ir.OutputByte
    convert Parser.InputByte = Ir.InputByte
    convert Parser.End = Ir.End
    convert _ = error "Invalid BFOp"