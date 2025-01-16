module CodeGen where

import Data.List (intercalate)
import Ir

generateC :: Instructions -> String
generateC instructions =
  unlines
    [ "#include <stdio.h>",
      "#include <stdlib.h>",
      "",
      "int main(void) {",
      "    char tape[30000] = {0};",
      "    char *ptr = tape;",
      "",
      indentCode 4 (genInstructions instructions),
      "    return 0;",
      "}"
    ]

genInstructions :: Instructions -> String
genInstructions = intercalate "\n" . map genInstruction

genInstruction :: BfInst -> String
genInstruction inst = case inst of
  IncrPtr -> "ptr++;"
  DecrPtr -> "ptr--;"
  IncrByte -> "++*ptr;"
  DecrByte -> "--*ptr;"
  OutputByte -> "putchar(*ptr);"
  InputByte -> "*ptr = getchar();"
  Loop body ->
    unlines
      [ "while (*ptr) {",
        indentCode 4 (genInstructions body),
        "}"
      ]
  End -> ""

indentCode :: Int -> String -> String
indentCode n = unlines . map (replicate n ' ' ++) . lines
