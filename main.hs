-- PFL 2023/24 - Haskell practical assignment quickstart

import Data.List

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code 
  deriving Show
type Code = [Inst]

data StackTypes =
  Int Integer | FF | TT 
  deriving (Show, Eq, Ord)
type Stack = [StackTypes]

createEmptyStack :: Stack
createEmptyStack = [] 
--cria um stack vazia

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackEleStr stack)
  where
      stackEleStr FF = "False"
      stackEleStr TT = "True"
      stackEleStr (Int n) = show n


-- createEmptyState :: State
createEmptyState = undefined -- TODO, Uncomment the function signature after defining State

-- state2Str :: State -> String
state2Str = undefined -- TODO

--run :: (Code, Stack, State) -> (Code, Stack, State)

run ([], stack, state) = ([],stack, state) 
-- caso de não haver mais inst

run ((Push n):code, stack, state) = run(code, (Int n):stack, state)
-- dar push a n

run ((Add):code, (Int x):(Int y):stack, state) = run(code, (Int (x+y)):stack, state)
-- adiciona x a y e adiciona o resultado á stack

run ((Sub):code, (Int x):(Int y):stack, state) = run(code, (Int (x-y)):stack, state)
-- subtrair x a y e adiciona o resultado á stack

run ((Mult):code, (Int x):(Int y):stack, state) = run(code, (Int (x*y)):stack, state)
--multiplica x por y e adiciona o resultado á stack

run ((Fals):code, stack, state) = run (code, (FF):stack, state)
--adiciona FF á stack

run ((Tru):code, stack, state) = run (code, (TT):stack, state)
--adiciona FF á stack

run ((Equ):code, x:y:stack, state) 
  | x == y = run (code, (TT):stack, state)
  | x /= y = run (code, (FF):stack, state)
--compara os dois topmost elements da stack e 
--mete TT se verdade na stack e FF otherwise

run ((Le):code, x:y:stack, state)
  | x <= y = run (code, (TT):stack, state)
  | x > y = run (code, (FF):stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples: 
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")