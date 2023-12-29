-- PFL 2023/24 - Haskell practical assignment quickstart

import Data.List
import Data.Ord

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
type State = [(String, StackTypes)]

createEmptyStack :: Stack
createEmptyStack = [] 
--cria um stack vazia

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackEleStr stack)
  where
      stackEleStr FF = "False"
      stackEleStr TT = "True"
      stackEleStr (Int n) = show n


createEmptyState :: State
createEmptyState = [] 

state2Str :: State -> String
state2Str state = intercalate "," (map stateEleStr sortedState)
  where
    stackEleStr (a, Int n) = a ++ "=" ++ show n
    stateEleStr (a, FF) = a ++ "=False"
    stateEleStr (a, TT) = a ++ "=True"  
    sortedState = sortBy (comparing fst) state
      --tem de ser passado para ordem alphabetica

--findNinState :: Show b => State -> String -> Maybe StackTypes
findNinState state n = lookup n state

--updateNinState :: State -> String -> StackTypes -> State
updateNinState state n newVal = case lookupIndex state n of
  Just index -> updateAtIndex state index (\(key, _) -> (key, newVal))
  Nothing    -> state ++ [(n, newVal)]

--lookupIndex :: State -> String -> Maybe Int
lookupIndex state n = elemIndex n (map fst state)

--updateAtIndex :: [a] -> Int -> (a -> a) -> [a]
updateAtIndex lst index f = take index lst ++ [f (lst !! index)] ++ drop (index + 1) lst

--run :: (Code, Stack, State) -> (Code, Stack, State)

run ([], stack, state) = ([],stack, state) 
-- caso de não haver mais inst

run ((Push n):code, stack, state) = run(code, (Int n):stack, state)
-- dar push a n

run ((Fetch n):code, stack, state) = case findNinState state n of
    Just value -> run(code, value:stack, state)
    noting -> run(code, stack, state)

run ((Store n):code, a:stack, state) = run(code, stack, (updateNinState state n a))

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

run ((Neg):code, x:stack, state)
    | x == FF = run (code, (TT):stack, state)
    | x == TT = run (code, (FF):stack, state)
--nega o valor no topo da stack e coloca-o a stack

run ((And):code, x:y:stack, state) 
  | x == TT && y == FF = run (code, (TT):stack, state)
  | x == FF || y == FF = run (code, (FF):stack, state)
  | otherwise = error "Run-time error"
--compara os dois topmost elements da stack (booleanos) e realiza a um e lógico

run ((Noop):code, stack, state) = (code,stack, state)
--não percebo a utilidade disto, mas tava no enunciado..

run ((Branch c1 c2):code, TT:stack, state) = run(c1, stack, state)
run ((Branch c1 c2):code, FF:stack, state) = run(c2, stack, state)
--se o primeiro valor da stack for TT faz c1, se for FF faz c2

run ((Loop c1 c2):code, stack , state) = run(c1++[Branch newCode [Noop]], stack, state)
  where
    newCode = c2++[Loop c1 c2]                    

--se V entao vai para c2, otherwise é recursiva

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
-- testAssembler [Push 1, Push 2, And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"


-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program
data Aexp =
  Val Integer | Var String | AddAexp Aexp Aexp | SubAexp Aexp Aexp | MultAexp Aexp Aexp
  deriving Show

data Bexp =
  EquAexp Aexp Aexp | LeAexp Aexp Aexp | AndBexp Bexp Bexp | EquBexp Bexp Bexp | NegBexp Bexp | TruB | FalsB
  deriving Show

data Stm =
  BranchS Bexp [Stm] [Stm] | LoopS Bexp [Stm] | AssignVar String Aexp
  deriving Show

type Program = [Stm]


-- compA :: Aexp -> Code
compA (Val n) = [Push n]
compA (Var a) = [Fetch a]
compA (AddAexp e1 e2) = compA e2 ++ compA e1 ++ [Add]
compA (SubAexp e1 e2) = compA e2 ++ compA e1 ++ [Sub]
compA (MultAexp e1 e2) = compA e2 ++ compA e1 ++ [Mult]

-- compB :: Bexp -> Code
compB (EquAexp e1 e2) = compA e2 ++ compA e1 ++ [Equ]
compB (LeAexp e1 e2) = compA e2 ++ compA e1 ++ [Le]
compB (AndBexp e1 e2) = compB e2 ++ compB e1 ++ [And]
compB (EquBexp e1 e2) = compB e2 ++ compB e1 ++ [Equ]
compB (NegBexp e) = compB e ++ [Neg]
compB (TruB) = [Tru]
compB (FalsB) = [Fals]

-- compile :: Program -> Code
compile stms = concatMap compStm stms
  where
    compStm (BranchS be stm1 stm2) = compB be ++ [Branch (compile stm1) (compile stm2)]
    compStm (LoopS be stm) = [Loop (compB be) (compile stm)]
    compStm (AssignVar var ae) = compA ae ++ [Store var]

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
--  push 43, store x, push 43, fecth x, le, branch (then, else)
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

--main :: IO ()
--main = do
--    putStrLn "Hello, World!"
