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

--cria um stack vazia
createEmptyStack :: Stack
createEmptyStack = [] 


--printa a lista stack separada por vírgulas
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackEleStr stack)
  where
      stackEleStr FF = "False"
      stackEleStr TT = "True"
      stackEleStr (Int n) = show n

--criar um state vazio
createEmptyState :: State
createEmptyState = [] 

--printa a lista state por ordem alfabética dos nomes das variáveis
state2Str :: State -> String
state2Str state = intercalate "," (map stateEleStr sortedState)
  where
    stateEleStr (a, Int n) = a ++ "=" ++ show n
    stateEleStr (a, FF) = a ++ "=False"
    stateEleStr (a, TT) = a ++ "=True"  
    sortedState = sortBy (comparing fst) state



--findNinState :: Show b => State -> String -> Maybe StackTypes
findNinState state n = lookup n state


updateNinState :: State -> String -> StackTypes -> State
updateNinState state n newVal = case lookupIndex state n of
  Just index -> updateAtIndex state index (\(key, _) -> (key, newVal))
  Nothing    -> state ++ [(n, newVal)]


lookupIndex :: State -> String -> Maybe Int
lookupIndex state n = elemIndex n (map fst state)

updateAtIndex :: [a] -> Int -> (a -> a) -> [a]
updateAtIndex lst index f = take index lst ++ [f (lst !! index)] ++ drop (index + 1) lst

--função run que opera com o code, stack e state dados
run :: (Code, Stack, State) -> (Code, Stack, State)

run ([], stack, state) = ([],stack, state) 
run ((Push n):code, stack, state) = run(code, (Int n):stack, state)
run ((Fetch n):code, stack, state) = case findNinState state n of
    Just value -> run(code, value:stack, state)
    Nothing -> error "Run-time error"
run ((Store n):code, a:stack, state) = run(code, stack, (updateNinState state n a))
run ((Add):code, (Int x):(Int y):stack, state) = run(code, (Int (x+y)):stack, state)
run ((Sub):code, (Int x):(Int y):stack, state) = run(code, (Int (x-y)):stack, state)
run ((Mult):code, (Int x):(Int y):stack, state) = run(code, (Int (x*y)):stack, state)
run ((Fals):code, stack, state) = run (code, (FF):stack, state)
run ((Tru):code, stack, state) = run (code, (TT):stack, state)
run ((Equ):code, x:y:stack, state) 
  | x == y = run (code, (TT):stack, state)
  | x /= y = run (code, (FF):stack, state)
  | otherwise = error "Run-time error"
run ((Le):code, x:y:stack, state)
  | x <= y = run (code, (TT):stack, state)
  | x > y = run (code, (FF):stack, state)
  | otherwise = error "Run-time error"
run ((Neg):code, x:stack, state)
    | x == FF = run (code, (TT):stack, state)
    | x == TT = run (code, (FF):stack, state)
    | otherwise = error "Run-time error"
run ((And):code, x:y:stack, state) 
  | x == TT && y == TT = run (code, (TT):stack, state)
  | x == FF || y == FF = run (code, (FF):stack, state)
  | otherwise = error "Run-time error"
run ((Noop):code, stack, state) = (code,stack, state)
run ((Branch c1 c2):code, TT:stack, state) = run(c1 ++ code, stack, state)
run ((Branch c1 c2):code, FF:stack, state) = run(c2 ++ code, stack, state)
run ((Loop c1 c2):code, stack , state) = run(c1++[Branch newCode [Noop]] ++ code, stack, state)
  where
    newCode = c2++[Loop c1 c2]                    

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

--recebe uma expressão atrimétrica e retorna uma lista de intruções
compA :: Aexp -> Code
compA (Val n) = [Push n]
compA (Var a) = [Fetch a]
compA (AddAexp e1 e2) = compA e2 ++ compA e1 ++ [Add]
compA (SubAexp e1 e2) = compA e2 ++ compA e1 ++ [Sub]
compA (MultAexp e1 e2) = compA e2 ++ compA e1 ++ [Mult]

--recebe uma expressão booleana e retorna uma lista de intruções
compB :: Bexp -> Code
compB (EquAexp e1 e2) = compA e2 ++ compA e1 ++ [Equ]
compB (LeAexp e1 e2) = compA e2 ++ compA e1 ++ [Le]
compB (AndBexp e1 e2) = compB e2 ++ compB e1 ++ [And]
compB (EquBexp e1 e2) = compB e2 ++ compB e1 ++ [Equ]
compB (NegBexp e) = compB e ++ [Neg]
compB (TruB) = [Tru]
compB (FalsB) = [Fals]

--recebe uma lista de stm e retorna uma lista de instruções
compile :: Program -> Code
compile stms = concatMap compStm stms
  where
    compStm (BranchS be stm1 stm2) = compB be ++ [Branch (compile stm1) (compile stm2)]
    compStm (LoopS be stm) = [Loop (compB be) (compile stm)]
    compStm (AssignVar var ae) = compA ae ++ [Store var]

data Token =
    IntTok Integer -- number
    | VarTok String -- variable name
    | AssignTok -- :=
    | AddTok -- +
    | SubTok -- -
    | MultTok -- *
    | OpenTok -- (
    | CloseTok -- )
    | BreakTok -- ;
    | EqualBTok -- =
    | EqualATok -- ==
    | LETok -- <=
    | AndTok -- and
    | TrueTok -- True
    | FalseTok -- False
    | IfTok -- if
    | ThenTok -- then
    | ElseTok -- else
    | WhileTok -- while
    | DoTok -- do
    | NotTok -- not
    | NoOpTok -- noop
    deriving (Show, Eq)

data StringToken =
  Str String | Tok Token 
  deriving Show

--recebe uma lista de stringtokens e retorna uma lista de tokens
stringToken2Token :: [StringToken] -> [Token]
stringToken2Token [] = []
stringToken2Token (x:xs) = case x of
  Str _  -> stringToken2Token xs
  Tok t  -> t : stringToken2Token xs

--recebe uma string e retorna uma lista de statements
parse :: String -> Program
parse str = parse_aux (stringToken2Token (mergeVarToks (mergeIntToks (parse_tokens (parse_tokens_aux str []))))) []

--usa parse_tokens_aux para obter uma lista de StringTokens e converte os strings que sobram em IntToks e VarToks
parse_tokens :: [StringToken] -> [StringToken]
parse_tokens [] = []
parse_tokens (Str s : rest) =
  case reads s :: [(Integer, String)] of
    [(n, "")] -> Tok (IntTok (fromIntegral n)) : parse_tokens rest
    _         -> Tok (VarTok s) : parse_tokens rest
parse_tokens (Tok t : rest) = Tok t : parse_tokens rest

--recebe uma lista de string tokens e retorna uma lista de string tokens
--junta os IntToks adjacentes para formar números com mais de um dígito
mergeIntToks :: [StringToken] -> [StringToken]
mergeIntToks [] = []
mergeIntToks (Tok (IntTok a):Tok (IntTok b):rest) = mergeIntToks ([Tok (IntTok (a*10 + b))] ++ rest)
mergeIntToks (a:rest) = [a] ++ mergeIntToks rest

--recebe uma lista de string tokens e retorna uma lista de string tokens
--junta os VarToks adjacentes para formar nomes de variáveis com mais de uma letra
mergeVarToks :: [StringToken] -> [StringToken]
mergeVarToks [] = []
mergeVarToks (Tok (VarTok a):Tok (VarTok b):rest) = mergeVarToks ([Tok (VarTok (a ++ b))] ++ rest)
mergeVarToks (a:rest) = [a] ++ mergeVarToks rest

--recebe uma string e uma lista (acumulador) de string tokens
--e retorna uma lista de string tokens
--lê o string de input e transforma-o em tokens (à exceção dos nomes das variáveis e números)
parse_tokens_aux :: String -> [StringToken] -> [StringToken]
parse_tokens_aux "" tokens = tokens
parse_tokens_aux (' ':rest) tokens = parse_tokens_aux rest tokens
parse_tokens_aux ('\n':rest) tokens = parse_tokens_aux rest tokens
parse_tokens_aux ('\t':rest) tokens = parse_tokens_aux rest tokens
parse_tokens_aux (':':'=':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok AssignTok])
parse_tokens_aux ('+':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok AddTok])
parse_tokens_aux ('-':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok SubTok])
parse_tokens_aux ('*':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok MultTok])
parse_tokens_aux ('(':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok OpenTok])
parse_tokens_aux (')':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok CloseTok])
parse_tokens_aux (';':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok BreakTok])
parse_tokens_aux ('=':'=':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok EqualATok])
parse_tokens_aux ('<':'=':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok LETok])
parse_tokens_aux ('=':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok EqualBTok])
parse_tokens_aux ('a':'n':'d':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok AndTok])
parse_tokens_aux ('T':'r':'u':'e':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok TrueTok])
parse_tokens_aux ('F':'a':'l':'s':'e':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok FalseTok])
parse_tokens_aux ('i':'f':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok IfTok])
parse_tokens_aux ('t':'h':'e':'n':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok ThenTok])
parse_tokens_aux ('e':'l':'s':'e':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok ElseTok])
parse_tokens_aux ('w':'h':'i':'l':'e':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok WhileTok])
parse_tokens_aux ('d':'o':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok DoTok])
parse_tokens_aux ('n':'o':'t':rest) tokens = parse_tokens_aux rest (tokens ++ [Tok NotTok])
parse_tokens_aux (c:rest) tokens = parse_tokens_aux rest (tokens ++ [Str [c]])

-- recebe uma lista de tokens e um token
-- retorna uma lista de tokens até à primeira ocorrência do token dado
parse_until :: [Token] -> Token -> [Token] 
parse_until [] _ = [] 
parse_until (x:xs) obj
  | x == obj = []
  | otherwise = x:parse_until xs obj

-- recebe uma lista de tokens e um token
-- retorna uma lista de tokens depois da primeira ocorrência do token dado
parse_after :: [Token] -> Token -> [Token] 
parse_after [] _ = [] 
parse_after (x:xs) obj
  | x == obj = xs
  | otherwise = parse_after xs obj


-- recebe uma lista de tokens e um inteiro
-- retorna uma lista de tokens quando parenteses derem match
parse_brackets :: [Token] -> Int -> [Token]
parse_brackets [] _ = []
parse_brackets (tok:rest) n 
  | tok == OpenTok = [OpenTok] ++ (parse_brackets rest (n+1))
  | tok == CloseTok && n == 0 = []
  | tok == CloseTok = [CloseTok] ++ (parse_brackets rest (n-1))
  | otherwise = [tok] ++ (parse_brackets rest n)


-- recebe uma lista de tokens e um inteiro
-- retorna uma lista de tokens depois de parenteses darem match
parse_after_brackets :: [Token] -> Int -> [Token]
parse_after_brackets [] _ = []
parse_after_brackets (tok:rest) n 
  | tok == OpenTok = parse_after_brackets rest (n+1)
  | tok == CloseTok && n == 0 = rest
  | tok == CloseTok = parse_after_brackets rest (n-1)
  | otherwise = parse_after_brackets rest n

-- recebe uma lista de tokens e um token
-- retorna uma lista de tokens até à primeira ocorrência do token dado fora de parênteses
parse_until_ob :: [Token] -> Token -> Int -> [Token]
parse_until_ob [] _ _ = []
parse_until_ob (tok:rest) t n
  | tok == OpenTok = [OpenTok] ++ (parse_until_ob rest t (n+1))
  | tok == CloseTok = [CloseTok] ++ (parse_until_ob rest t (n-1))
  | tok == t && n == 0 = []
  | otherwise = [tok] ++ (parse_until_ob rest t n)
  
-- recebe uma lista de tokens e um token
-- retorna uma lista de tokens depois da primeira ocorrência do token dado fora de parênteses
parse_after_ob :: [Token] -> Token -> Int -> [Token]
parse_after_ob [] _ _ = []
parse_after_ob (tok:rest) t n
  | tok == OpenTok = parse_after_ob rest t (n+1)
  | tok == CloseTok = parse_after_ob rest t (n-1)
  | tok == t && n == 0 = rest
  | otherwise = parse_after_ob rest t n

-- recebe uma lista de tokens e retorna uma expressão aritmétrica
parse_aexp :: [Token] -> Aexp
parse_aexp [IntTok x] = (Val x)
parse_aexp [VarTok x] = (Var x)
parse_aexp (OpenTok:rest) 
  | (parse_after_brackets rest 0) == [] = parse_aexp (parse_brackets rest 0)
  | (SubTok:last) <- (parse_after_brackets rest 0) = (SubAexp (parse_aexp (parse_brackets rest 0)) (parse_aexp last))
  | (AddTok:last) <- (parse_after_brackets rest 0) = (AddAexp (parse_aexp (parse_brackets rest 0)) (parse_aexp last))
  | (MultTok:last) <- (parse_after_brackets rest 0) = (MultAexp (parse_aexp (parse_brackets rest 0)) (parse_aexp last))
  | otherwise = error "No operator or end after ()"

parse_aexp toks
  | tokExistsOutsideBrackets toks AddTok 0 = (AddAexp (parse_aexp (parse_until_ob toks AddTok 0)) (parse_aexp (parse_after_ob toks AddTok 0)))
  | tokExistsOutsideBrackets toks SubTok 0 = (SubAexp (parse_aexp (reverse (parse_after_ob (reverse toks) SubTok 0))) (parse_aexp (reverse (parse_until_ob (reverse toks) SubTok 0))))
  | tokExistsOutsideBrackets toks MultTok 0 = (MultAexp (parse_aexp (parse_until_ob toks MultTok 0)) (parse_aexp (parse_after_ob toks MultTok 0)))
  | otherwise = error "Not a valid expression"

-- recebe uma lista de tokens e retorna uma expressão booleana
parse_bexp :: [Token] -> Bexp
parse_bexp toks
  | (OpenTok:rest) <- toks, (parse_after_brackets rest 0) == [] = parse_bexp (parse_brackets rest 0)
  | (OpenTok:rest) <- toks, (AndTok:last) <- (parse_after_brackets rest 0) = (AndBexp (parse_bexp (parse_brackets rest 0)) (parse_bexp last))
  | (OpenTok:rest) <- toks, (EqualBTok:last) <- (parse_after_brackets rest 0) = (EquBexp (parse_bexp (parse_brackets rest 0)) (parse_bexp last))
  | toks == [TrueTok] = TruB
  | toks == [FalseTok] = FalsB
  | tokExistsOutsideBrackets toks AndTok 0 = AndBexp (parse_bexp (parse_until_ob toks AndTok 0)) (parse_bexp (parse_after_ob toks AndTok 0))
  | tokExistsOutsideBrackets toks EqualBTok 0 = EquBexp (parse_bexp (parse_until_ob toks EqualBTok 0)) (parse_bexp (parse_after_ob toks EqualBTok 0))
  | tokExistsOutsideBrackets toks NotTok 0 = NegBexp (parse_bexp (parse_after_ob toks NotTok 0))
  | tokExistsOutsideBrackets toks EqualATok 0 = EquAexp (parse_aexp (parse_until_ob toks EqualATok 0)) (parse_aexp (parse_after_ob toks EqualATok 0))
  | tokExistsOutsideBrackets toks LETok 0 = LeAexp (parse_aexp (parse_until_ob toks LETok 0)) (parse_aexp (parse_after_ob toks LETok 0))
  | otherwise = error "No valid boolexp token found"

--recebe uma lista de tokens, um token e um inteiro
--retorna um booleano verdadeiro se o token existir fora de parenteses
tokExistsOutsideBrackets :: [Token] -> Token -> Int -> Bool
tokExistsOutsideBrackets [] _ _ = False
tokExistsOutsideBrackets (OpenTok:rest) tok n = tokExistsOutsideBrackets rest tok (n+1)
tokExistsOutsideBrackets (CloseTok:rest) tok n = tokExistsOutsideBrackets rest tok (n-1)
tokExistsOutsideBrackets (t:rest) tok 0
  | t == tok = True
  | otherwise = tokExistsOutsideBrackets rest tok 0
tokExistsOutsideBrackets (_:rest) tok n = tokExistsOutsideBrackets rest tok n

--recebe uma lista de tokens e uma lista de stm (acumulador) retorna uma lista de statements
--função recursiva auxiliar do parse
parse_aux :: [Token] -> Program -> Program
parse_aux [] program = program 
parse_aux (VarTok name:AssignTok:rest) program = parse_aux cont (program++[AssignVar name (parse_aexp (untilBreak))])
  where 
    untilBreak = parse_until rest BreakTok
    cont = parse_after rest BreakTok
parse_aux (IfTok:rest) program = parse_aux cont (program++[BranchS (parse_bexp beforeThen) (parse_aux thenSmt []) (parse_aux elseSmt [])])
  where 
    beforeThen = parse_until rest ThenTok     
    afterThen = parse_after rest ThenTok
    thenSmt = case afterThen of 
      OpenTok:last -> parse_brackets last 0
      _ -> parse_until afterThen BreakTok
    afterElse = parse_after afterThen ElseTok
    (elseSmt, cont) = case afterElse of
      OpenTok:last -> (parse_brackets last 0, parse_after (parse_after_brackets last 0) BreakTok)
      _ -> (parse_until afterElse BreakTok, parse_after afterElse BreakTok)
parse_aux (WhileTok:rest) program = parse_aux cont (program++[LoopS (parse_bexp beforeDo) (parse_aux doStm [])])
  where 
    beforeDo = parse_until rest DoTok
    afterDo = parse_after rest DoTok
    (_:afterOpen) = afterDo
    doStm = parse_brackets afterOpen 0 
    cont = parse_after (parse_after_brackets afterOpen 0) BreakTok
parse_aux (BreakTok:rest) program = parse_aux rest program

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

-- main :: IO ()
-- main = do
--  let result1 = testParser "x := 5; x := x - 1;"
--  let result2 = testParser "x := 0 - 2;"
--  let result3 = testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"
--  let result4 = testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);"
--  let result5 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"
--  let result6 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"
--  let result7 = testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;"
--  let result8 = testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;"
--  let result9 = testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;"
--  let result10 = testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"
--  let result11 = testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"
--  let result12 = testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"

--  print result1
--  print result2
--  print result3
--  print result4
--  print result5
--  print result6
--  print result7
--  print result8
--  print result9
--  print result10
--  print result11
--  print result12