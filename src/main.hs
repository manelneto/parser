import Data.Map (Map, fromList, toList, (!), member, insert, empty, lookup)
import Data.List (sort, intercalate)


data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
  
type Code = [Inst]

-- Stack is a list of either Integers or Bools
type Stack = [Either Integer Bool]

-- State is a map from a string to either an Integer or a Bool
type State = Map String (Either Integer Bool)


-- Returns an empty Stack
createEmptyStack :: Stack 
createEmptyStack = []

-- Returns an empty State 
createEmptyState :: State 
createEmptyState = empty

-- Returns the string representation of a stack in the form "x,y,z", where x is the top of the stack
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (h:t)
  | null t = case h of
      Left x -> show x ++ stack2Str t
      Right x -> show x ++ stack2Str t
  | otherwise = case h of
      Left x -> show x ++ "," ++ stack2Str t
      Right x -> show x ++ "," ++ stack2Str t 
  
-- Returns the string representation of a state in the form "x=1,y=False,z=2", 
-- where x, y and z are the variables in the state, ordered alphabetically
state2Str :: State -> String
state2Str state = case toList state of
  [] -> ""
  lst -> intercalate "," $ map (\(x, y) -> x ++ "=" ++ showValue y) lst  where
    showValue :: Either Integer Bool -> String
    showValue (Left intVal) = show intVal
    showValue (Right boolVal) = show boolVal

-- Runs a list of instructions, returning as output an empty code list,
-- a stack and the output values in the storage
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (h:t, stack, state) = case h of
  Add -> run (t, add stack, state)
  Mult -> run (t, mult stack, state)
  Sub -> run (t, sub stack, state)
  Equ -> run (t, eq stack, state)
  Le -> run (t, le stack, state)
  Push n -> run (t, push n stack, state)
  Tru -> run (t, true stack, state)
  Fals -> run (t, false stack, state)
  Fetch x -> run (t, fetch x stack state, state)
  Store x -> run (t, fst stackState, snd stackState) where  stackState = store x stack state
  Branch c1 c2 -> run (c ++ t, s, state) where (c, s) = branch c1 c2 stack  
  Noop -> run (t, stack, state)
  Loop c1 c2 -> run(c ++ t, stack, state) where c = loop c1 c2
  Neg -> run (t, neg stack, state)
  And -> run (t, myAnd stack, state)


-- Instructions


-- Adds the top two integer values of the stack, respectively, 
-- and pushes the result onto the top of the stack
add :: Stack -> Stack
add [] = error "Run-time error"
add [x] = error "Run-time error"
add (x:y:t) = case (x, y) of
  (Left x, Left y) -> Left (x + y):t
  _ -> error "Run-time error"


-- Multiplies the top two integer values of the stack, respectively,
-- and pushes the result onto the top of the stack
mult :: Stack -> Stack
mult [] = error "Run-time error"
mult [x] = error "Run-time error"
mult (x:y:t) = case (x, y) of
  (Left x, Left y) -> Left (x * y):t
  _ -> error "Run-time error"


-- Subtracts the topmost element of the stack with the second topmost element,
-- and pushes the result onto the top of the stack
sub :: Stack -> Stack
sub [] = error "Run-time error"
sub [x] = error "Run-time error"
sub (x:y:t) = case (x, y) of
  (Left x, Left y) -> Left (x - y):t
  _ -> error "Run-time error"


-- Compares the top two values of the stack for equality,
-- and pushes a boolean with the comparison result onto the top of the stack
eq :: Stack -> Stack
eq [] = error "Run-time error"
eq [x] = error "Run-time error"
eq (x:y:t) = case (x, y) of
  (Left x, Left y) -> Right (x == y):t
  (Right x, Right y) -> Right (x == y):t
  _ -> error "Run-time error"


-- Determines whether the topmost stack element is less or equal to the second topmost element,
-- and pushes a boolean with the comparison result onto the top of the stack
le :: Stack -> Stack
le [] = error "Run-time error"
le [x] = error "Run-time error"
le (x:y:t) = case (x, y) of
  (Left x, Left y) -> Right (x <= y):t
  _ -> error "Run-time error"


-- Pushes an integer onto the top of the stack
push :: Integer -> Stack -> Stack
push x s = Left x:s


-- Pushes the boolean value True onto the top of the stack
true :: Stack -> Stack
true s = Right True:s


-- Pushes the boolean value False onto the top of the stack
false :: Stack -> Stack
false s = Right False:s


-- Pushes the value bound to x onto the top of the stack.
-- If x is not bound, raises a run-time error
fetch :: String -> Stack -> State -> Stack
fetch x s state = case Data.Map.lookup x state of
  Just val -> case val of
    Left intVal -> Left intVal : s
    Right boolVal -> Right boolVal : s
  Nothing -> error "Run-time error"


-- Pops the topmost element of the stack,
-- and updates the storage so that the popped value is bound to x.
store :: String -> Stack -> State -> (Stack, State)
store x [] state = error "Run-time error"
store x (h:t) state = 
    (t, insert x h state)


-- If the top of the stack is the value True, 
-- the stack is popped and c1 is to be executed next.
-- Otherwise, if the top element of the stack is the value False,
-- then it will be popped and c2 will be executed next.
branch :: Code -> Code -> Stack -> (Code, Stack)
branch c1 c2 [] = error "Run-time error"
branch c1 c2 (h:t) = case h of
  Right True -> (c1, t)
  Right False -> (c2, t)
  _ -> error "Run-time error"


-- Writes c1 to the beggining of the code list,
-- followed by a branch instruction that executes c2 followed by the loop instruction itself.
-- or a Noop 
loop :: Code -> Code -> Code
loop c1 c2 = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]


-- Negates the topmost boolean value of the stack,
-- and pushes the result onto the top of the stack
neg :: Stack -> Stack
neg [] = error "Run-time error"
neg (h:t) = case h of
  Right x -> Right (not x):t
  _ -> error "Run-time error"


-- Pops the two top values of the stack,
-- and pushes the result of the logical operation AND between them if they are both booleans
myAnd :: Stack -> Stack
myAnd [] = error "Run-time error"
myAnd [x] = error "Run-time error"
myAnd (x:y:t) = case (x, y) of
  (Right x, Right y) -> Right (x && y):t
  _ -> error "Run-time error"


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

--
-- Examples:
-- Teste 1 certo testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- Teste 2 certo testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- Teste 3 certo testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- Teste 4 certo testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- Teste 5 certo testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- Teste 6 certo testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- Teste 7 certo testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- Teste 8 certo testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- Teste 9 certo testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"


test1 :: Bool
test1 = testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")

test2 :: Bool
test2 = testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")

test3 :: Bool
test3 = testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")

test4 :: Bool
test4 = testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")

test5 :: Bool
test5 = testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")

test6 :: Bool
test6 = testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")

test7 :: Bool
test7 = testAssembler [Push (-20),Push (-21), Le] == ("True","")

test8 :: Bool
test8 = testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")

test9 :: Bool
test9 = testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

tests :: Bool
tests = test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8 && test9


-- Part 2


-- Aexp is an arithmetic expression
data Aexp = Num Integer | NumVar String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp

-- Bexp is a boolean expression
data Bexp = Bool Bool | BoolVar String | EqA Aexp Aexp | LeA Aexp Aexp | EqB Bexp Bexp | AndB Bexp Bexp | NegB Bexp

-- Stm is a statement
data Stm = AssignA String Aexp | AssignB String Bexp | If Bexp Stm Stm | While Bexp Stm | Seq Stm Stm

-- Program is a list of statements
type Program = [Stm]


-- Compiles an arithmetic expression
compA :: Aexp -> Code
compA expA = case expA of
  Num num -> [Push num]
  NumVar var -> [Fetch var]
  AddA left right -> compA right ++ compA left ++ [Add]
  SubA left right -> compA right ++ compA left ++ [Sub]
  MultA left right -> compA right ++ compA left ++ [Mult]


-- Compiles a boolean expression
compB :: Bexp -> Code
compB expB = case expB of
  Bool bool -> if bool then [Tru] else [Fals]
  BoolVar var -> [Fetch var]
  EqA left right -> compA right ++ compA left ++ [Equ]
  LeA left right -> compA right ++ compA left ++ [Le]
  EqB left right -> compB right ++ compB left ++ [Equ]
  AndB left right -> compB right ++ compB left ++ [And]
  NegB exp -> compB exp ++ [Neg]


-- Compiles a program
compile :: Program -> Code
compile [] = []
compile (h:t) = case h of
  AssignA var expA -> compA expA ++ [Store var] ++ compile t
  AssignB var expB -> compB expB ++ [Store var] ++ compile t
  Seq s1 s2 -> compile [s1] ++ compile [s2] ++ compile t
  If expB s1 s2 -> compB expB ++ [Branch (compile [s1]) (compile [s2])] ++ compile t
  While expB s -> Loop (compB expB) (compile [s]) : compile t


--parse :: String -> Program
parse = undefined

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

{-
test1 :: Bool
test1 = testParser "x := 5; x := x - 1;" == ("","x=4")

test2 :: Bool
test2 = testParser "x := 0 - 2;" == ("","x=-2")

test3 :: Bool
test3 = testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")

test4 :: Bool
test4 = testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")

test5 :: Bool
test5 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")

test6 :: Bool
test6 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")

test7 :: Bool
test7 = testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")

test8 :: Bool
test8 = testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")

test9 :: Bool
test9 = testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")

test10 :: Bool
test10 = testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")

test11 :: Bool
test11 = testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")

test12 :: Bool
test12 = testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

tests :: Bool
tests = test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8 && test9 && test10 && test11 && test12
-}

-- Exemplo de programa simples: x := 10;
program1 :: Program
program1 = [AssignA "x" (Num 10)]
correct1 :: Code
correct1 = [Push 10, Store "x"]

-- Exemplo de sequência de comandos: x := 5; y := x + 3;
program2 :: Program
program2 = [AssignA "x" (Num 5), AssignA "y" (AddA (NumVar "x") (Num 3))]
correct2 :: Code
correct2 = [Push 5, Store "x", Push 3, Fetch "x", Add, Store "y"]

-- Exemplo de condicional: if x = 0 then y := 1 else y := 2;
program3 :: Program
program3 = [If (EqA (NumVar "x") (Num 0)) (AssignA "y" (Num 1)) (AssignA "y" (Num 2))]
correct3 :: Code
correct3 = [Push 0, Fetch "x", Equ, Branch [Push 1, Store "y"] [Push 2, Store "y"]]

-- Exemplo de while loop: while x > 0 do x := x - 1;
program4 :: Program
program4 = [While (NegB (LeA (NumVar "x") (Num 0))) (AssignA "x" (SubA (NumVar "x") (Num 1)))]
correct4 :: Code
correct4 = [Loop [Push 0, Fetch "x", Le, Neg] [Push 1, Fetch "x", Sub, Store "x"]]

-- ? Nao percebi qual era o codigo correspondente, por isso nao consegui traduzir para a nova linguagem
-- program5 :: Program
-- program5 = [While (EqB (FetchA "x") (PushA 0)) (StoreS "x" (SubA (FetchA "x") (PushA 1)))]

-- x = 1
assignment :: Program
assignment = [AssignA "x" (Num 1)]
assignment' :: Code
assignment' = [Push 1, Store "x"]

-- x = 1; y = x + 2
addition :: Program
addition = [AssignA "x" (Num 1), AssignA "y" (AddA (NumVar "x") (Num 2))]
addition' :: Code
addition' = [Push 1, Store "x", Push 2, Fetch "x", Add, Store "y"]

-- x = true; y = not x
negation :: Program
negation = [AssignB "x" (Bool True), AssignB "y" (NegB (BoolVar "x"))]
negation' :: Code
negation' = [Tru, Store "x", Fetch "x", Neg, Store "y"]

-- if (true) x = true else y = false
conditional :: Program
conditional = [If (Bool True) (AssignB "x" (Bool True)) (AssignB "y" (Bool False))]
conditional' :: Code
conditional' = [Tru, Branch [Tru, Store "x"] [Fals, Store "y"]]

-- ! VEJAM ESTE EXEMPLO DO PDF
-- y = 1; while (x != 1) do (y = y * x; x = x - 1)
factorial :: Program
factorial = [AssignA "y" (Num 1), While (NegB (EqA (NumVar "x") (Num 1))) (Seq (AssignA "y" (MultA (NumVar "y") (NumVar "x"))) (AssignA "x" (SubA (NumVar "x") (Num 1))))]
factorial' :: Code
factorial' = [Push 1, Store "y", Loop [Push 1, Fetch "x", Equ, Neg] [Fetch "x", Fetch "y", Mult, Store "y", Push 1, Fetch "x", Sub, Store "x"]]


code2Str :: Code -> String
code2Str [] = ""
code2Str (h:t)
  | null t = show h ++ code2Str t
  | otherwise = show h ++ "," ++ code2Str t

testCompiler :: Program -> String
testCompiler program = code2Str code
  where code = compile program

test1' :: Bool
test1' = testCompiler program1 == code2Str correct1

test2' :: Bool
test2' = testCompiler program2 == code2Str correct2

test3' :: Bool
test3' = testCompiler program3 == code2Str correct3

test4' :: Bool
test4' = testCompiler program4 == code2Str correct4

test5' :: Bool
test5' = testCompiler assignment == code2Str assignment'

test6' :: Bool
test6' = testCompiler addition == code2Str addition'

test7' :: Bool
test7' = testCompiler negation == code2Str negation'

test8' :: Bool
test8' = testCompiler conditional == code2Str conditional'

test9' :: Bool
test9' = testCompiler factorial == code2Str factorial'

tests' :: Bool
tests' = test1' && test2' && test3' && test4' && test5' && test6' && test7' && test8' && test9'
