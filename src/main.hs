import Data.Map (Map, fromList, toList, (!), member, insert, empty, lookup)
import Data.List (sort, intercalate)

-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
  
type Code = [Inst]

-- Stack is a list of either Integers or Bools

data Stack = Stack [Either Integer Bool]

-- Storage is a list of variables and their values in the form (VarName, Value)
data State = State (Map String (Either Integer Bool))

myPush:: Either Integer Bool -> Stack -> Stack
myPush x (Stack s) = Stack (x:s)

--  |||||||||||||||||||||||||||||||||||
--                  Stack
--  |||||||||||||||||||||||||||||||||||

-- createEmptyStack :: Stack
-- Function that returns an empty Stack
createEmptyStack :: Stack 
createEmptyStack = Stack []



-- Returns the string representation of a stack in the form "x,y,z", where x is the top of the stack
stack2Str :: Stack -> String
stack2Str (Stack []) = ""
stack2Str (Stack (h:t))
  | null t = case h of
      Left x -> show x ++ stack2Str (Stack t)
      Right x -> show x ++ stack2Str (Stack t)
  | otherwise = case h of
      Left x -> show x ++ "," ++ stack2Str (Stack t)
      Right x -> show x ++ "," ++ stack2Str (Stack t)
    

--  |||||||||||||||||||||||||||||||||||
--                  State 
--  |||||||||||||||||||||||||||||||||||

createEmptyState :: State 
createEmptyState = State empty

state2Str :: State -> String
state2Str (State m) = case toList m of
  [] -> ""
  lst -> intercalate "," $ map (\(x, y) -> x ++ "=" ++ showValue y) lst  where
    showValue :: Either Integer Bool -> String
    showValue (Left intVal) = show intVal
    showValue (Right boolVal) = show boolVal

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((h:t), stack, state) = case h of
  Add -> run (t, add stack, state)
  Mult -> run (t, mult stack, state)
  Sub -> run (t, sub stack, state)
  Equ -> run (t, eq stack, state)
  Le -> run (t, le stack, state)
  Push n -> run (t, push n stack, state)
  Tru -> run (t, true stack, state)
  Fals -> run (t, false stack, state)
  Fetch x -> run (t, (fetch x stack state), state)
  Store x -> run (t, fst stackState, snd stackState) where  stackState = store x stack state
  Branch c1 c2 -> run (c ++ t, s, state) where (c, s) = branch c1 c2 stack  
  Noop -> run (t, stack, state)
  Loop c1 c2 -> run(c ++ t, stack, state) where c = loop c1 c2
  Neg -> run (t, neg stack, state)
  And -> run (t, myAnd stack, state)


add :: Stack -> Stack
add (Stack []) = error "Run-time error"
add (Stack [x]) = error "Run-time error"
add (Stack (x:y:t)) = case (x, y) of
  (Left x, Left y) -> Stack (Left (x + y):t)
  _ -> error "Run-time error"

mult :: Stack -> Stack
mult (Stack []) = error "Run-time error"
mult (Stack [x]) = error "Run-time error"
mult (Stack (x:y:t)) = case (x, y) of
  (Left x, Left y) -> Stack (Left (x * y):t)
  _ -> error "Run-time error"

sub :: Stack -> Stack
sub (Stack []) = error "Run-time error"
sub (Stack [x]) = error "Run-time error"
sub (Stack (x:y:t)) = case (x, y) of
  (Left x, Left y) -> Stack (Left (x - y):t)
  _ -> error "Run-time error"

eq :: Stack -> Stack
eq (Stack []) = error "Run-time error"
eq (Stack [x]) = error "Run-time error"
eq (Stack (x:y:t)) = case (x, y) of
  (Left x, Left y) -> Stack (Right (x == y):t)
  (Right x, Right y) -> Stack (Right (x == y):t)
  _ -> error "Run-time error"

le :: Stack -> Stack
le (Stack []) = error "Run-time error"
le (Stack [x]) = error "Run-time error"
le (Stack (x:y:t)) = case (x, y) of
  (Left x, Left y) -> Stack (Right (x <= y):t)
  _ -> error "Run-time error"


push :: Integer -> Stack -> Stack
push x (Stack s) = Stack (Left x:s)

true :: Stack -> Stack
true (Stack s) = Stack (Right True:s)

false :: Stack -> Stack
false (Stack s) = Stack (Right False:s)

fetch :: String -> Stack -> State -> Stack
fetch x (Stack s) (State m) = case Data.Map.lookup x m of
  Just val -> case val of
    Left intVal -> Stack (Left intVal : s)
    Right boolVal -> Stack (Right boolVal : s)
  Nothing -> error "Run-time error"

store :: String -> Stack -> State -> (Stack, State)
store x (Stack []) (State s) = error "Run-time error"
store x (Stack (h:t)) (State s) = 
    (Stack t, State (insert x h s))


branch :: Code -> Code -> Stack -> (Code, Stack)
branch c1 c2 (Stack []) = error "Run-time error"
branch c1 c2 (Stack (h:t)) = case h of
  Right True -> (c1, Stack t)
  Right False -> (c2, Stack t)
  _ -> error "Run-time error"

loop :: Code -> Code -> Code
loop c1 c2 = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]

neg :: Stack -> Stack                   -- Ver caso Inteiro ????
neg (Stack []) = error "Run-time error"
neg (Stack (h:t)) = case h of
  Right x -> Stack (Right (not x):t)
  _ -> error "Run-time error"

myAnd :: Stack -> Stack
myAnd (Stack []) = error "Run-time error"
myAnd (Stack [x]) = error "Run-time error"
myAnd (Stack (x:y:t)) = case (x, y) of
  (Right x, Right y) -> Stack (Right (x && y):t)
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