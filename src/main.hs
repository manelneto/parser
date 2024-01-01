import Data.Map (Map, toList, insert, empty, lookup)
import Data.List (intercalate)
import Data.Char (isAlpha, isAlphaNum, isLower, isDigit, digitToInt)


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


-- Shows a value that can be either an Integer or a Bool
showValue :: Either Integer Bool -> String
showValue (Left intVal) = show intVal
showValue (Right boolVal) = show boolVal

-- Returns the string representation of a stack in the form "x,y,z", where x is the top of the stack
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (h:t)
  | null t = showValue h ++ stack2Str t
  | otherwise = showValue h ++ "," ++ stack2Str t

-- Returns the string representation of a state in the form "x=1,y=False,z=2", 
-- where x, y and z are the variables in the state, ordered alphabetically
state2Str :: State -> String
state2Str state = case toList state of
  [] -> ""
  lst -> intercalate "," $ map (\(x, y) -> x ++ "=" ++ showValue y) lst


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
  Store x -> run (t, fst stackState, snd stackState) where stackState = store x stack state
  Branch c1 c2 -> run (c ++ t, s, state) where (c, s) = branch c1 c2 stack  
  Noop -> run (t, stack, state)
  Loop c1 c2 -> run(c ++ t, stack, state) where c = loop c1 c2
  Neg -> run (t, neg stack, state)
  And -> run (t, myAnd stack, state)


-- Instructions


-- Adds the top two integer values of the stack,
-- and pushes the result onto the top of the stack
add :: Stack -> Stack
add (x:y:t) = case (x, y) of
  (Left x, Left y) -> Left (x + y):t
  _ -> error "Run-time error"
add _ = error "Run-time error"


-- Multiplies the top two integer values of the stack,
-- and pushes the result onto the top of the stack
mult :: Stack -> Stack
mult (x:y:t) = case (x, y) of
  (Left x, Left y) -> Left (x * y):t
  _ -> error "Run-time error"
mult _ = error "Run-time error"


-- Subtracts the topmost element of the stack with the second topmost element,
-- and pushes the result onto the top of the stack
sub :: Stack -> Stack
sub (x:y:t) = case (x, y) of
  (Left x, Left y) -> Left (x - y):t
  _ -> error "Run-time error"
sub _ = error "Run-time error"


-- Compares the top two values of the stack for equality,
-- and pushes a boolean with the comparison result onto the top of the stack
eq :: Stack -> Stack
eq (x:y:t) = case (x, y) of
  (Left x, Left y) -> Right (x == y):t
  (Right x, Right y) -> Right (x == y):t
  _ -> error "Run-time error"
eq _ = error "Run-time error"


-- Determines whether the topmost stack element is less than or equal to the second topmost element,
-- and pushes a boolean with the comparison result onto the top of the stack
le :: Stack -> Stack
le (x:y:t) = case (x, y) of
  (Left x, Left y) -> Right (x <= y):t
  _ -> error "Run-time error"
le _ = error "Run-time error"


-- Pushes an integer onto the top of the stack
push :: Integer -> Stack -> Stack
push x s = Left x:s


-- Pushes the boolean value True onto the top of the stack
true :: Stack -> Stack
true s = Right True:s


-- Pushes the boolean value False onto the top of the stack
false :: Stack -> Stack
false s = Right False:s


-- Pushes the value bound to x onto the top of the stack
-- If x is not bound, raises a run-time error
fetch :: String -> Stack -> State -> Stack
fetch x s state = case Data.Map.lookup x state of
  Just val -> case val of
    Left intVal -> Left intVal : s
    Right boolVal -> Right boolVal : s
  Nothing -> error "Run-time error"


-- Pops the topmost element of the stack,
-- and updates the storage so that the popped value is bound to x
store :: String -> Stack -> State -> (Stack, State)
store x [] state = error "Run-time error"
store x (h:t) state = (t, insert x h state)


-- If the top of the stack is the value True, 
-- the stack is popped and c1 is to be executed next
-- Otherwise, if the top element of the stack is the value False,
-- then it will be popped and c2 will be executed next
branch :: Code -> Code -> Stack -> (Code, Stack)
branch c1 c2 [] = error "Run-time error"
branch c1 c2 (h:t) = case h of
  Right True -> (c1, t)
  Right False -> (c2, t)
  _ -> error "Run-time error"


-- Writes c1 to the beggining of the code list,
-- followed by a branch instruction that executes c2 followed by the loop instruction itself or a Noop 
loop :: Code -> Code -> Code
loop c1 c2 = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]


-- Negates the topmost boolean value of the stack,
-- and pushes the result onto the top of the stack
neg :: Stack -> Stack
neg [] = error "Run-time error"
neg (h:t) = case h of
  Right x -> Right (not x):t
  _ -> error "Run-time error"


-- Pops the top two values of the stack,
-- and pushes the result of the logical operation AND between them if they are both booleans
myAnd :: Stack -> Stack
myAnd (x:y:t) = case (x, y) of
  (Right x, Right y) -> Right (x && y):t
  _ -> error "Run-time error"
myAnd _ = error "Run-time error"



-- Aexp is an arithmetic expression
data Aexp = Num Integer | Var String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp deriving Show

-- Bexp is a boolean expression
data Bexp = Bool Bool | EqA Aexp Aexp | LeA Aexp Aexp | EqB Bexp Bexp | AndB Bexp Bexp | NegB Bexp deriving Show

-- Stm is a statement
data Stm = Assign String Aexp | If Bexp [Stm] [Stm] | While Bexp [Stm] deriving Show

-- Program is a list of statements
type Program = [Stm]


-- Compiles a program using the auxiliary functions compA and compB
compile :: Program -> Code
compile [] = []
compile (h:t) = case h of
  Assign var expA -> compA expA ++ [Store var] ++ compile t
  If expB s1 s2 -> compB expB ++ [Branch (compile s1) (compile s2)] ++ compile t
  While expB s -> Loop (compB expB) (compile s) : compile t


-- Compiles an arithmetic expression
compA :: Aexp -> Code
compA expA = case expA of
  Num num -> [Push num]
  Var var -> [Fetch var]
  AddA left right -> compA right ++ compA left ++ [Add]
  SubA left right -> compA right ++ compA left ++ [Sub]
  MultA left right -> compA right ++ compA left ++ [Mult]


-- Compiles a boolean expression
compB :: Bexp -> Code
compB expB = case expB of
  Bool bool -> if bool then [Tru] else [Fals]
  EqA left right -> compA right ++ compA left ++ [Equ]
  LeA left right -> compA right ++ compA left ++ [Le]
  EqB left right -> compB right ++ compB left ++ [Equ]
  AndB left right -> compB right ++ compB left ++ [And]
  NegB exp -> compB exp ++ [Neg]


data Token = AssignTok | IfTok | ThenTok | ElseTok | WhileTok | DoTok | SepTok | OpenTok | CloseTok
           | NumTok Integer | VarTok String | PlusTok | MinusTok | MultTok | NotTok | AndTok 
           | LessEqTok | EqTok | EqBoolTok | TrueTok | FalseTok deriving (Show, Eq)
 

-- Returns a program from a string using the lexer and an auxiliary function
parse :: String -> Program
parse input = parseAux (lexer input)


-- Recursive function that parses a list of tokens into a program
parseAux :: [Token] -> Program
parseAux [] = []
parseAux tokens =
  case instructionId of
    "assign" ->
      case parseAssign (head instruction) of
        Just (assignStm, []) -> assignStm : parseAux restTokens
        _ -> error "Parse error"

    "while" ->
      case parseWhile (head instruction) (instruction !! 1) of
        Just (whileStm, []) -> whileStm : parseAux restTokens
        _ -> error "Parse error"

    "if" ->
      case parseIfThenElse (head instruction) (instruction !! 1) (instruction !! 2) of
        Just (ifStm, []) -> ifStm : parseAux restTokens
        _ -> error "Parse error"

    _ -> error "Invalid instruction length"
  where
    (instructionId, instruction, restTokens) = getInstructions tokens


-- Returns a list of tokens that represents a single instruction and the rest of the tokens still to be parsed
getInstructions :: [Token] -> (String, [[Token]], [Token])
getInstructions (left : AssignTok : restTokens) = ("assign", assign, rest) where
  assign = [left : AssignTok : takeWhile (/= SepTok) restTokens]
  rest = drop 1 (dropWhile (/= SepTok) restTokens)
getInstructions (WhileTok : restTokens) = ("while", [afterWhile, afterDo], rest) where 
  firstSep = splitOnToken DoTok restTokens;
  afterWhile = fst firstSep;
  secondSep = splitOnToken SepTok (snd firstSep);
  afterDo = fst secondSep;
  rest = snd secondSep;
getInstructions (IfTok : restTokens) = ("if", [afterIf, afterThen, afterElse], rest)
  where
  firstSep = splitOnToken ThenTok restTokens;
  afterIf = fst firstSep;
  secondSep = splitOnToken ElseTok (snd firstSep);
  afterThen = fst secondSep;
  thirdSep = splitOnToken SepTok (snd secondSep);
  afterElse = fst thirdSep;
  rest = snd thirdSep;
getInstructions (OpenTok : restTokens) = getInstructions (init restTokens)


-- Splits a list of tokens into two lists of tokens:
-- the first one containing the tokens before the first ocurrence of the given token regarding parenthesis,
-- and the second one containing the tokens after that ocurrence of the token
splitOnToken :: Token -> [Token] -> ([Token], [Token])
splitOnToken token list = split list 0 []
  where
    split [] _ acc = (reverse acc, [])
    split (x:xs) count acc
      | x == token && count == 0 = (reverse acc, xs)
      | x == OpenTok = split xs (count + 1) (x:acc)
      | x == CloseTok = split xs (count - 1) (x:acc)
      | otherwise = split xs count (x:acc)


-- Statement Parsers


-- Parses an assignment statement
-- Returns a tuple with the assignment statement and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid assignment, returns Nothing
parseAssign :: [Token] -> Maybe (Stm, [Token])
parseAssign (VarTok varName : AssignTok : restTokens) = case parseAexp restTokens of 
  Just (varValue, _) -> Just (Assign varName varValue, []) 
  Nothing -> Nothing
parseAssign _ = Nothing


-- Parses an if-then-else statement
-- Returns a tuple with the if-then-else statement and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid if-then-else, returns Nothing
parseIfThenElse :: [Token] -> [Token] -> [Token] -> Maybe (Stm, [Token])
parseIfThenElse tokens1 tokens2 tokens3 = case parseBexp tokens1 of
  Just (condition, _) -> case parseAux tokens2 of
    [] -> Nothing 
    thenStm -> case parseAux tokens3 of
      [] -> Nothing
      elseStm -> Just (If condition thenStm elseStm, []) 
  _ -> Nothing 


-- Parses a while statement
-- Returns a tuple with the while statement and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid while, returns Nothing
parseWhile :: [Token] -> [Token] -> Maybe (Stm, [Token])
parseWhile tokens1 tokens2 = case parseBexp tokens1 of
  Just (condition, _) -> case parseAux tokens2 of 
    [] -> Nothing
    whileStm -> Just (While condition whileStm, []) 
  _ -> Nothing


-- Arithmetic Expression Parsers


-- Parses a sum or subtraction expression
-- Returns a tuple with the Sum/Subtraction expression and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid sum or subtraction expression, returns Nothing
parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens = case parseMultIntPar tokens of
  Just (expr1, PlusTok : restTokens1) -> case parseMultIntPar restTokens1 of
    Just (expr2, restTokens2) -> Just (AddA expr1 expr2, restTokens2)
    Nothing -> Nothing
  Just (expr1, MinusTok : restTokens1) -> case parseMultIntPar restTokens1 of
    Just (expr2, restTokens2) -> Just (SubA expr1 expr2, restTokens2)
    Nothing -> Nothing
  result -> result


-- Parses a multiplication expression
-- Returns a tuple with the Multiplication expression and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid multiplication expression, returns Nothing
parseMultIntPar :: [Token] -> Maybe (Aexp, [Token])
parseMultIntPar tokens = case parseIntPar tokens of 
  Just (expr1, MultTok : restTokens1) -> case parseMultIntPar restTokens1 of
    Just (expr2, restTokens2) -> Just (MultA expr1 expr2, restTokens2)
    Nothing -> Nothing
  result -> result


-- Parses an integer, variable expression or a parenthesis expression
-- Returns a tuple with the Integer or Variable expression and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid integer or variable expression, returns Nothing
parseIntPar :: [Token] -> Maybe (Aexp, [Token])
parseIntPar (NumTok n : restTokens) = Just (Num n, restTokens)
parseIntPar (VarTok var : restTokens) = Just (Var var, restTokens)
parseIntPar (OpenTok : restTokens1) = case parseAexp restTokens1 of
  Just (expr, CloseTok : restTokens2) -> Just (expr, restTokens2)
  _ -> Nothing
parseIntPar tokens = Nothing


-- Boolean Expression Parsers


-- Parses a boolean expression
-- Returns a tuple with the boolean expression and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid boolean expression, returns Nothing
parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens = case parseEqBNegEqALeBoolPar tokens of
  Just (expr1, AndTok : restTokens1) -> case parseBexp restTokens1 of
    Just (expr2, restTokens2) -> Just (AndB expr1 expr2, restTokens2)
    Nothing -> Nothing
  result -> result


-- Parses a boolean equality expression
-- Returns a tuple with the boolean equality expression and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid boolean equality expression, returns Nothing
parseEqBNegEqALeBoolPar :: [Token] -> Maybe (Bexp, [Token])
parseEqBNegEqALeBoolPar tokens = case parseNegEqALeBoolPar tokens of
  Just (expr1, EqBoolTok : restTokens1) -> case parseEqALeBoolPar restTokens1 of
    Just (expr2, restTokens2) -> Just (EqB expr1 expr2, restTokens2)
    Nothing -> Nothing
  result -> result


-- Parses a boolean negation expression
-- Returns a tuple with the boolean negation expression and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid boolean negation expression, returns Nothing
parseNegEqALeBoolPar :: [Token] -> Maybe (Bexp, [Token])
parseNegEqALeBoolPar (NotTok:restTokens) = case parseEqALeBoolPar restTokens of
  Just (expr,restTokens2) -> Just (NegB expr, restTokens2)
  result -> result
parseNegEqALeBoolPar tokens = parseEqALeBoolPar tokens


-- Parses an arithmetic equality expression
-- Returns a tuple with the arithmetic equality expression and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid arithmetic equality expression, returns Nothing
parseEqALeBoolPar :: [Token] -> Maybe (Bexp, [Token])
parseEqALeBoolPar tokens = case parseAexp tokens of
  Just (expr1, EqTok : restTokens1) -> case parseAexp restTokens1 of
    Just (expr2, restTokens2) -> Just (EqA expr1 expr2, restTokens2)
    Nothing -> Nothing
  _ -> parseLeBoolPar tokens


-- Parses an arithmetic less or equal expression
-- Returns a tuple with the arithmetic less or equal expression and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid arithmetic less or equal expression, returns Nothing
parseLeBoolPar :: [Token] -> Maybe (Bexp, [Token])
parseLeBoolPar tokens = case parseAexp tokens of
  Just (expr1, LessEqTok : restTokens1) -> case parseAexp restTokens1 of
    Just (expr2, restTokens2) -> Just (LeA expr1 expr2, restTokens2)
    Nothing -> Nothing
  _ -> parseBoolPar tokens


-- Parses a boolean expression between parenthesis
-- Returns a tuple with the boolean expression and the rest of the tokens still to be parsed
-- If the tokens do not represent a valid boolean expression between parenthesis, returns Nothing
parseBoolPar :: [Token] -> Maybe (Bexp, [Token])
parseBoolPar (TrueTok : restTokens) = Just (Bool True, restTokens)
parseBoolPar (FalseTok : restTokens) = Just (Bool False, restTokens)
parseBoolPar (OpenTok : restTokens1) = case parseBexp restTokens1 of
  Just (exp, CloseTok : restTokens2) -> Just (exp, restTokens2)
  _ -> Nothing
parseBoolPar _ = Nothing


-- Auxiliar functions that splits the input string into a list of tokens
-- Ignores words started by uppercase letters
lexer :: String -> [Token]
lexer [] = []
lexer (c:t) = case c of
  ' ' -> lexer t
  ';' -> SepTok : lexer t
  '(' -> OpenTok : lexer t
  ')' -> CloseTok : lexer t
  '+' -> PlusTok : lexer t
  '-' -> MinusTok : lexer t
  '*' -> MultTok : lexer t
  '<' | t /= [] && take 1 t == "=" -> LessEqTok : lexer (drop 1 t)
  '=' | t /= [] && take 1 t == "=" -> EqTok : lexer (drop 1 t)
      | otherwise -> EqBoolTok : lexer t
  ':' | t /= [] && take 1 t == "=" -> AssignTok : lexer (drop 1 t)
  'n' | t /= [] && take 2 t == "ot" -> NotTok : lexer (drop 2 t)
  'T' | t /= [] && take  3 t == "rue" -> TrueTok : lexer (drop 3 t)
  'F' | t /= [] && take 4 t == "alse" -> FalseTok : lexer (drop 4 t)
  'i' | t /= [] && take 1 t == "f" -> IfTok : lexer (drop 1 t)
  't' | t /= [] && take 3 t == "hen" -> ThenTok : lexer (drop 3 t)
  'e' | t /= [] && take 3 t == "lse" -> ElseTok : lexer (drop 3 t)
  'w' | t /= [] && take 4 t == "hile" -> WhileTok : lexer (drop 4 t)
  'd' | t /= [] && take 1 t == "o" -> DoTok : lexer (drop 1 t)
  'a' | t /= [] && take 2 t == "nd" -> AndTok : lexer (drop 2 t)
  _   | isAlpha c && isLower c -> let (var, rest) = span isAlphaNum (c:t) in VarTok var : lexer rest
      | isDigit c -> let (num, rest) = span isDigit (c:t) in NumTok (fromIntegral (stringToInt num)) : lexer rest
      | otherwise -> lexer t


-- Auxiliar function to convert a string into an integer
stringToInt :: String -> Int
stringToInt = foldl (\acc chr->10*acc+digitToInt chr) 0



-- Helper function to test the assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)


-- Helper function to test the parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)
