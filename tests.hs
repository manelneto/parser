-- Examples:
-- Teste 1 testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- Teste 2 testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- Teste 3 testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- Teste 4 testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- Teste 5 testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- Teste 6 testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- Teste 7 testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- Teste 8 testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- Teste 9 testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

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


test1' :: Bool
test1' = testParser "x := 5; x := x - 1;" == ("","x=4")

test2' :: Bool
test2' = testParser "x := 0 - 2;" == ("","x=-2")

test3' :: Bool
test3' = testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")

test4' :: Bool
test4' = testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")

test5' :: Bool
test5' = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")

test6' :: Bool
test6' = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")

test7' :: Bool
test7' = testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")

test8' :: Bool
test8' = testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")

test9' :: Bool
test9' = testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")

test10' :: Bool
test10' = testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")

test11' :: Bool
test11' = testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")

test12' :: Bool
test12' = testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

tests' :: Bool
tests' = test1' && test2' && test3' && test4' && test5' && test6' && test7' && test8' && test9' && test10' && test11' && test12'

-- Exemplo de programa simples: x := 10;
program1 :: Program
program1 = [Assign "x" (Num 10)]
correct1 :: Code
correct1 = [Push 10, Store "x"]

-- Exemplo de sequência de comandos: x := 5; y := x + 3;
program2 :: Program
program2 = [Assign "x" (Num 5), Assign "y" (AddA (Var "x") (Num 3))]
correct2 :: Code
correct2 = [Push 5, Store "x", Push 3, Fetch "x", Add, Store "y"]

-- Exemplo de condicional: if x = 0 then y := 1 else y := 2;
program3 :: Program
program3 = [If (EqA (Var "x") (Num 0)) [Assign "y" (Num 1)] [Assign "y" (Num 2)]]
correct3 :: Code
correct3 = [Push 0, Fetch "x", Equ, Branch [Push 1, Store "y"] [Push 2, Store "y"]]

-- Exemplo de while loop: while x > 0 do x := x - 1;
program4 :: Program
program4 = [While (NegB (LeA (Var "x") (Num 0))) [Assign "x" (SubA (Var "x") (Num 1))]]
correct4 :: Code
correct4 = [Loop [Push 0, Fetch "x", Le, Neg] [Push 1, Fetch "x", Sub, Store "x"]]

-- Exemplo de while loop: while x != 0 do x := x - 1;
program5 :: Program
program5 = [Assign "x" (Num 10),While (NegB (EqA (Var "x") (Num 0))) [Assign "x" (SubA (Var "x") (Num 1))]]

-- x = 1
assignment :: Program
assignment = [Assign "x" (Num 1)]
assignment' :: Code
assignment' = [Push 1, Store "x"]

-- x = 1; y = x + 2
addition :: Program
addition = [Assign "x" (Num 1), Assign "y" (AddA (Var "x") (Num 2))]
addition' :: Code
addition' = [Push 1, Store "x", Push 2, Fetch "x", Add, Store "y"]

-- y = 1; while (x != 1) do (y = y * x; x = x - 1)
factorial :: Program
factorial = [Assign "y" (Num 1), While (NegB (EqA (Var "x") (Num 1))) [Assign "y" (MultA (Var "y") (Var "x")), Assign "x" (SubA (Var "x") (Num 1))]]
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

test1'' :: Bool
test1'' = testCompiler program1 == code2Str correct1

test2'' :: Bool
test2'' = testCompiler program2 == code2Str correct2

test3'' :: Bool
test3'' = testCompiler program3 == code2Str correct3

test4'' :: Bool
test4'' = testCompiler program4 == code2Str correct4

test5'' :: Bool
test5'' = testCompiler assignment == code2Str assignment'

test6'' :: Bool
test6'' = testCompiler addition == code2Str addition'

test7'' :: Bool
test7'' = testCompiler factorial == code2Str factorial'

tests'' :: Bool
tests'' = test1' && test2' && test3' && test4' && test5' && test6' && test7'
