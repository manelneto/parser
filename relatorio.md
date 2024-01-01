## Grupo

Grupo: **T12_G03**

* Manuel Ramos Leite Carvalho Neto - up202108744
* Matilde Isabel da Silva Simões - up202108782
* Pedro Vidal Marcelino - up202108754

A contribuição de cada elemento do grupo para o trabalho prático é 33.3%.

## Introdução 

Este relatório apresenta em detalhe a implementação de uma máquina virtual em Haskell, construída para executar instruções específicas. A tarefa proposta requereu a criação de um interpretador capaz de lidar com operações aritméticas, lógicas e controlo de fluxo, oferecendo uma solução robusta e flexível para a execução de programas de baixo nível. Ao longo deste relatório, exploraremos as estratégias adotadas, as decisões fundamentais na definição de tipos de dados e funções, além de discutir a implementação de aspectos cruciais para o funcionamento da máquina virtual.

## Parte 1

### Definição dos tipos 

Os tipos *Stack* e *State* foram definidos para representar a pilha e o estado da máquina virtual. A pilha é uma lista de valores que podem ser inteiros ou booleanos, enquanto que o estado é um *Map* de pares chave-valor onde as chaves são strings e os valores podem ser inteiros ou booleanos.

```haskell
type Stack = [Either Integer Bool]
type State = Map String (Either Integer Bool)
```
A utilização da estrutura de dados *Map* para representar o estado da máquina virtual oferece vantagens significativas, destacando-se pela eficiência na procura de valores associados às variáveis. A capacidade de acesso eficiente, juntamente com a facilidade de inserção e remoção dinâmica de variáveis, torna o *Map* uma escolha apropriada para lidar com o estado durante a execução do programa. Além disso, a ordenação natural das chaves facilita a representação do estado como uma lista de pares chave-valor, ordenados alfabeticamente, como especificado no enunciado. Essa abordagem simplifica o código, contribuindo para uma implementação mais clara e eficaz da máquina virtual.

### Definição das funções

As funções **createEmptyStack** e **createEmptyState** retornam versões vazias de pilha e do estado, respetivamente. Para a pilha, isso significa apenas uma lista vazia, enquanto para o estado, usamos a função empty do módulo Data.Map.

```haskell
createEmptyStack :: Stack 
createEmptyStack = []

createEmptyState :: State 
createEmptyState = empty
```

As funções **stack2Str** e **state2Str** convertem a pilha e o estado para strings. 

A função **stack2Str** percorre a pilha e converte cada valor para uma string, onde os valores são separados por vírgulas. Isso facilita a visualização da pilha durante a execução do programa.

```haskell	
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (h:t)
  | null t = case h of
      Left x -> show x ++ stack2Str t
      Right x -> show x ++ stack2Str t
  | otherwise = case h of
      Left x -> show x ++ "," ++ stack2Str t
      Right x -> show x ++ "," ++ stack2Str t 
```

A função **state2Str** converte o estado para uma lista de pares chave-valor, onde são listados em ordem alfabética e separados por vírgulas. 

```haskell
state2Str :: State -> String
state2Str state = case toList state of
  [] -> ""
  lst -> intercalate "," $ map (\(x, y) -> x ++ "=" ++ showValue y) lst  where
    showValue :: Either Integer Bool -> String
    showValue (Left intVal) = show intVal
    showValue (Right boolVal) = show boolVal
```

Cada instrução é representada por um construtor de dados no tipo **Inst**.

A função **run** desempenha o papel central na execução dos programas. Esta recebe como entrada a lista de instruções (**Code**), a pilha(**Stack**), e o estado (**State**). O padrão inicial verifica se a lista de instruções está vazia, indicando a conclusão bem-sucedida do programa. Neste caso, a função simplesmente retorna a pilha e o estado atuais.

```haskell
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
```

Caso haja instruções a serem processadas, o padrão subsequente utiliza a estrutura **case** para identificar o tipo de instrução no topo da lista. A partir daí, a lógica específica para cada instrução é executada, manipulando a pilha e o estado conforme necessário. Abaixo está exemplificada a lógica para a instrução **Add**.
    
```haskell
run :: (Code, Stack, State) -> (Code, Stack, State)
run (h:t, stack, state) = case h of
  Add -> run (t, add stack, state)

add :: Stack -> Stack
add [] = error "Run-time error"
add [x] = error "Run-time error"
add (x:y:t) = case (x, y) of
  (Left x, Left y) -> Left (x + y):t
  _ -> error "Run-time error"
```

Em resumo, a função **run** coordena a interpretação de cada instrução, garantindo a integridade da pilha e do estado e, indicando a falha do programa caso ocorra um erro de execução.


## Parte 2

### Definição dos tipos

O tipo **Aexp** representa expressões aritméticas. Este tipo é definido por um conjunto de construtores de dados: números inteiros (Num), variáveis (NumVar), adição (AddA), subtração (SubA), e multiplicação (MultA).

```haskell
data Aexp = Num Integer | NumVar String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp deriving Show
```

O tipo **Bexp** representa expressões booleanas. Este tipo é definido por um conjunto de construtores de dados: para valores booleanos (Bool), igualdade de expressões aritméticas (EqA), comparação menor ou igual de expressões aritméticas (LeA), igualdade de expressões booleanas (EqB), conjunção de expressões booleanas (AndB), e negação de expressões booleanas (NegB).

```haskell
data Bexp = Bool Bool | EqA Aexp Aexp | LeA Aexp Aexp | EqB Bexp Bexp | AndB Bexp Bexp | NegB Bexp deriving Show
```

O tipo **Stm** representa instruções imperativas. Este tipo é definido por um conjunto de construtores de dados: para atribuição de variável (Assign), instruções condicionais (If), e loops (While).

```haskell
data Stm = Assign String Aexp | If Bexp [Stm] [Stm] | While Bexp [Stm] deriving Show
```

O tipo **Program** representa um programa. Este tipo é definido como uma lista de instruções **Stm**. 

```haskell
type Program = [Stm]
```

### Definição das funções









