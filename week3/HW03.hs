module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state variableName value =
    newFunction where
        newFunction :: String -> Int
        newFunction a
          | a == variableName = value
          | otherwise = state a

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var s) = state s
evalE _ (Val x) = x
evalE state (Op exp1 bop exp2) = case bop of
    Plus -> applyIntFunction (+)
    Minus -> applyIntFunction (-)
    Times -> applyIntFunction (*)
    Divide -> applyIntFunction div
    Gt -> applyBoolFunction (>)
    Ge -> applyBoolFunction (>=)
    Lt -> applyBoolFunction (<)
    Le -> applyBoolFunction (<=)
    Eql -> applyBoolFunction (==)
    where applyIntFunction :: (Int -> Int -> Int) -> Int
          applyIntFunction function = evalE state exp1 `function` evalE state exp2
          applyBoolFunction :: (Int -> Int -> Bool) -> Int
          applyBoolFunction function
            | evalE state exp1 `function` evalE state exp2 = 1
            | otherwise = 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar statement = case statement of
    Assign string exp -> DAssign string exp
    Incr string -> DAssign string $ Op (Var string) Plus (Val 1)
    If exp statement1 statement2 -> DIf exp (desugar statement1) (desugar statement2)
    While exp statement -> DWhile exp (desugar statement)
    For initialization loopCondition update body -> DSequence (desugar initialization) whileLoop where
        whileLoop = DWhile loopCondition (DSequence (desugar body) (desugar update))
    Sequence statement1 statement2 -> DSequence (desugar statement1) (desugar statement2)
    Skip -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state dietStatement = case dietStatement of
    DAssign string exp -> extend state string (evalE state exp)
    DIf condition trueStatement falseStatement ->
        if (evalE state condition == 1)
           then evalSimple state trueStatement
           else evalSimple state falseStatement
    DWhile condition dietStatement ->
        if (evalE state condition == 1)
           then evalSimple (evalSimple state dietStatement) (DWhile condition dietStatement)
           else state
    DSequence dietStatement1 dietStatement2 -> 
        evalSimple (evalSimple state dietStatement1) dietStatement2
    DSkip -> state

run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
