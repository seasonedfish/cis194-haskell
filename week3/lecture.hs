data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
           deriving Show


data FailableDouble = Failure
                    | OK Double
                    deriving Show


safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)


failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d


data Person = Person String Int Thing
    deriving Show


brent :: Person
brent = Person "Brent" 31  SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage


getAge :: Person -> Int
getAge (Person _ a _) = a


data IntList = Empty | Cons Int IntList
    deriving Show

intListProduct :: IntList -> Int
intListProduct Empty = 1
intListProduct list@(Cons firstNumber otherNumbers) = firstNumber * intListProduct otherNumbers


