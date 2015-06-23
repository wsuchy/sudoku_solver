module Main where

import Control.Applicative
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Control.Exception
main = do
    takeAllLines >> putStrLn "\nOK"

takeAllLines :: IO [[String]]
takeAllLines = sequence ( take 9 $ repeat takeOneLine)

takeOneLine :: IO [String]
takeOneLine = do
    x <- sequence ( take 9 $ repeat takeOneChar)
    putStrLn " "
    return x

takeOneChar:: IO (String)
takeOneChar = do
     z <- liftA check getChar
     putStr z
     return (z)

check :: Char -> String
check x
    | x > '9' = "\x08_"
    | x < '0' = "\x08_"
    | otherwise  = '\x08' : x : []



-- l1 = [ ["1", "8", "_", "_","_","9" , "_","2","_"], ["_", "_", "_", "8","_","5" , "_","_","1"], ["9", "5", "_", "4","2","_" , "8","_","_"], ["_", "_", "2", "_","_","_" , "_","_","_"], ["5", "_", "_", "_","_","_" , "_","_","8"], ["_", "_", "_", "_","_","_" , "1","_","_"], ["_", "_", "1", "_","5","4" , "_","3","2"], ["3", "_", "_", "9","_","7" , "_","_","_"], ["_", "4", "_", "3","_","_" , "_","1","6"] ]
-- l1 = [["5", "_", "_", "_","_","4" , "_","_","6"], ["_", "_", "7", "_","_","8" , "1","_","_"],  [ "-","9","-", "-","6","-","-","4","-"], ["8", "3", "_", "-","5","_" , "-","_","_"], ["_", "_", "6", "7","_","3" , "9","_","_"], ["_", "_", "_", "_","8","_" , "_","3","2"], ["_", "2", "_", "_","3","_" , "-","7","_"], ["_", "_", "8", "5","-","-" , "2","-","-"], ["6", "_", "_", "4","_","-" , "_","_","3"] ]
l1 = [["-", "_", "9", "_","7","-" , "_","2","-"], ["3", "_", "-", "_","_","-" , "9","_","7"],  [ "-","1","-", "-","-","-","8","-","-"], ["-", "5", "6", "8","-","_" , "-","_","_"], ["_", "_", "-", "7","_","9" , "-","_","_"], ["_", "_", "_", "_","-","5" , "3","8","-"], ["_", "-", "8", "_","-","_" , "-","5","_"], ["5", "_", "2", "-","-","-" , "-","-","6"], ["-", "6", "_", "4","_","-" , "1","_","-"] ]
--l1 =
data Field = FieldNum Int
            | FieldPossible (Set.Set Int)
            | FieldEmpty
     deriving (Show)

instance Read Field where
    -- readsPrec is the main function for parsing input
    readsPrec _ x =
        if x <= "9" && x >="1"
        then [(FieldNum (read x ::Int), "")]
        else[ ( FieldPossible ( Set.fromList [1,2,3,4,5,6,7,8,9]), "")]

indexByCoords :: Num  a => a -> a -> a
indexByCoords x y = x + (9*y)

multiListToField :: [[String]] -> Seq.Seq Field
multiListToField ll =
   let  l1 = foldr (++) [] ll
       in   Seq.fromList ( map (\x -> read x :: Field) l1 )



getIndexesForRow :: (Num t, Enum t) => t -> [t]
getIndexesForRow r =  [ indexByCoords i  r |i <- [0..8]]

getIndexesForCol ::  (Num t, Enum t) => t -> [t]
getIndexesForCol c =  [ indexByCoords c  i |i <- [0..8]]

-- 0,1,2 3,4,5, 6,7,8
getIndexesForQuadrant ::  (Num t, Enum t,Integral t) => t ->  t -> [t]
getIndexesForQuadrant x y =
    let startX = (quot x 3) * 3
        startY = (quot y 3) * 3
    in [ indexByCoords x' y' |x' <- [startX .. startX+2], y' <- [startY..startY+2] ]


seqIndexHasDigit :: Seq.Seq Field -> Int -> Int -> Bool
seqIndexHasDigit s i d =
    let elem = Seq.index s i
    in case (elem) of
        FieldNum x -> x == d
        _ -> False

generalRulesIndexes :: Integral a => a -> a -> [[a]]
generalRulesIndexes x y = [getIndexesForRow y, getIndexesForCol x, getIndexesForQuadrant x y]

checkListForDigit ::  Seq.Seq Field -> [Int] -> Int -> Bool
checkListForDigit s is d  =
    foldr (\i c -> (seqIndexHasDigit s i d) || c  ) False is == True

-- main sudoku rules
canHaveDigit s (x, y) d =
    let
    indexes = Set.toList $ Set.fromList $ foldr (++) [] $ generalRulesIndexes x y
    found = checkListForDigit s indexes d
    in not found

-- check if proposed digit is unique in row, col and quadrant
propositionUnique s (x,y) d =
    let
        currentIndex = indexByCoords x y
        checkOneForProposition i c
            | i == currentIndex  = c
            | otherwise = case (Seq.index s i) of
                                    (FieldPossible ds) -> Set.notMember d ds && c
                                    _ -> c
        checkListForProposition l = foldr checkOneForProposition True l
    in any id $ map checkListForProposition $ generalRulesIndexes x y


normalizeField :: Set.Set Int -> Field
normalizeField f
    | Set.null f = FieldEmpty
    | Set.size f == 1 = FieldNum $ Set.elemAt 0 f
    | otherwise =  FieldPossible f

solveOneNumberInSequence ::  Seq.Seq Field -> (Int,Int)-> Either String (Seq.Seq Field)
solveOneNumberInSequence s (x,y) =
    let i = indexByCoords x y
        filteredHelper fnc f = Set.filter (\l -> fnc s (x,y) l ) f

        orderedCheck f =
            let
                byGeneralRules = filteredHelper canHaveDigit f
                byPropositions = filteredHelper propositionUnique byGeneralRules
            in  if Set.null byPropositions then  byGeneralRules else byPropositions

        fixElement e =
            case (e) of
                (FieldPossible f) -> normalizeField  $ orderedCheck f
                _ -> e
        old = Seq.index s i
        new = fixElement old
    in case (new) of
        (FieldEmpty) -> Left  ("No solution in" ++ show (x,y))
        _ -> Right (Seq.update i new s)

solveAllNumbersInSequence :: Seq.Seq Field -> Either String (Seq.Seq Field)
solveAllNumbersInSequence s =
    let coordinates = [(x,y) | x <- [0..8], y <- [0..8] ]
        fnc cs s' = case (cs) of
            [] -> s'
            (c:cs) ->  case (s') of
                (Right s'') -> fnc cs $ solveOneNumberInSequence s'' c
                (Left _) -> s'

    in fnc coordinates (Right s)

sumAllPosibilites :: Seq.Seq Field -> Int
sumAllPosibilites s =
    let
    sum i e c = case e of
         (FieldPossible n) -> Set.size n + c
         _ -> c
    in Seq.foldrWithIndex sum 0 s

findSolution ::Either String (Seq.Seq Field) -> Either String (Seq.Seq Field)
findSolution s =
    let
    fnc s' p' =
        case (s') of
            (Left _) -> s'
            (Right s'') -> if sumAllPosibilites s'' /= p'
                           then  fnc (solveAllNumbersInSequence s'') (sumAllPosibilites s'')
                           else s'
    in fnc s 0



findSmallestToGuess s =
    let
        checkSeqElement num e = case e of
            (FieldPossible f) -> Set.size f == num
            otherwise -> False
        helper num = case (Seq.findIndexL (checkSeqElement num) s) of
                            Just i -> i
                            Nothing -> helper num + 1
    in helper 2

guessAndChange s n =
    let
        i = findSmallestToGuess s
        read i = case (Seq.index s i) of
                FieldPossible f -> f
                _ -> error "Shouldnt happend"

        old = read i
        upd x = Seq.update i x s

    in
         if n >= Set.size old
          then Left ("No options left: " ++ (show i) ++ " try:" ++ (show n))
          else Right ( upd $ FieldNum ( Set.elemAt n old))


fnc1 s n =
    let s1 = case s of
                Right s' -> guessAndChange s' n
                Left _ -> s
    in case s1 of
        Right s' -> case findSolution s1 of
                    Left s2 -> fnc1 s (n + 1)
                    Right s2 -> if  sumAllPosibilites s2 == 0
                                then Right s2
                                else case (fnc1 (Right s2) 0) of
                                        Left s3 -> fnc1 s (n + 1)
                                        Right s3 -> Right s3

        Left s' -> s1




-- prettyShow ::  Seq.Seq Field -> String
prettyShow s  =
    let
    nl i
        | rem i 9 == 0 =  " | "
        | rem i 3 == 0 =  " * "
        | otherwise = " "
    str i e c = case (e) of
        (FieldNum n) -> nl i ++ show (n) ++ c
        (FieldPossible n) -> nl i ++ "{" ++  (Set.foldr (\x c -> show x ++ "," ++ c)  "" n ) ++ "}" ++ c
        -- _ -> nl i ++ "?" ++ c
    in case s of
        Right s' -> Seq.foldrWithIndex str "" s'
        Left s' -> s'


s = multiListToField l1