import Control.Monad.State
import Data.List
import qualified Data.Map as M

data Flower = DIAMOND | CLUB | HEART | SPADE | JOKER0 | JOKER1 deriving (Show, Eq, Ord)
type Value = Int

data Poker = Poker {flower::Flower, value::Value} deriving (Show)

data PokerType = Single |  Pair | KingBomb | Triangle | FourBomb | ThreePlusOne | ThreePlusTwo | FourPlusTwo |
                    Series | SeriesPair | SeriesTriangle | SeriesThreePlusOne | SeriesThreePlusTwo deriving (Show)
data PokerList = PokerList {pokerList :: [Poker]} deriving (Show)

fromStoAandS :: Int -> (String,Int)
fromStoAandS c | c `mod` 5 == 0 = ("foo",c+1)
               | otherwise = ("bar",c+1)

--继承Eq，如果值相等，那么他们相等
instance Eq Poker where
    x == y = (value x) == (value y)

instance Ord Poker where
    x `compare` y = if ((value x) < (value y)) 
                        then LT
                        else if ((value x) > (value y))
                            then GT
                            else EQ 


data PokerListState = PokerListState {
    pokerList1 :: [Poker],
    solePokerIndex :: [Int] --Index is up to pokerList
} deriving (Show)

--构建一个StateMonad
getPokerListState :: PokerList -> State PokerListState Int
getPokerListState x = state $ (\_ -> (0, PokerListState (sort $ pokerList x) []))

changePokerListState :: PokerListState -> State PokerListState ()
changePokerListState x = put x


isKing :: Poker -> Bool
isKing (Poker JOKER0 _)  = True
isKing (Poker JOKER1 _)  = True
isKing _                 = False

isTwo :: Poker -> Bool
isTwo (Poker _ 2) = True
isTwo _           =  False

addCountToMap :: Maybe Int -> Maybe Int
addCountToMap Nothing   = Just 1
addCountToMap (Just x)  = Just (x+1)

allTheSame :: PokerList -> Bool
allTheSame x = let xv = (pokerList x) in 
                    all (== head xv) (tail xv)

alterM :: [Poker] -> M.Map Int Int -> M.Map Int Int
alterM (x:xs) m = M.alter addCountToMap (value x) (alterM xs m)
alterM [] m     = m

valueDiffToDiff :: [(Int, Int)] -> [Int]
valueDiffToDiff ((_, diff):xs) = sort $ (valueDiffToDiff xs) ++ [diff]
valueDiffToDiff [] = []

getPokerListValueDiff :: PokerList -> [Int]
getPokerListValueDiff x = valueDiffToDiff (M.toList m) where 
                                xv = pokerList x
                                m = alterM xv M.empty

getPokerValueList :: PokerList -> [Int]  
getPokerValueList x = (go . pokerList) x where
                        go ((Poker _ value):xs) = sort $ (go xs) ++ [value]
                        go [] = []



getTestPokerList :: PokerList
getTestPokerList = PokerList [(Poker JOKER0 123), (Poker JOKER1 123)]

getPokerListType :: PokerList -> Either PokerType PokerList
getPokerListType x = do 
                        y <- isSingle x
                        z <- isPair y
                        s <- isKingBomb z 
                        return s

isSingle :: PokerList -> Either PokerType PokerList 
isSingle x = let xv = (pokerList x) in 
                if length xv == 1 
                    then Left Single
                    else Right x

isPair :: PokerList -> Either PokerType PokerList
isPair x = let xv = (pokerList x) in
                if length xv == 2 && ((xv !! 0) == xv !! 1)
                    then Left Pair
                    else Right x

isKingBomb :: PokerList -> Either PokerType PokerList
isKingBomb x = let xv = (pokerList x) in
                    if length xv == 2 && isKing (xv !! 0) && isKing (xv !! 1)
                        then Left KingBomb
                        else Right x

isTriangle :: PokerList -> Either PokerType PokerList
isTriangle x = let xv = (pokerList x) in
                   if length xv == 3 && (allTheSame x)
                       then Left Triangle
                       else Right x

isFourBomb :: PokerList -> Either PokerType PokerList
isFourBomb x = let xv = (pokerList x) in 
                    if length xv == 4 && (allTheSame x)
                        then Left FourBomb
                        else Right x

isThreePlusOne :: PokerList -> Either PokerType PokerList
isThreePlusOne x = let xv = (pokerList x) in
                    if length xv == 4 && getPokerListValueDiff(x) == [1,3]
                        then Left ThreePlusOne
                        else Right x

isThreePlusTwo :: PokerList -> Either PokerType PokerList
isThreePlusTwo x = let xv = (pokerList x) in
                    if length xv == 5 && getPokerListValueDiff(x) == [2,3]
                        then Left ThreePlusTwo
                        else Right x
isFourPlusTwo :: PokerList -> Either PokerType PokerList
isFourPlusTwo x = let xv = (pokerList x) in
                    if length xv == 6 && getPokerListValueDiff(x) == [1,1,4]
                        then Left FourPlusTwo
                        else Right x 

isSeries :: PokerList -> Either PokerType PokerList
isSeries x = let xv = sort $ pokerList x; 
                 len = (length xv); 
                 valuelist = getPokerValueList x in
                        if len <= 12 && len >= 5 && 
                            not (isTwo $ last xv) && 
                            not (isKing $ last xv) && 
                            valuelist == [(head valuelist)..(last valuelist)]
                            then Left Series
                            else Right x

checkPairList :: [Int] -> Maybe Int 
checkPairList (x:(x1:[])) = if x == x1
                                then Just [] >>= checkPairList
                                else Nothing
checkPairList (x:(x1:xs)) = if x == x1 && (x+1) == (head xs)
                                then Just xs >>= checkPairList
                                else Nothing
checkPairList []          = Just 1
checkPairList _           = Nothing


checkTriangleList :: [Int] -> Maybe Int 
checkTriangleList (x:(x1:(x2:[]))) = if x == x1 && x == x2
                                then Just [] >>= checkTriangleList
                                else Nothing
checkTriangleList (x:(x1:(x2:xs))) = if x == x1 && x == x2 && (x+1) == (head xs)
                                then Just xs >>= checkTriangleList
                                else Nothing
checkTriangleList []          = Just 1
checkTriangleList _           = Nothing

isSeriesPair :: PokerList -> Either PokerType PokerList
isSeriesPair x = let xv = sort $ pokerList x; 
                     len = (length xv); 
                     valuelist = getPokerValueList x in
                        if len <= 20 && len >= 6 && 
                           (mod len 2) == 0 && 
                           (checkPairList valuelist) == Just 1
                            then Left SeriesPair
                            else Right x


isSeriesTriangle :: PokerList -> Either PokerType PokerList
isSeriesTriangle x = let xv = sort $ pokerList x; len = (length xv); valuelist = getPokerValueList x in
                        if len <= 18 && len >= 6 && 
                           (mod len 3) == 0 && 
                           (checkTriangleList valuelist) == Just 1
                            then Left SeriesTriangle
                            else Right x

--isSeriesThreePlusOne :: PokerList -> Either PokerType PokerList
--isSeriesThreePlusOne x = 


getPokerByGIndex :: Int -> Poker
getPokerByGIndex x = (pokerList getAllPokers) !! x

getAllPokers :: PokerList
getAllPokers =  PokerList  [
    (Poker  DIAMOND    3 ),
    (Poker  CLUB       3 ),
    (Poker  HEART      3 ),
    (Poker  SPADE      3 ),

    (Poker  DIAMOND    4 ),
    (Poker  CLUB       4 ),
    (Poker  HEART      4 ),
    (Poker  SPADE      4 ),

    (Poker  DIAMOND    5 ),
    (Poker  CLUB       5 ),
    (Poker  HEART      5 ),
    (Poker  SPADE      5 ),

    (Poker  DIAMOND    6 ),
    (Poker  CLUB       6 ),
    (Poker  HEART      6 ),
    (Poker  SPADE      6 ),

    (Poker  DIAMOND    7 ),
    (Poker  CLUB       7 ),
    (Poker  HEART      7 ),
    (Poker  SPADE      7 ),

    (Poker  DIAMOND    8 ),
    (Poker  CLUB       8 ),
    (Poker  HEART      8 ),
    (Poker  SPADE      8 ),

    (Poker  DIAMOND    9 ),
    (Poker  CLUB       9 ),
    (Poker  HEART      9 ),
    (Poker  SPADE      9 ),

    (Poker  DIAMOND    10 ),
    (Poker  CLUB       10 ),
    (Poker  HEART      10 ),
    (Poker  SPADE      10 ),

    (Poker  DIAMOND    11 ),
    (Poker  CLUB       11 ),
    (Poker  HEART      11 ),
    (Poker  SPADE      11 ),

    (Poker  DIAMOND    12 ),
    (Poker  CLUB       12 ),
    (Poker  HEART      12 ),
    (Poker  SPADE      12 ),

    (Poker  DIAMOND    13 ),
    (Poker  CLUB       13 ),
    (Poker  HEART      13 ),
    (Poker  SPADE      13 ),

    (Poker  DIAMOND    14 ),
    (Poker  CLUB       14 ),
    (Poker  HEART      14 ),
    (Poker  SPADE      14 ),

    (Poker  DIAMOND    15 ),
    (Poker  CLUB       15 ),
    (Poker  HEART      15 ),
    (Poker  SPADE      15 ),

    (Poker  JOKER0 16 ),
    (Poker  JOKER1 17 )]

