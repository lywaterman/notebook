import qualified Data.Vector as V
import Control.Monad.State
import qualified Data.Map as M

data Flower = DIAMOND | CLUB | HEART | SPADE | JOKER0 | JOKER1 deriving (Show, Eq, Ord)
type Value = Int

data Poker = Poker {flower::Flower, value::Value} deriving (Show, Ord)

data PokerType = Single |  Pair | KingBomb | Triangle | FourBomb | ThreePlusOne | ThreePlusTwo | FourPlusTwo |
                    Series | SeriesPair | SeriesTriangle | SeriesThreePlusOne | SeriesThreePlusTwo deriving (Show)
data PokerList = PokerList {pokerList :: (V.Vector Poker)} deriving (Show)

--继承Eq，如果值相等，那么他们相等
instance Eq Poker where
    x == y = (value x) == (value y)

data PokerListState = PokerListState {
    pokerList1 :: (V.Vector Poker),
    solePokerIndex :: (V.Vector Int) --Index is up to pokerList
} deriving (Show)


isKing :: Poker -> Bool
isKing (Poker JOKER0 _)  = True
isKing (Poker JOKER1 _)  = True
isKing _                 = False

addCountToMap :: Maybe Int -> Maybe Int
addCountToMap Nothing   = Just 1
addCountToMap (Just x)  = Just (x+1)

allTheSame :: PokerList -> Bool
allTheSame x = let xv = (pokerList x) in 
                    V.all (== V.head xv) (V.tail xv)

alterM :: [Poker] -> M.Map Int Int -> M.Map Int Int
alterM (x:xs) m = M.alter addCountToMap (value x) (alterM xs m)
alterM [] m     = m

--M.alter addCountToMap 1 m
getPokerListValueDiff :: PokerList -> [(Int,Int)]
getPokerListValueDiff x = (M.toList m) where 
                                xv = V.toList (pokerList x)
                                m = alterM xv M.empty

getTestPokerList :: PokerList
getTestPokerList = PokerList . V.fromList $ [(Poker JOKER0 123), (Poker JOKER1 123)]

getPokerListType :: PokerList -> Either PokerType PokerList
getPokerListType x = do 
                        y <- isSingle x
                        z <- isPair y
                        s <- isKingBomb z 
                        return s

isSingle :: PokerList -> Either PokerType PokerList 
isSingle x = let xv = (pokerList x) in 
                if V.length xv == 1 
                    then Left Single
                    else Right x

isPair :: PokerList -> Either PokerType PokerList
isPair x = let xv = (pokerList x) in
                if V.length xv == 2 && ((xv V.! 0) == xv V.! 1)
                    then Left Pair
                    else Right x

isKingBomb :: PokerList -> Either PokerType PokerList
isKingBomb x = let xv = (pokerList x) in
                    if V.length xv == 2 && isKing (xv V.! 0) && isKing (xv V.! 1)
                        then Left KingBomb
                        else Right x

--isTriangle :: PokerList -> Either PokerType PokerList
--isTriangle x = let xv = (pokerList x) in
--                    if V.length xv == 3 && ((xv V.! 0) == (xv V.! 1) == (xv V.! 2))
--                        then Left Triangle
--                        else Right x

getPokerByGIndex :: Int -> Poker
getPokerByGIndex x = (pokerList getAllPokers) V.! x

getAllPokers :: PokerList
getAllPokers =  PokerList . V.fromList $ [
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

