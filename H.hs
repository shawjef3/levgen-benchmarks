{-# LANGUAGE BangPatterns #-}
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import System.Environment
import qualified Data.List as L
import System.Random
import Random.Xorshift
import Control.Monad.Random

type Pos = (Int,Int)

data Tile = Wall | Space deriving (Show)

data Room = Room
    { rx, ry, rw, rh :: !Int
    } deriving (Show)

data Lev = Lev
    { lRooms :: ![Room]
    , lTiles :: [Tile]
    }

levDim, minWid, maxWid :: Int
levDim = 50
minWid = 2
maxWid = 8

genRoom :: [Room] -> Rand Xorshift Room
genRoom rsDone = do
    x <- getRandom
    y <- getRandom
    w <- getRandom
    h <- getRandom
    let x' = rem x levDim
    let y' = rem y levDim
    let w' = rem w maxWid + minWid
    let h' = rem h maxWid + minWid
    let testRoom = Room {rx = x', ry = y', rw= w', rh= h'}
    if checkBound testRoom || checkColl testRoom rsDone
        then genRoom rsDone
        else return testRoom

genRooms :: Int -> Rand Xorshift [Room]
genRooms n = genRoomsAux n []
    where
        genRoomsAux :: Int -> [Room] -> Rand Xorshift [Room]
        genRoomsAux 0 rooms = return rooms
        genRoomsAux n rooms = do
            room <- genRoom rooms
            genRoomsAux (n-1) (room:rooms)

checkBound :: Room -> Bool
checkBound (Room x y w h) =
    x<=0 || y<=0 || x+w >= levDim || y+h >= levDim

checkColl :: Room -> [Room] -> Bool
checkColl room = any (roomHitRoom room)

roomHitRoom :: Room -> Room -> Bool
roomHitRoom (Room x y w h) (Room x2 y2 w2 h2)
    = not ((x2+w2+1) < x || x2 > (x+w+1)
        || (y2+h2+1) < y || y2 > (y+h+1))

inRoom :: Pos -> Room -> Bool
inRoom (x, y) (Room rx ry rw rh) =
        (rx <= x) && (x < rx + rw)
    &&  (ry <= y) && (y < ry + rh)

showTiles :: [Tile] -> String
showTiles = unlines . chunksOf levDim . map toChar
  where toChar Wall = '0'
        toChar Space = '1'

genLevs :: Int -> [Lev]-> Rand Xorshift [Lev]
genLevs 0 done = return done
genLevs n done = do
    rooms <- genRooms 50000
    let tiles = map (toTile rooms) [1 .. levDim ^ 2]
    genLevs (n-1) (Lev{lRooms = rooms, lTiles = tiles}:done)
  where
    toTile rooms n = if (any (toPos n `inRoom`) rooms) then Space else Wall
    toPos n = let (y, x) = quotRem n levDim in (x, y)

biggestLev :: [Lev] -> Lev
biggestLev = L.maximumBy (comparing (length . lRooms))

main :: IO ()
main = do
    (v:_) <- getArgs
    putStr "The random seed is: "
    putStrLn v
    gen <- newXorshift
    let levs = evalRand (genLevs 100 []) gen
    putStr $ showTiles $ lTiles $ biggestLev levs
