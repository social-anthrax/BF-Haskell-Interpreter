module Compiler 
    (runBf,
    parseBrainfuck, 
    runBrainFuckFromFile,
    balanced,
    Ops(..)
    )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Char ( chr
                 , ord )
import System.IO (hFlush, stdout)


allowedChars :: [Ops]
allowedChars = [LoopL, LoopR]

pruneCode :: [Ops] -> [Ops]
pruneCode = filter (`elem` allowedChars) 

-- | Remove all non runnable characters
filterCode :: [Char] -> [Char]
filterCode = filter (`elem` "<>+-[],.")

newtype BFSource = BFSource [Ops] 

bfSourceDecomp :: BFSource -> [Ops]
bfSourceDecomp (BFSource ops) = ops

instance Show BFSource where
    show (BFSource sc) = concat $ show <$> sc

matchingParens :: Map Ops Ops
matchingParens =
  Map.fromList
    [ 
      (LoopL, LoopR)
    ]

isOpening :: Ops -> Bool
isOpening c = Data.Maybe.isJust $ Map.lookup c matchingParens


balanced :: [Ops] -> Bool
balanced inp = balanced' [] (pruneCode inp)
  where
    balanced' :: [Ops] -> [Ops] -> Bool
    balanced' [] [] = True
    balanced' _ [] = False
    balanced' [] (c : cs) = balanced' [c] cs
    balanced' (o : os) (c : cs)
      | isOpening c = balanced' (c : o : os) cs
      | otherwise = case Map.lookup o matchingParens of
        Nothing -> False
        Just closing -> (closing == c) && balanced' os cs


data Ops = ShiftRight   -- >
         | ShiftLeft    -- <
         | Increment    -- +
         | Decrement    -- -
         | LoopL        -- [
         | LoopR        -- ]
         | Read         -- ,
         | Print        -- .
    deriving (Ord , Eq)


instance Show Ops where
    show ShiftLeft = "<"
    show ShiftRight = ">"
    show Increment = "+"
    show Decrement = "-"
    show LoopL = "["
    show LoopR = "]"
    show Read = ","
    show Print = "."
    

data OptimisedOps   = OShiftRight Int 
                    | OShiftLeft Int
                    | OIncrement Int
                    | ODecrement Int
                    | OLoopL
                    | OLoopR
                    | ORead Int 
                    | OPrint Int

runBf :: BFSource -> IO ()
runBf = eval emptyTape . bfSourceToTape
  where
    bfSourceToTape :: BFSource -> Tape Ops
    bfSourceToTape (BFSource (b : bs)) = Tape [] b bs

runBrainFuckFromFile :: String -> IO ()
runBrainFuckFromFile f = do
    file <- readFile f
    let parsed = parseBrainfuck file
    
    case parsed of 
        Right src -> print src >> runBf src
        Left err -> putStrLn err


parseBrainfuck :: String -> Either String BFSource
parseBrainfuck src
    | balanced parsed = Right $ BFSource parsed
    | otherwise = error "mismatched parenthesis" 
        where
            charToOps :: Char -> Ops
            charToOps '<' = ShiftLeft 
            charToOps '>' = ShiftRight
            charToOps '+' = Increment 
            charToOps '-' = Decrement 
            charToOps '[' = LoopL
            charToOps ']' = LoopR
            charToOps ',' = Read
            charToOps '.' = Print

            parsed :: [Ops]
            parsed = map charToOps $ filterCode src
    
data Tape a = Tape [a] -- left pivot element
                    a  -- Pivot element
                   [a] -- Right pivot element

pivotElem :: Tape a -> a
pivotElem (Tape _ p _) = p 

instance Functor Tape where
    fmap f (Tape rs a ls) = Tape (map f rs) (f a) (map f ls)


-- | Initialise empty infinite tape
emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
    where zeros = repeat 0

shiftRight :: Tape a -> Tape a
shiftRight (Tape ls c (r:rs)) = Tape (c:ls) r rs

shiftLeft :: Tape a -> Tape a
shiftLeft (Tape (l:ls) c rs) = Tape ls l (c:rs)


eval :: Tape Int -> Tape Ops -> IO()
eval dataTape@(Tape l p r) instructionTape@(Tape _ instruction _) = 
    case instruction of 
        ShiftRight  -> nextInstruction (shiftRight dataTape) instructionTape 
        ShiftLeft   -> nextInstruction (shiftLeft dataTape) instructionTape 
        Increment   -> nextInstruction (Tape l (p+1) r) instructionTape 
        Decrement   -> nextInstruction (Tape l (p-1) r) instructionTape
        -- hflush necessary so that eval prints during run time.
        Print       -> putChar (chr $ pivotElem dataTape) >> hFlush stdout >> skip
        Read        -> do
            c <- getChar 
            nextInstruction (Tape l (ord c) r) instructionTape
        LoopL   | p == 0 -> seekLoopR 0 dataTape instructionTape
                | otherwise -> skip
        LoopR   | p /= 0 -> seekLoopL 0 dataTape instructionTape
                | otherwise -> skip

    where
        skip = nextInstruction dataTape instructionTape

-- | Move the instruction pointer left until the matching '[' is found.
-- The first parameter retains the current bracket count so that nested 
-- brackets still function as expected.
seekLoopR ::
    Int -> -- Parenthesis balance
    Tape Int -> -- Data tape
    Tape Ops -> -- Instruction tape
    IO ()
seekLoopR 1 dataTape instructionTape@(Tape _ LoopR _)   = nextInstruction dataTape instructionTape
seekLoopR b dataTape instructionTape@(Tape _ LoopR _)   = seekLoopR (b-1) dataTape (shiftRight instructionTape)
seekLoopR b dataTape instructionTape@(Tape _ LoopL _)   = seekLoopR (b+1) dataTape (shiftRight instructionTape)  
seekLoopR b dataTape instructionTape                    = seekLoopR b dataTape (shiftRight instructionTape)

seekLoopL ::
  Int -> -- Parenthesis balance
  Tape Int -> -- Data tape
  Tape Ops -> -- Instruction tape
  IO ()
seekLoopL 1 dataTape instructionTape@(Tape _ LoopL _)   = nextInstruction dataTape instructionTape
seekLoopL b dataTape instructionTape@(Tape _ LoopL _)   = seekLoopL (b - 1) dataTape (shiftLeft instructionTape)
seekLoopL b dataTape instructionTape@(Tape _ LoopR _)   = seekLoopL (b + 1) dataTape (shiftLeft instructionTape)
seekLoopL b dataTape instructionTape                    = seekLoopL b dataTape (shiftLeft instructionTape)


nextInstruction :: Tape Int -> Tape Ops -> IO()
-- When there are no more instructions
nextInstruction dataTape (Tape _ _ []) = return ()
nextInstruction dataTape instructionTape = eval dataTape (shiftRight instructionTape) 
