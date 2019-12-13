module Language.IntCode.Core where

-- import Data.Vector (Vector)
-- import qualified Data.Vector as Vec
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Lens

-- | Memory of the IntCode computer is represented as a Vector
-- | It is also the code of the computer.
type Memory = IntMap Integer


-- | An instruction's parameter
data Param = Im  Integer -- ^ Immediate
           | Ref Integer -- ^ Reference
           | Rel Integer -- ^ Relative
           deriving Show

-- | An IntCode:tm: instruction   
data IntCode = Hlt                          -- ^ Halts the computer
             | Add        Param Param Param -- ^ Adds two numbers
             | Mul        Param Param Param -- ^ Multiplies two numbers
             | Inp        Param             -- ^ Reads from input to a location
             | Out        Param             -- ^ Writes to output
             | Jnz        Param Param       -- ^ Jumps to address if value is true (non-zero)
             | Jez        Param Param       -- ^ Jumps to address if value is false (zero)
             | Less       Param Param Param -- ^ Performs (<) on two numbers
             | Equals     Param Param Param -- ^ Performs (==) on two numbers
             | AdjustBase Param             -- ^ Adjusts the base value
             deriving Show

digits :: Integer -> Int -> [Integer]
digits v n = map (`mod` 10) . reverse . take n . iterate (`div` 10) $ v

-- | Read an IntCode instruction from memory directly
readIntCode :: Integer       -- ^ Address to read at 
            -> Memory        -- ^ Memory
            -> Maybe IntCode -- ^ Resulting IntCode
readIntCode addr mem = do
    raw <- mem ^? ix (fromIntegral addr)
    let inst = raw `mod` 100      -- Opcode actual value
        digs = raw `digits` 5     -- Opcode digits starting from C's digit
        lda :: Integer -> Maybe Param
        lda offset = case digs !! (3 - fromIntegral offset) of 
            0 -> Ref <$> (mem ^? ix (fromIntegral $ addr + offset))
            1 -> Im  <$> (mem ^? ix (fromIntegral $ addr + offset))
            2 -> Rel <$> (mem ^? ix (fromIntegral $ addr + offset))
            _ -> Nothing
    case inst of
        1  -> Add        <$> (lda 1) <*> (lda 2) <*> (lda 3)
        2  -> Mul        <$> (lda 1) <*> (lda 2) <*> (lda 3)
        3  -> Inp        <$> (lda 1)
        4  -> Out        <$> (lda 1)
        5  -> Jnz        <$> (lda 1) <*> (lda 2)
        6  -> Jez        <$> (lda 1) <*> (lda 2)
        7  -> Less       <$> (lda 1) <*> (lda 2) <*> (lda 3)
        8  -> Equals     <$> (lda 1) <*> (lda 2) <*> (lda 3)
        9  -> AdjustBase <$> (lda 1)
        99 -> Just Hlt
        _  -> Nothing

-- | Read all    
readAllIntCodes :: Memory -> [IntCode]
readAllIntCodes memory = go 0
    where go ip = case readIntCode ip memory of
                Just opcode -> opcode : go (ip + steps opcode)
                Nothing     -> []

-- | The amount of Words an IntCode instruction takes up
steps :: IntCode -> Integer
steps Hlt            = 1
steps (Add _ _ _)    = 4
steps (Mul _ _ _)    = 4
steps (Inp _)        = 2
steps (Out _)        = 2
steps (Jnz _ _)      = 3
steps (Jez _ _)      = 3
steps (Less _ _ _)   = 4
steps (Equals _ _ _) = 4
steps (AdjustBase _) = 2