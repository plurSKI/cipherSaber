import System.Environment (getArgs)
import Random
import Char 
import Array
import Data.Word
import Data.Bits
import Data.ByteString.Internal

-- Handle I/O
handleArgs function operation key inputFile outputFile = do
	message <- readFile inputFile
	writeFile outputFile (function operation message key)

main = mainWith doCypher
	where mainWith function = do
		args <- getArgs
		case args of
			[operation, key, input, output] ->  handleArgs function operation key input output
			_ -> putStrLn "Usage: cipherSaber operation key inputFile outputFile"

-- Set up the key and message and call mix and crypt
doCypher "d" msg key = crypt 0 1 0 (mix 0 0 [0..255] (key ++ (take 10 msg))) (drop 10 msg) []
doCypher "e" msg key = let salt = "A#$hZVv46~"
                       in salt ++ (crypt 0 1 0 (mix 0 0 [0..255] (key ++ salt)) (msg) [])
doCypher _ msg key = "Invalid Operation Specified: Use either d for decrypt or e for encrypt"


-- Mix the state vector with the key
mix 256 j state key = state
mix i j state key = let j' = (j + (state !! i) +  (ord (key !! (i `mod` (length key))))) `mod` 256
                    in mix (i+1) j' (swap i j' state) key

-- Apply the state vector to the message

crypt byte i j state msg xorArray 
  | byte == length msg = map w2c (zipWith xor (map c2w ( map chr xorArray)) (map c2w msg))
  | byte /= length msg = let j' = ( j + (state !! i) ) `mod` 256
                             n = (( state !! i ) + (state !! j')) `mod` 256
                         in crypt (byte + 1) ((i+1) `mod` 256) j' (swap i j' state) msg 
			          (xorArray ++ [ (swap i j' state) !! n])

-- Array element swapping and indexing
swap i j x   | i == j = x
             | i < j  = realSwap i j x 
	     | i > j  = realSwap j i x
realSwap i j x = ((take i x) ++ [x !! j]   ++ 
		  (take (j - i - 1) (drop ( (i + 1) `mod` 256) x)) ++ [x !! i] ++ 
		  (drop ( (j + 1) `mod` 256) x))
