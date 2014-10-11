{-
    This program will take even length strings and swap every set of two letters.
    That is, "abcd" becomes "badc" and "HelloWorld" becomes "eHllWorodl".

    Not only does it do that - it does it tail-recursively!
-}

{-
    Where the magic happens.

    The accumulator will have the form ([x, w] ++ ([z, y] ++ [])) where the
    original string was [w, x, y, z].
-}
permute' :: String -> (String -> String) -> String
permute' "" acc = acc []
permute' (x:(y:ys)) acc = permute' ys (\rest -> acc([y, x] ++ rest))

-- Provide a nice way to call permute'.
permute :: String -> String
permute xs = permute' xs id

-- Main program - calls permute for every line of input provided on stdin.
main :: IO ()
main = do
    io (map permute)

-- Read from stdin, process, write to stdout.
-- This is the single coolest line of code I have ever typed.
io f = interact (unlines . f . lines)
