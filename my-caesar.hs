import Data.Char

cypher :: String -> Int -> String
cypher string rotation = map (cypher_char rotation) string

lower_bound_lc = fromEnum 'a'
upper_bound_lc = fromEnum 'z'

lower_bound_uc = fromEnum 'A'
upper_bound_uc = fromEnum 'Z'

cypher_char :: Int -> Char -> Char
cypher_char rotation c = if should_cypher c then cypher_char' rotation c else c
    where should_cypher c = isLetter(c) && isAscii(c)

cypher_char' :: Int -> Char -> Char
cypher_char' rotation c
  | isLower c = toEnum $ cypher_int val rotation lower_bound_lc upper_bound_lc
  | isUpper c = toEnum $ cypher_int val rotation lower_bound_uc upper_bound_uc
  | otherwise = c
 where val = fromEnum c

cypher_int :: Int -> Int -> Int -> Int -> Int
cypher_int val rotation = wrap (val + rotation)

wrap :: Int -> Int -> Int -> Int
wrap val lower upper = (adjusted_val `mod` range) + lower
    where range = upper - lower + 1
          adjusted_val = val - lower

decypher :: String -> Int -> String
decypher text i = cypher text (-i)

