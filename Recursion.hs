-- PLEASE READ : wlecome! Basic vocab: "char" = "character", "str" = "string", etc.


-- takes a char representing a num between zero and nine, returning corresponding int
char_to_int :: Char -> Integer             -- Function 1
char_to_int c
    | c == '0' = 0
    | c == '1' = 1
    | c == '2' = 2
    | c == '3' = 3
    | c == '4' = 4
    | c == '5' = 5
    | c == '6' = 6
    | c == '7' = 7
    | c == '8' = 8
    | c == '9' = 9
    | otherwise = (-1)


-- returns a str that contains n copies of the char c
repeat_char :: Char -> Integer -> String      -- Function 2 
repeat_char c 0 = []
repeat_char c n = 
    if n > 0 then c : repeat_char c (n-1)
        else []
        
        
-- decodes str in a simple decoding manner (e.g., "a1b2" returns "abb")
decode :: String -> String                    -- Function 3 
decode [] = []
decode [z] = error ("sorry, can not decode") -- if only 1 item remains in the list in the end, error
decode (x:y:xs) = repeat_char x (char_to_int y) ++ decode xs
    

-- takes num between 0 and 9, returning corresponding char
int_to_char :: Integer -> Char                 -- Function 4
int_to_char c  
    | c == 0 = '0'  
    | c == 1 = '1'
    | c == 2 = '2'
    | c == 3 = '3'
    | c == 4 = '4'
    | c == 5 = '5'
    | c == 6 = '6'
    | c == 7 = '7'
    | c == 8 = '8'
    | c == 9 = '9'
    
    
-- takes a char c and a str, returning the num of times that c occurs at the start of str (e.g., length_char 'a' "aaaabaa" should return 4)
length_char :: Char -> String -> Integer      -- Function 5
length_char c [] = 0  
length_char c (x:xs) = 
    if x == c
        then 1 +  length_char c xs 
    else 0


-- takes char c and a str, returning a version of str without any leading instances of c. (e.g., drop_char 'a' "aaabaa" should return "baa")
drop_char :: Char -> String -> String          -- Function 6
drop_char c [] = []
drop_char c (x:xs)= 
    if c == x 
        then drop_char c xs
    else (x:xs)
    
    
-- encodes str in simple encoding manner (encode "aaabbbccc" should return "a3b3c3")
encode :: String -> String                      -- Function 7
encode [] = [] 
encode (x:xs) = complex_encode (x:xs)
    
    
-- returns non-simple repeat encoding of the str (complex_encode "hello" should return "hel2o")
complex_encode :: String -> String              -- Function 8
complex_encode [] = []
complex_encode (x:xs)  -- works for all numbers (below and above 9)
    | length_char x xs == 0 = x : complex_encode xs -- if only one letter, ignore (dont put 1 next to it)
    | otherwise = x : complex_IntToChar (length_char x (x:xs)) ++ complex_encode(drop_char x (xs))


complex_IntToChar :: Integer -> String  -- Supporter function for complex_encode
complex_IntToChar 0 = []
complex_IntToChar n = complex_IntToChar (n `div` 10) ++ [int_to_char (n `mod` 10)]    
    
    
-- takes non-simple repeated encoded str, and returns original un-encoded str 
complex_decode :: String -> String              -- Function 9 
complex_decode [] = []
complex_decode (x:y:xs) 
    | xs /= [] = repeat_char x (char_to_int y) ++ (complex_decode xs) -- Function doesnt work, need to come back to it
    | otherwise = repeat_char x (char_to_int y)                       -- the function stops decoding after first loop, saying that it is a non exhaustive pattern

     
-- complex_CharToInt :: Char -> Integer     -- supporter function for complex_decode 
-- complex_CharToInt (x:xs) = int_to_char x ++ complex_CharToInt xs


