-- For full instructions see the other file (class_test.hs)
-- PLEASE FILL THIS IN
-- Student Name : Emmanuel Sedicol (20072377)



-- Fix the error in the file below. Load the file, read the error mesage
-- and fix ONE ERROR at a time. Then load the file and read the next error
-- message. 
-- Note if either a change in the type of a function / value or  the value/definition will allow 
-- the code to compile, you can change either to make it error-free. 

a1:: Bool
a1 = True                   

a2 :: Int
a2 = head [5,6]   

a3 :: Integer
a3 = 99  

a4 :: Int
a4 = if 1 == 2 then 4 else 5                    

a5 :: Double
a5 = 1.0

a6 :: Int -> Int
a6 x = x `div` 2

-- a7 :: Num a => a -> Double
-- a7 = a9 a5

a8 :: Bool
a8 =  a6 8 == 3 && True   

a9 :: Num a => a
a9 = 10

-- a10 :: (==)
-- a10 == 23