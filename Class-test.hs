import Data.List
import System.IO

-- This file is made up of 
--    SECTION A - 20 Mark
--    SECTION B - 20 Marks
--    SECTION C - 40 Marks
-- The other file (clas_test_fixing errors.hs) is worth 20 Marks.
-- Please put your name below (where indicated) and zip the files into a .zip file using the naming convention 
-- first letter of first name + last name (e.g. mmeagher.zip)
-- Note the Prelude.hs file should not be put into the folder with your code. This is for information purposes only. 


-- PLEASE FILL THIS IN
-- Student Name Emmanuel Sedicol (20072377)

--SECTION A - 20 Marks

-- For each named declaration below. Use the comment preceeding
-- it as a guide to creating a comprehension that computes the
-- same value as that displayed in the comment.
---For example

-- [1, 3, 5, 7, 9, 11]
x1 = [1,3 .. 11]

-- [300,400,500]
x2 = [x * 100 | x <- [3..5]]

-- [(1,1),(2,4),(3,9),(4,16), (5,25)]
x3 = [(n,n*n) | n <- [1..5]]

--x4:: fill in type
x4 :: a -> b -> (a,b)
x4 something somethingelse = (something, somethingelse)


-- write a function mysum that 
-- takes an integer (n) as a parameter and returns the sum 
-- of all the numbers or all the numbers from 1 to n
-- Hint: use the following scheme
-- e.g. mysum 4 = 10
-- The sum function will be useful
-- sum [1,4,6] ---> 1 + 4 + 6

mySum :: Int -> Int
mySum n = n + mySum(n - 1)


--SECTION B - 20 Marks
-- For each named expression replace "undefined"
-- with an expression with the same type as the declaration

j1:: (String,String, Int)
j1 = ("String1", "String2", 1)

j2:: [String]
j2 = ["Hello", "World"]

j3:: Char
j3 = 'r'

j4:: Double
j4 = 13.2

--- For the next section, write in a type definition with the associated declaration
--For example
jj:: [Integer]
jj = [1,2,3]

j5:: (Char, Bool, Int, Double)
j5 = ('x', True, 3, 4.0)

j6:: (String, [Int])
j6 = ("Hello", [2,3,4])

j7 :: [[Bool]]
j7 = [[True, False], []]

j8:: [(Int,Int,Int)]
j8 = [(3,4,5)]

j9:: Int  -> Int
j9  4 = 47

data School =  Computing | Business | Engineering | Humanities | Education
    deriving (Eq,Ord,Show)

students = 
  [("James",24,Computing,"Tipperary")
  ,("Mary",36,Business,"Waterford")
  ,("Jane",19,Engineering,"Cork")
  ,("Tom",41,Business,"Kilkenny")
  ,("Ann",9,Education,"Wexford")
  ,("Leo",50,Computing,"Tipperary")
  ,("Harry",71,Humanities,"Cork")  
  ,("Simon",80,Business,"Cork")
  ,("Donald",23,Computing,"Kilkenny")
  ,("Kate",32,Humanities,"Wexford")
  ,("Jack",50,Computing,"Tipperary")
  ,("Randall",15,Engineering,"Cork")
  ,("Beth",24,Computing,"Tipperary")
  ,("Toby",50,Humanities,"Tipperary")
  ,("Kevin",50,Education,"Tipperary")
  ,("Nicky",29,Computing,"Wexford")
  ]

  
name   (nm,ag,sh,cy)  = nm  
age    (nm,ag,sh,cy)  = ag
school (nm,ag,sh,cy)  = sh
county (nm,ag,sh,cy)  = cy 

------------------------------------
-- the names of all students who live in Wexford
p1 =[name student | student <-students, county student=="Wexford"]

-----------------------------------

-- How many students live in Tipperary
p2 = length [name student | student <-students, county student=="Tipperary"]

-------------------------------------------------
-- the list of students (with their ages) of students who live in Kilkenny
p3 = [(name student, age student)| student <- students, county student=="Kilkenny"]

-------------------------------------------------
-- list students (names only) in aplphabetical order who liven in Tipperary
p4 = sort [name student| student <-students, county student=="Tipperary"]

-------------------------------------------------
-- list of students, grouped by school 
p5 = group ( sortBy f students)  
     where 
     f x y = (school x ) `compare` (school y)


