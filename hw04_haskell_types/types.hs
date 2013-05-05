{- Author: TODO: WRITE YOUR NAME HERE
 - Email:  TODO: WRITE YOUR EMAIL HERE
 -
 - CS / CE 4337 Spring 2013 Sections 001, 002
 -
 - Assignment 4:   Type Classes and Higher Order Functions
 - Assigned:       Monday, 2013-04-29
 - Due:            Wednesday, 2013-05-08, 23:59 (11:59pm) (NO LATE PERIOD!)
 - Estimated SLOC: 20
 -
 -
 - **DON'T FORGET TO FILL IN YOUR NAME AND EMAIL ABOVE!**
 -
 -
 - **WARNING!**
 -
 - Before submission, make sure to search the file for "TODO" to make sure you
 - have completed all tasks!
 -
 - **end WARNING!!!**
 -
 -
 - For each function, you **MUST** also supply the type signature!
 -
 -
 - Running Your Code
 - =================
 -
 - Please see the HW 01 file `intro.hs` for information about running your
 - code and using the test cases.
 -}

import Control.Exception
import Control.Monad
import Control.DeepSeq

import qualified Data.List as List

import Test.HUnit


-- Extra Unit Test Scaffolding
-- ===========================================================================


-- Tells Haskell that two exceptions are equal if their strings are equal.
instance Eq ErrorCall where
    x == y = (show x) == (show y)


-- | A HUnit assertion to test that a given call throws an exception.
--
--   Taken from 
--   http://stackoverflow.com/questions/6147435/is-there-an-assertexception-in-any-of-the-haskell-test-frameworks/6147930#6147930
--   and 
--   http://stackoverflow.com/questions/13350164/how-do-i-test-for-an-error-in-haskell
--
--   NOTE: This may not work if the exception is not thrown directly from the
--   test case expression. For example, if you are testing that a function `f`
--   throws an exception `E`, and `E` is actually thrown by `g` (a function
--   called by `f`), the exception may not be caught, causing a failing test.
--
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex) 


-- | An HUnit assertion to test that a given call throws an error.
--   
--   Based on `assertException`. However, this only handles calls to `error`,
--   not exceptions in general. The actual error message is discarded; the
--   only thing tested is that an error was thrown, not whether a particular
--   error *message* was thrown. 
--
--   It appears to work better than `assertException` above, in that if the
--   error is thrown by a function called by the function under testing `f`,
--   the thrown error will still be detected and the test will pass. This is
--   done via the `force` function from the `Control.DeepSeq` module.
--
assertError :: ( NFData a, Show a ) => String -> a -> Assertion
assertError name f = 
    handle (\ (ErrorCall _) -> return () ) $ do
        res <- evaluate action
        assertFailure $ unlines [ name
                                , "expected: error message"
                                , " but got: " ++ show res
                                ]
    where 
        action  =  force f

    



{- Problem 0: 'collatzListGen' (A more general Collatz list)
 - ===========================================================================
 -
 - Recall 'collatzList' from HW 1. Load your 'intro.hs' file into GHCi and
 - type the following at the prompt:
 -
 -     collatzList (3 :: Int)
 -
 - You should get the following error message:
 -
 -     >>> collatzList (3 :: Int)
 -     <interactive>:7:14:
 -         Couldn't match expected type `Integer' with actual type `Int'
 -         In the first argument of `collatzList', namely `(3 :: Int)'
 -         In the expression: collatzList (3 :: Int)
 -         In an equation for `it': it = collatzList (3 :: Int)
 -
 - This is because we specified that the input and resulting list are both of
 - type 'Integer'. We seek to relax that restriction.
 -
 - Write a function 'collatzListGen' that behaves the same as 'collatzList',
 - except it takes an 'Integral' type as its input and returns a list whose
 - elements are of that same 'Integral' type.
 -
 - Remember that 'Integral' is a *type constraint*, not an actual type!
 -}

-- | A more general form of 'collatzList' --- takes a value 'n' of an
--   'Integral' type and maps it to the list of 'Integral'-typed values
--   corresponding to the Collatz sequence beginning at 'n'.
--

-- TODO: WRITE YOUR CODE HERE




-- Test Cases
-- ---------------------------------------------------------------------------

collatzListGen_test1 = 
    TestCase ( assertEqual 
                    "collatzListGen 1"
                    [1]
                    ( collatzListGen 1 )
             )

{- NOTE: Currently, these test cases will cause compiler errors due to my use
 - of the DeepSeq module. These will be enabled once I finally figure out a
 - workaround.
-} 
-- NOTE: When this test case is run, it may display that an error or failure
-- occurred. If it does, please let me know.
collatzListGen_test0 = 
    TestCase ( assertError
                    "collatzListGen 0"
                    ( collatzListGen ( 0 :: Int ) )
             ) 

-- NOTE: When this test case is run, it may display that an error or failure
-- occurred. If it does, please let me know.
collatzListGen_testNeg8 = 
    TestCase ( assertError
                    "collatzListGen ( -8 )" 
                    ( collatzListGen ( ( -8 ) :: Integer ) )
             )
{-
 -}

collatzListGen_test7 = 
    TestCase ( assertEqual
                    "collatzListGen 7"
                    [7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4,
                     2, 1]
                    ( collatzListGen 7 )
             )

collatzListGen_test51 = 
    TestCase ( assertEqual
                    "collatzListGen 51"
                    [51, 154, 77, 232, 116, 58, 29, 88, 44, 22, 11, 34, 17,
                     52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
                    ( collatzListGen 51 )
             )



collatzListGen_tests = 
    TestList [ collatzListGen_test1
             , collatzListGen_test0
             , collatzListGen_testNeg8
             , collatzListGen_test7
             , collatzListGen_test51 
             ]






-- | A binary 'Tree' data type. The type of the data stored in the tree is not
--   set here; instead, we use a type variable to represent the type of data
--   stored in the tree. 
--
--   Observe that the inductive / recursive structure of the tree is encoded
--   in the type definition. This should make it easy to pick the base and
--   recursive cases for a function operating on 'Tree's. Compare with lists,
--   which could be defined similarly.
data Tree a  =  Empty
             |  Node a (Tree a) (Tree a)
             deriving (Show, Eq)


-- | Show a 'Tree a' in a "pretty" format --- multi-line, indentation
--   indicating subtrees.
--
showPretty :: Show a => Tree a -> String
showPretty t =
    showPrettyHelper t 0


tabSize = 4

showPrettyHelper :: Show a => Tree a -> Int -> String
showPrettyHelper Empty x = 
    ( replicate ( x * tabSize ) ' ' ) ++ "Empty"

showPrettyHelper ( Node v tLeft tRight ) x =
    ( replicate ( x * tabSize ) ' ' ) ++ "(Node " ++ ( show v ) ++ "\n"
        ++ ( showPrettyHelper tLeft ( x + 1 ) ) ++ "\n"
        ++ ( showPrettyHelper tRight ( x + 1 ) ) ++ "\n"
        ++ ( replicate ( x * tabSize ) ' ' ) ++ ")"


-- | Pretty-print a 'Tree a' to the console; uses 'showPretty' above.
--
prettyPrintTree :: Show a => Tree a -> IO ()
prettyPrintTree = putStrLn . showPretty



-- | Insert a value into an ordered 'Tree'. If the 'Tree' already contains the
--   value, then return the same 'Tree' (i.e., don't insert duplicate values). 
--
--   This function lets us store a value of any type in the 'Tree', so long as
--   the type can be ordered and printed out. The ordering is necessary,
--   because we have to be able to compare the input with the values in the
--   'Tree'. 
--
--   Also, due to Haskell's type system, the values stored in the 'Tree' must
--   all be of the same type, similarly to how lists must contain values of
--   the same type.
--
--    *  Note that 'Ord a' implies 'Eq a' --- i.e., 'Ord a' is a subset of 
--       'Eq a'.
--
insertTree :: ( Ord a, Show a ) => Tree a -> a -> Tree a

-- If the 'Tree' is 'Empty', then the result is a 'Tree' with 'x' at the root
-- and two 'Empty's as children.
insertTree Empty x  =  Node x Empty Empty

-- Just like with lists, the only other option is a non-'Empty' 'Tree'.
-- Because we can compare on 'x' (due to the type constraints above), we have
-- three possibilities:
insertTree ( Node v tLeft tRight ) x
    -- 'x' and the root value are equal; return the same 'Tree', because we
    -- aren't permitting duplicates.
    | x == v = Node v tLeft tRight

    -- 'x' is less than the root, so insert it into the left tree.
    | x < v = Node v (insertTree tLeft x) tRight

    -- 'x' is greater than the root, so insert it into the right tree.
    | x > v = Node v tLeft (insertTree tRight x)



-- | Create a new 'Tree a' from an '[a]'.
--
--   Thank you functional programming and Currying. :)
--
createTree :: ( Ord a, Show a ) => [ a ] -> Tree a
createTree = foldl insertTree Empty



-- Some sample data

-- A tree containing integers
intTree = createTree [ 9, 7, 2, 8, 6, 0, 5, 3, 1 ]

-- A tree containing all permutations of a list of integers
listTree = createTree ( List.permutations [ 0 .. 3 ] )

-- A tree containing strings
strTree = createTree [ "hello"
                     , "world"
                     , "lorem"
                     , "ipsum"
                     , "dolor"
                     , "sit"
                     , "amet"
                     ]



{- Problem 1: memberTree
 - ============================================================================
 -
 - Write a function 'memberTree' that takes a value 'x' of type 'a' --- where
 - 'a' is an instance of 'Ord a' and 'Show a' --- and a 'Tree' of type 'Tree
 - a' and returns a 'Bool' indicating whether 'x' is located in the 'Tree'.
 - Because 'a' is an ordered type, the 'Tree' should be ordered as well (i.e.,
 - you may assume that the tree is ordered, as if all values were inserted
 - using 'insertTree' above).
 -}

-- | Check whether 'x' is a member of a 'Tree a'.

-- TODO: WRITE YOUR CODE HERE




-- Test Cases
-- ----------------------------------------------------------------------------


memberTree_testIntEmpty =
    TestCase ( assertEqual
                "memberTree 5 Empty"
                False
                ( memberTree 5 Empty )
             )

memberTree_testIntPresent =
    TestCase ( assertEqual
                "memberTree 5 intTree"
                True
                ( memberTree 5 intTree )
             )

memberTree_testIntNotPresent =
    TestCase ( assertEqual
                "memberTree (-10) intTree"
                False
                ( memberTree (-10) intTree )
             )

memberTree_testListEmpty =
    TestCase ( assertEqual
                "memberTree [ 1, 3, 2, 0 ] Empty"
                False
                ( memberTree [ 1, 3, 2, 0 ] Empty )
             )

memberTree_testListPresent =
    TestCase ( assertEqual
                "memberTree [ 1, 3, 2, 0 ] listTree"
                True
                ( memberTree [ 1, 3, 2, 0 ] listTree )
             )

memberTree_testListNotPresent =
    TestCase ( assertEqual
                "memberTree [ 4, 5, 6, 7 ] listTree"
                False
                ( memberTree [ 4, 5, 6, 7 ] listTree )
             )

memberTree_testStrEmpty =
    TestCase ( assertEqual
                "memberTree \"dolor\" Empty"
                False
                ( memberTree "dolor" Empty )
             )

memberTree_testStrPresent =
    TestCase ( assertEqual
                "memberTree \"dolor\" strTree"
                True
                ( memberTree "dolor" strTree )
             )

memberTree_testStrNotPresent =
    TestCase ( assertEqual
                "memberTree \"goodbye\" strTree"
                False
                ( memberTree "goodbye" strTree )
             )


memberTree_tests = 
    TestList [ memberTree_testIntEmpty
             , memberTree_testIntPresent
             , memberTree_testIntNotPresent
             , memberTree_testListEmpty
             , memberTree_testListPresent
             , memberTree_testListNotPresent
             , memberTree_testStrEmpty
             , memberTree_testStrPresent
             , memberTree_testStrNotPresent
             ]






-- | An implementation of 'zipWith', for inspiration for the next function and
--   for Problems 1 and 2.
--
myZipWith :: ( a -> b -> c ) -> [ a ] -> [ b ] -> [ c ]
myZipWith _ [] _   =  []

myZipWith _ _  []  =  []

myZipWith fn ( x : xs ) ( y : ys ) =
    ( fn x y ) : ( myZipWith fn xs ys )



-- | A 'zipWith'-like function for Trees. If either Tree is empty, then return
--   Empty
--
--   For inspiration for Problems 1 and 2.
--
zipWithTree :: ( Show a, Show b ) => 
                    ( a -> b -> c ) -> Tree a -> Tree b -> Tree c
zipWithTree _ Empty _      =  Empty

zipWithTree _ _     Empty  =  Empty

zipWithTree fn ( Node x t0Left t0Right ) ( Node y t1Left t1Right ) =
    Node ( fn x y ) 
        ( zipWithTree fn t0Left t1Left ) 
        ( zipWithTree fn t0Right t1Right )



-- | An implementation of 'map', for inspiration in Problem 1.
--
myMap :: ( a -> b ) -> [ a ] -> [ b ]
myMap _ []           =  []
myMap fn ( x : xs )  =  ( fn x ) : ( myMap fn xs )



{- Problem 2: mapTree
 - ===========================================================================
 -
 - Write a function 'mapTree' that takes a function 'fn' of type 'a -> b' and
 - a 'Tree a' named 't' and returns a 'Tree b' such that every node in the
 - output is the result of applying 'fn' to the corresponding node of 't'.
 -
 - Note that the only constraint on the types 'a' and 'b' is that they are
 - instances of the class 'Show'. We could require that they be ordered, but
 - that is not important for this function (we'd like to be able to operate on
 - arbitrary trees, not just ordered ones). 
 -
 - Furthermore, even if both 'a' and 'b' were instances of the 'Ord' class,
 - there's no guarantee that these types would have the same ordering or that
 - 'fn' would preserve that ordering --- for instance, what if we mapped an
 - ordered tree of 'Integer's through the negation function?
 -
 - Also, recall that the '->' operator is right-associative. This will force
 - you to use parentheses if you want to indicate a single value of an arrow
 - type.
 -
 - For inspiration, consider the 'myMap' function above.
 -}

-- | Map the nodes in a 'Tree a' named 't' through a function 'fn' to obtain a
--   new 'Tree b'.

-- TODO: WRITE YOUR CODE HERE




-- Test Cases
-- ----------------------------------------------------------------------------

mapTree_testEmpty =
    TestCase ( assertEqual
                "mapTree (\\x -> x * 2) Empty"
                Empty
                ( mapTree (\x -> x * 2) Empty )
             )

mapTree_testIntDouble =
    TestCase ( assertEqual
                "mapTree (\\x -> x * 2) intTree"
                (Node 18 
                    (Node 14 
                        (Node 4 
                            (Node 0 
                                Empty 
                                (Node 2 
                                    Empty 
                                    Empty
                                )
                            ) 
                            (Node 12 
                                (Node 10 
                                    (Node 6 
                                        Empty 
                                        Empty
                                    ) 
                                    Empty
                                ) 
                                Empty
                            )
                        ) 
                        (Node 16 
                            Empty 
                            Empty
                        )
                    ) 
                    Empty 
                )
                ( mapTree (\x -> x * 2) intTree )
             )

mapTree_testStringLength =
    TestCase ( assertEqual 
                "mapTree length strTree"
                (Node 5
                    (Node 5
                        (Node 4
                            Empty
                            Empty
                        )
                        Empty
                    )
                    (Node 5
                        (Node 5
                            (Node 5
                                Empty
                                Empty
                            )
                            (Node 3
                                Empty
                                Empty
                            )
                        )
                        Empty
                    )
                )
                ( mapTree length strTree )
            )
                                


-- | A small function for use in testing 'mapTree'. Can you work out what it's
--   doing?
--    *  'replicate :: Int -> a -> [a]' constructs a list of type 'a' by
--       repeating a given element the provided number of times. In this case,
--       it's going to construct a string of a's, b's, etc.
--
--    *  'flip :: (a -> b -> c) -> b -> a -> c' takes a function of two
--       arguments and flips the order of the arguments. This is necessary
--       because we know what character we want to replicate, but we don't
--       know how many times, so we have to make it so the second argument is
--       the number of repetitions.
--
--    *  'zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]' takes two lists and
--       applies the function to the two elements at each position. If one
--       list is longer than the other, the leftover elements are ignored.
--        *  For convenience, I wrote a 'zipTreeWith' function above.
--
--    *  'concat :: [[a]] -> [a] takes a list of lists and concatenates them
--       all together.
--
--    *  '(.) :: (b -> c) -> (a -> b) -> (a -> c)' is function composition.
--
buildString :: [ Int ] -> String
buildString = concat . zipWith ( flip replicate ) [ 'a' .. 'z' ]


mapTree_testListBuildString =
    TestCase ( assertEqual
                "mapTree buildString listTree"
                (Node "bccddd"
                    Empty
                    (Node "accddd"
                        (Node "bbcddd"
                            (Node "bcccdd"
                                Empty
                                Empty
                            )
                            (Node "bbbcdd"
                                (Node "bbcccd"
                                    Empty
                                    Empty
                                )
                                (Node "bbbccd"
                                    Empty
                                    Empty
                                )
                            )
                        )
                        (Node "aabddd"
                            (Node "abbddd"
                                (Node "acccdd"
                                    Empty
                                    Empty
                                )
                                (Node "aacddd"
                                    (Node "abbbcc"
                                        (Node "abbccc"
                                            Empty
                                            (Node "abbbdd"
                                                Empty
                                                Empty
                                            )
                                        )
                                        Empty
                                    )
                                    (Node "aacccd"
                                        Empty
                                        Empty
                                    )
                                )
                            )
                            (Node "aaabbc"
                                (Node "aabbbc"
                                    (Node "aabccc"
                                        Empty
                                        (Node "aabbbd"
                                            Empty
                                            Empty
                                        )
                                    )
                                    (Node "aaabcc"
                                        (Node "aaacdd"
                                            Empty
                                            (Node "aaabdd"
                                                (Node "aaaccd"
                                                    Empty
                                                    Empty
                                                )
                                                Empty
                                            )
                                        )
                                        (Node "aaabbd"
                                            Empty
                                            Empty
                                        )
                                    )
                                )
                                Empty
                            )
                        )
                    )
                )
                ( mapTree buildString listTree )
             )

mapTree_tests = 
    TestList [ mapTree_testEmpty
             , mapTree_testIntDouble
             , mapTree_testStringLength
             , mapTree_testListBuildString
             ]




-- | An implementation of 'foldl', for inspiration in Problem 2.
--
myFoldl :: ( a -> b -> a ) -> a -> [ b ] -> a

-- If the list is empty, then we're done --- return our "partial work" to this
-- point.
myFoldl _ x []   =   x

-- If the list is nonempty, then take the result of '( fn x y )' --- i.e.,
-- compute the work done up to and including 'y' --- and provide that to the
-- recursive call to 'myFoldl'.
myFoldl fn x ( y : ys )  =  myFoldl fn ( fn x y ) ys




{- Problem 3: foldInOrder
 - ============================================================================
 -
 - Write a function 'foldInOrder' that takes:
 -  *  A function 'fn' of type 'a -> b -> a'
 -  *  A value 'x' of type 'a'
 -  *  A 'Tree b' named 't'
 -
 - and returns a value of type 'a'. The return value is computed similarly to
 - that of 'foldl' (see 'myFoldl' above) by performing an in-order traversal
 - of the Tree, passing along the partial results via 'x'. 
 -
 - WARNING: This will take some pencil-and-paper thinking. It's one line, but
 - figuring out how to organize the steps will take some doing. The type
 - signature will help you here --- use it by seeing what type a certain value
 - should be and what you have available that will obtain that value.
 -
 - Recall that in-order traversal processes the left subtree first, then
 - processes the root node, then processes the right subtree.
 -}

-- | Fold the values stored in a 'Tree a' with a function 'fn' and an initial
--   value 'x'. Uses an in-order traversal of the tree.

-- TODO: WRITE YOUR CODE HERE





-- Test Cases
-- ----------------------------------------------------------------------------

foldInOrder_testEmpty =
    TestCase ( assertEqual
                "foldInOrder (+) 0 Empty"
                0
                ( foldInOrder (+) 0 Empty )
             )

foldInOrder_testInt =
    TestCase ( assertEqual
                "foldInOrder (+) 10 intTree"
                51
                ( foldInOrder (+) 10 intTree )
             )

foldInOrder_testList =
    TestCase ( assertEqual
                "foldInOrder (\\x y -> x ++ ( buildString y ) ) \"\" listTree"
                "bccdddbcccddbbcdddbbcccdbbbcddbbbccdaccdddacccddabbdddabbcccabbbddabbbccaacdddaacccdaabdddaabcccaabbbdaabbbcaaacddaaaccdaaabddaaabccaaabbdaaabbc"
                ( foldInOrder (\x y -> x ++ ( buildString y ) ) "" listTree )
             )

foldInOrder_testString =
    TestCase ( assertEqual
                "foldInOrder (++) \"goodbye\" strTree"
                "goodbyeametdolorhelloipsumloremsitworld"
                ( foldInOrder (++) "goodbye" strTree )
             )

foldInOrder_tests =
    TestList [ foldInOrder_testEmpty
             , foldInOrder_testInt
             , foldInOrder_testList
             , foldInOrder_testString
             ]



-- Main (just runs the unit tests)
-- ----------------------------------------------------------------------------

main = do
    putStrLn "Running 'collatzListGen' tests"
    runTestTT collatzListGen_tests
    putStrLn ""

    putStrLn "Running 'memberTree' tests"
    runTestTT memberTree_tests
    putStrLn ""

    putStrLn "Running 'mapTree' tests"
    runTestTT mapTree_tests
    putStrLn ""

    putStrLn "Running 'foldInOrder' tests"
    runTestTT foldInOrder_tests
    putStrLn ""


