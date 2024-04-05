-- Andrey Perunov
-- st81049

-- Variant 1
-- Implement a function.
  -- Requirement: Function, for some k, m, and n, determines the number of solutions of the equation kx² + m(x –n) = 0, using case distinction.
-- Answer the questions.
  -- What is the type of the following functions?x
  -- tail, sqrt, pi, exp, (ˆ), (/=) and function you created? 
    -- tail :: [a] -> [a]
    -- sqrt :: Floating a => a -> a
    -- pi :: Floating a => a
    -- exp :: Floating a => a -> a
    -- (ˆ) :: (Num a, Integral b) => a -> b -> a
    -- (/=) :: Eq a => a -> a -> Bool
    -- solutions :: Int -> Int -> Int -> Int
    -- discriminant :: Int -> Int -> Int -> Int
  -- How can you query the interpreter for the type of an expression and how can you explicitly specify the types of functions in your program?
    -- running ghci and typing: ":t *your expression here*"
    -- *function_name* :: *input type* -> *return type*
    -- *input type* can be repeated for multiple arguments 

-- Determines the number of solutions of the equation kx² + m(x –n) = 0
solutions :: Int -> Int -> Int -> Int
solutions k m n -- kx² + m(x – n) = 0
-- kx² + m(x – n) = 0 == kx² + mx - mn) = 0
-- a == k, b == m, c == -mn
  | discriminant k m (m*(-n)) > 0 = 2 -- D > 0 - two real roots
  | discriminant k m (m*(-n)) == 0 = 1 -- D = 0 - one real root
  | discriminant k m (m*(-n)) < 0 = 0 -- D < 0 - no real roots
  | otherwise = -1 -- error

-- ax² + bx + c = 0
-- D = b² - 4ac
discriminant :: Int -> Int -> Int -> Int
discriminant a b c = b^2 - 4 * a * c

main :: IO ()
main = do
  putStrLn "kx^2 + m(x - n) = 0"
  putStrLn "Enter k: "
  kString <- getLine
  let k = read kString :: Int
  putStrLn "Enter m: "
  mString <- getLine
  let m = read mString :: Int
  putStrLn "Enter n: "
  nString <- getLine
  let n = read nString :: Int
  putStrLn "Number of solutions: "
  putStrLn (show (solutions k m n))