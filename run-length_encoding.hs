-- Andrey Perunov
-- st81049

-- Variant 1
-- Subtask 1
-- Define a function that determines the “run-length encoding”
-- of a list: [1, 2, 2, 3, 2, 4]
-- is mapped to [(1, 1),(2, 2),(1, 3),(1, 2),(1, 4)].
-- That is, the list is mapped to a list of pairs whose first element says how many times the second component of the pair appears in adjacent positions in the list.

-- Subtask 2
-- Define a function that groups successive duplicate elements in a list into sublists:
-- [1, 2, 2, 3, 2, 4] is mapped to [[1], [2, 2], [3], [2], [4]].


-- subtask 1
encode :: [Int] -> [(Int, Int)]
encode [] = []
encode (head:tail) = encode' tail head 1
  where
    encode' :: [Int] -> Int -> Int -> [(Int, Int)]
    -- if the list is empty, return the last element and its count
    encode' [] h count = [(count, h)]
    encode' (head:tail) h count
      -- if the next element is the same as the current one
      -- start counting
      | head == h = encode' tail h (count + 1)
      -- if the next element is different, add the current count and element to the list
      -- start counting next number
      | otherwise = (count, h) : encode' tail head 1

-- subtask 2
-- switched back to (x:xs) instead of (head:tail) because of head function
group :: [Int] -> [[Int]]
group [] = []
group (x:xs) = group' xs [x]
  where
    group' :: [Int] -> [Int] -> [[Int]]
    -- if the list is empty, return the last element
    group' [] h = [h]
    group' (x:xs) h
      -- if the next element is the same as the current one
      -- add it to the current sublist
      | x == head h = group' xs (x:h)
      -- if the next element is different, add the current sublist to the list
      -- start a new sublist with the next element
      | otherwise = h : group' xs [x]

main :: IO ()
main = do
  putStrLn "Enter a list of numbers: "
  input <- getLine
  let strings = words input
  let ints = [read s :: Int | s <- strings]
  let encoded = encode ints
  putStrLn "Subtask 1: "
  putStrLn (show encoded)
  let grouped = group ints
  putStrLn "Subtask 2: "
  putStrLn (show grouped)

