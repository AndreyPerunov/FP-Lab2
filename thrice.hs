-- Andrey Perunov
-- st81049

-- Variant 1
-- Given the following definitions:
-- thrice a = [a, a, a]
-- sums (a : b : bs) = a : sums (a + b : bs) sums as	= as

-- What does the following expression evaluate to?
-- map thrice (sums [0 .. 4])
-- Answer: [[0,0,0],[1,1,1],[3,3,3],[6,6,6],[10,10,10]]
-- Explanation:
-- sums [0 .. 4]
-- 0 : sums [0 + 1, 2, 3, 4]
-- 0 : 1 : sums [1 + 2, 3, 4]
-- 0 : 1 : 3 : sums [3 + 3, 4]
-- 0 : 1 : 3 : 6 : sums [6 + 4]
-- 0 : 1 : 3 : 6 : 10 : sums []
-- 0 : 1 : 3 : 6 : 10 : []
-- [0, 1, 3, 6, 10]
-- map thrice [0, 1, 3, 6, 10]
-- [thrice 0, thrice 1, thrice 3, thrice 6, thrice 10]
-- [[0, 0, 0], [1, 1, 1], [3, 3, 3], [6, 6, 6], [10, 10, 10]]

thrice a = [a, a, a]

sums (a : b : bs) = a : sums (a + b : bs)
sums as = as

main = print $ map thrice (sums [0 .. 4])