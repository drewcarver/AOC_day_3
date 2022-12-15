import Data.Char

filterDups [list1, list2, list3] = filter(\a -> elem a list2 && elem a list3) list1
filterDups _                     = ""

group _ [] = []
group n l
  | n > 0     = (take n l) : (group n (drop n l))
  | otherwise = error "Can't group by this number"

sumBadges = sum . map (getPriority . head . filterDups) . group 3 . lines

getPriority item
  | item `elem` ['a'..'z'] = (ord item) - 96
  | item `elem` ['A'..'Z'] = (ord item) - 38

main = readFile "input.txt" >>= (print . sumBadges)
