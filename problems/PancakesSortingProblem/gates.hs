import Data.List
import Debug.Trace
import Functions
import Data.Function (on)
import qualified Data.Set as Set

is_an_adjacency :: Int -> Int -> Int -> Bool
is_an_adjacency a b l | (abs(a-b) == 1) || (a==1 && b==1) = True
                        | otherwise = False

free :: Int -> [Int] -> Bool
free _ [] = False
free x l | (ind == ll) = not (is_an_adjacency a x ll)
            | (ind == 1) = not (is_an_adjacency x b ll)
            | (ind == 0) = False
            | otherwise = (not(is_an_adjacency a x ll) && not(is_an_adjacency x b ll))
                where a = return_nth' (ind-1) l
                        b = return nth' (ind+1) l
                        ind = indexOf x l 
                        ll = length l 

in_a_block :: Int -> [Int] -> Bool
in_a_block x l = not(free x l)

diff_blocks :: Int -> Int -> [Int] -> Bool
diff_blocks x y xs = abs(posx-posy) /= abs(x-y)
                     where posx = indexOf x xs -1
                            posy = indexOf y xs -1

frist_of_block :: Int -> [Int] -> Bool
frist_of_block x l | indexOf x l == 0 = False
                    | (prev == 0) = is_an_adjacency x next len
                    | otherwise = (not (is_an_adjacency prev x len) && is_an_adjacency x next len)
                    wehre prev = return_nth' ((indexOf x l) - 1) l
                            next = return_nth' ((indexOf x l) + 1) l
                            len = length(l)

last_of_block :: Int -> [Int] -> Bool
last_of_block x l | (next == 0) = is_an_adjacency prev x len
                    | otherwise = (is_an_adjacency prev x len && not(is_an_adjacency x next len))
                        where prev = return_nth' ((indexOf x l) - 1) l
                                next = return_nth' ((indexOf x l) + 1) l 
                                len = length(l)

end_of_block :: [Int] -> Int -> Int
end_of_block [] _ = -1
end_of_block [x] _ = x
end_of_block (x:y:xs) len | (is_an_adjacency x y len == False) = x
                            | otherwise = end_of_block (y:xs) len 

flipping :: ([Int,Int]) -> Int -> Int -> ([Int,Int])
flipping list posx posp = (fs,ps)
                            where ys = flip_stack posx xs
                                  posy = (indexOf x ys) -1
                                  zs = flip_stack posy ys
                                  posz = posp
                                  vs = flip_stack posz zs
                                  posv = (indexOf x vs) -1
                                  xs = fst list
                                  x = head xs
                                  path = snd list
                                  fs = flip_stack posv vs
                                  ps = path ++ [posy,posz,posv]

case_1a :: ([Int,Int]) -> ([Int,Int])
case_1a xs = (ys,p)
            where pos = (indexOf (x+1) list)-1
                    list = fst xs
                    path = snd xs
                    x = head list
                    p = path ++ [pos]
                    ys = flip_stack pos list

case_1b :: ([Int],[Int]) -> ([Int],[Int])
case_1b xs = (ys,p)
            where pos = (indexOf (x-1) list)-1
                    list = fst xs
                    path = snd xs
                    x = head list
                    p = path ++ [pos]
                    ys = flip_stack pos list

case_2a :: ([Int],[Int]) -> ([Int],[Int])
case_2a xs = (ys,p)
            where pos = (indexOf (x+1) list)-1
                    list = fst xs
                    path = snd xs
                    x = head list
                    p = path ++ [pos]
                    ys = flip_stack pos list

case_2b :: ([Int],[Int]) -> ([Int],[Int])
case_2b xs = (ys,p)
            where pos = (indexOf (x-1) list)-1
                    list = fst xs
                    path = snd xs
                    x = head list
                    p = path ++ [pos]
                    ys = flip_stack pos list

case_3a :: ([Int],[Int]) -> ([Int],[Int])
case_3a xs | posn < posp = flipping xs posn posp
            | otherwise = flipping xs posp posn
            where posn = indexOf (x+1) list
                    posp = indexOf (x-1) list
                    list = fst xs
                    path = snd xs
                    x = head list

case_3b :: ([Int],[Int]) -> ([Int],[Int])
case_3b list = (zs,p)
                where posn = indexOf (x+1) xs
                        ys = flip_stack posn xs
                        posx = (indexOf x ys) -1
                        zs = flip_stack posx ys
                        xs = fst list
                        path = snd list
                        x = head xs
                        p = path ++ [posn,posx]

case_3c :: ([Int],[Int]) -> ([Int],[Int])
case_3c list = (zs,p)
            where posn = indexOf (x-1) xs
                    ys = flip_stack posn xs
                    posx = (indexOf x ys) -1
                    zs = flip_stack posx ys
                    xs = fst list
                    path = snd list
                    x = head xs
                    x = head xs
                    p = path ++ [posn,posx]

case_4a :: ([Int],[Int]) -> ([Int],[Int])
case_4a xs = (ys,p)
            where pos = (indexOf (x+1) list)-1
                    list = fst xs
                    path = snd xs
                    x = head list
                    p = path ++ [pos]
                    ys = flip_stack pos list

case_4a :: ([Int],[Int]) -> ([Int],[Int])
case_4a xs = (ys,p)
            where pos = (indexOf (x+1) list)-1
                    list = fst xs
                    path = snd xs
                    x = head list
                    p = path ++ [pos]
                    ys = flip_stack pos list

case 5a :: ([Int],[Int]) -> ([Int],[Int])
case_5a xs = (ys,p)
            where pos = (indexOf (x+1) list)-1
                    list = fst xs 
