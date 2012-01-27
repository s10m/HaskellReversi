import Text.Printf
import Data.Char

--Board has one constructor: a list of tuples of row-column co-ordinates and an accompanying character
data Board = Board [((Int,Int),Char)] deriving (Show)

--return all directions as tuples
north :: Int -> Int -> (Int,Int)
north x y = (x-1,y)

nEast :: Int -> Int -> (Int,Int)
nEast x y = (x-1,y+1)

east :: Int -> Int -> (Int,Int)
east x y = (x,y+1)

sEast :: Int -> Int -> (Int,Int)
sEast x y = (x+1,y+1)

south :: Int -> Int -> (Int,Int)
south x y = (x+1,y)

sWest :: Int -> Int -> (Int,Int)
sWest x y = (x+1,y-1)

west :: Int -> Int -> (Int,Int)
west x y = (x,y-1)

nWest :: Int -> Int -> (Int,Int)
nWest x y = (x-1,y-1)

--recursive get char at x y function
getChWith :: Int -> Int -> Board -> Char
getChWith _ _ (Board []) = '.'
getChWith x y (Board (((bx,by),c):bs)) = if ((x==bx) && (y==by)) then if c=='e' then '.' else c else getChWith x y $ Board bs

--set char at pos x y to chnew using a simple map
setChAt :: Int -> Int -> Board -> Char -> Board
setChAt x y (Board b) chnew = Board $ map (\((xi,yi), c) -> if (xi==x && yi==y) then ((xi,yi),chnew) else ((xi,yi),c)) b

--fire off to return an n x n board of 'e's
populate :: Int -> Board
populate n = Board $ pr n n

--get n rows recursively
pr :: Int -> Int -> [((Int,Int),Char)]
pr 1 n = fillY 1 n
pr x n = pr (x-1) n ++ fillY x n

--fill a row
fillY :: Int -> Int -> [((Int,Int),Char)]
fillY x 1 = [((x,1),'e')]
fillY x y = (fillY x $ y - 1) ++ [((x,y),'e')]

--recursively return a row as string
getAllChsForX :: Int -> Int -> Board -> String
getAllChsForX x 1 b = [getChWith x 1 b]
getAllChsForX x y b = getAllChsForX x (y-1) b ++ " " ++ [getChWith x y b]

--get top row for printing board recursively
getNumsStr :: Int -> String
getNumsStr 1 = "   1"
getNumsStr n = getNumsStr (n-1) ++ printf "%2d" n

--recursively convert a board to string representation
getAllChs :: Int -> Int -> Board -> String
getAllChs 1 n b = getNumsStr n ++ "\n" ++ " 1 " ++ getAllChsForX 1 n b
getAllChs x n b = getAllChs (x-1) n b ++ "\n" ++ printf "%2d " x ++ getAllChsForX x n b

--convert board to string
boardToStr :: Board -> String
boardToStr (Board a) = 	let n = floor . sqrt . fromIntegral $ length a
			in getAllChs n n (Board a) ++ "\n"
--main gets dimension and initialises board
main = 	do 	putStrLn "Enter dimension for square board (ideally a square number): "
		n <- getLine		
		playRev False $ initialise $ populate $ read n

--sets up the middle 4 squares in a board
initialise :: Board -> Board
initialise b = setChMid 0 0 'B' $ setChMid 0 1 'W' $ setChMid 1 0 'W' $ setChMid 1 1 'B' b

--set ox oy as offset from middle to c
setChMid :: Int -> Int -> Char -> Board -> Board
setChMid ox oy c b = setChAt (ox+(round $ fromIntegral (getn b) / 2)) (oy+(round $ fromIntegral (getn b) / 2)) b c

--print board to screen
printOutB :: Board -> IO ()
printOutB b = putStrLn $ boardToStr b

--get character used by player
getplayerChar :: Bool -> Char
getplayerChar b = 	if b then 'B'
			else 'W'

--tests if anyone can go at x
canGoAt :: ((Int,Int),Char) -> Board -> Bool
canGoAt x b = canGoWith 'B' x b || canGoWith 'W' x b

--tests if player (Char) p can go by seeing if they can go anywhere with a fold over the board list
playerCanGo :: Board -> Char -> Bool
playerCanGo (Board b) p = foldr (\x acc-> if acc==True then True else if canGoWith p x (Board b) then True else False) False b

--tests if a player c can go at x
canGoWith :: Char -> ((Int,Int),Char) -> Board -> Bool
canGoWith c ((r,col),char) b = isValidMove b c r col

--check all 8 compass directions to see if moves can be made which affect any directions
isValidMove :: Board -> Char -> Int -> Int -> Bool
isValidMove b c x y = foldr (\dir acc -> if (not acc) then isValidMoveDir b c (x,y) dir True else acc) False ["N","NE","E","SE","S","SW","W","NW"]

--simpler get char at function, taking a tuple instead of 2 ints
getCharAt' :: Board -> (Int,Int) -> Char
getCharAt' (Board bd) (x,y) = snd $ head [((a,b),c) | ((a,b),c) <- bd, a==x, b==y]

--similiar strategy to getCharAt' but for setting characters at (r,c)
setCharAt' :: (Int,Int) -> Char -> Board -> Board
setCharAt' (r,c) chto (Board bd)  = Board $ map (\((a,b),ch) -> if (a==r)&&(b==c) then ((a,b),chto) else ((a,b),ch)) bd

--given a board, computes square dimension
getn :: Board -> Int
getn (Board b) = round $ sqrt $ fromIntegral $ length b

--mapping directions to their respective tuple-returning functions
next :: String -> Int -> Int -> (Int,Int)
next "N" r c = north r c
next "NE" r c= nEast r c
next "E" r c = east r c
next "SE" r c= sEast r c
next "S" r c = south r c
next "SW" r c= sWest r c
next "W" r c = west r c
next "NW" r c= nWest r c

--determins if a tuple is on the board or not
isin :: Int -> (Int,Int) -> Bool
isin n (r,c) = if (c>n) || (c<1) || (r>n) || (r<1) then False else True

--given a direction, recursively checks if a move can be made which joins to another stone of player p, thus is valid
isValidMoveDir :: Board -> Char -> (Int,Int) -> String -> Bool -> Bool
isValidMoveDir b p (r,c) d first = 	if first then
						if isin (getn b) $ next d r c then
							if (getCharAt' b (next d r c)) == p || (getCharAt' b (next d r c)) == 'e' || (getCharAt' b (r,c)) /= 'e' then False
							else isValidMoveDir b p (next d r c) d False
						else False
					else if (getCharAt' b (r,c)) == p then True
					else if isin (getn b) $ next d r c then
						if (getCharAt' b (next d r c)) == 'e' then False
						else isValidMoveDir b p (next d r c) d False
					else False

--flips stones from a point at which a valid move has been taken
flipStones :: Int -> Int -> Char -> Board -> Board
flipStones r c ch b = foldr (setChsInLn r c ch) b $ map (\(d,yn)-> (d,numToFlip r c d ch yn b)) $ getDirs b ch r c

--computes the number of opposing team colours there are between 2 of those of a given team
numToFlip :: Int -> Int -> String -> Char -> Bool -> Board -> Int
numToFlip r c d ch yn b = 	if (not yn) then 0
				else if (getCharAt' b (next d r c)) == ch then 0
				else 1 + numToFlip (fst (next d r c)) (snd (next d r c)) d ch yn b

--do the actual flipping of n stones
setChsInLn :: Int -> Int -> Char -> (String,Int) -> Board -> Board
setChsInLn r c ch (dir,n) b = 	let (rown,coln)=next dir r c in
				if n==0 then b
				else if n==1 then
					setCharAt' (rown,coln) ch b
				else setCharAt' (rown,coln) ch $ setChsInLn rown coln ch (dir,(n-1)) b

--helper for flipStones
getDirs :: Board -> Char -> Int -> Int -> [(String,Bool)]
getDirs b ch r c = map (\y -> if isValidMoveDir b ch (r,c) y True then (y,True) else (y,False)) ["N","NE","E","SE","S","SW","W","NW"]

isNumerical :: String -> Bool
isNumerical s = foldr (isNumerical') True s

isNumerical' :: Char -> Bool -> Bool
isNumerical' c b = 	if (not b) then False
			else if isDigit c then True
			else False

--gets row number from user
getX :: Board -> IO String
getX b = 	do
		x <- getLine
		if isNumerical x then return x
		else
			do
			putStrLn "please enter a number"
			getX b

--gets column number from user
getY :: Board -> IO String
getY b = do
		y <- getLine
		if isNumerical y then return y
		else
			do
			putStrLn "please enter a number"
			getY b

--computes scores of both players as 2-tuple
getscores :: Board -> (Int,Int)
getscores (Board b) = foldr (\((_,_),ch) (bl,w) -> if ch=='B' then (bl+1,w) else if ch=='W' then (bl,w+1) else (bl,w)) (0,0) b

--computes scores for a given colour c
getscoresChar :: Board -> Char -> Int
getscoresChar (Board b) c = foldr (\((rws,cols),ch) out -> if ch==c then out+calcscore rws cols (getn (Board b)) else out) 0 b

--weights towards corners
calcscore :: Int -> Int -> Int -> Int
calcscore r c n = if (r==n&&c==n)||(r==1&&c==n)||(r==n&&c==1)||(r==1&&c==1) then 2 else 1

--get who has biggest score as string
printWinner :: Board -> String
printWinner b = let (bl,w) = getscores b in
			if bl > w then "black. Score was (black-white): " ++ show bl ++ "-" ++ show w
			else if bl==w then "draw. Score was (black-white): " ++ show bl ++ "-" ++ show w
			else "white. Score was (white-black): " ++ show w ++ "-" ++ show bl

--computes the maximum of the given moves
maxofmoves :: Board -> Char -> ((Int,Int),Char) -> (Int,(Int,Int)) -> (Int,(Int,Int))
maxofmoves b p ((r,c),_) (mx,(rold,cold)) = 	let max=minimax (domove r c p b) p 4 in
						if max > mx then (max,(r,c))
						else (mx,(rold,cold))

--do all moves and return play with best score
minimax' :: Board -> Char -> (Int,Int)
minimax' b p = snd $ foldr (maxofmoves b p) (-9999999,(0,0)) $ validmoves b p

--construct minmax tree and return the maximum value, following the Negamax algorithm on wikipedia as closely as possible
minimax :: Board -> Char -> Int -> Int
minimax b p d = if d<=0 || boardisdone b then hstc b p
		else fst $ foldr (compmax) (0-9999999,(0,0)) $ map (\((r,c),ch) -> (0-minimax (domove r c ch b) (notplayer p) (d-1),(r,c))) $ validmoves b p

--returns a list of currently valid moves
validmoves :: Board -> Char -> [((Int,Int),Char)]
validmoves (Board b) p = foldr (\((r,c),ch) mvs -> if isValidMove (Board b) p r c then [((r,c),ch)]++mvs else mvs) [] b

--"negate" the player char
notplayer :: Char -> Char
notplayer c = if c=='B' then 'W' else 'B'

--compute the maximum of the given plays
compmax :: (Int,(Int,Int)) -> (Int,(Int,Int)) -> (Int,(Int,Int))
compmax (mnew,nc) (mold,oc) = 	if mnew>mold then (mnew,nc)
				else (mold,oc)

--compute base case
boardisdone :: Board -> Bool
boardisdone b = not (playerCanGo b 'B' || playerCanGo b 'W')

--return simple heuristic for a board
hstc :: Board -> Char -> Int
hstc b p = 	let (bl,wt)=getscores b in
		if p=='B' then bl--(bl-wt)
		else wt--(wt-bl)

--return a board with the given (valid) move made
domove :: Int -> Int -> Char -> Board -> Board
domove r c p b = setCharAt' (r,c) p $ flipStones r c p b

--put an 'X' where a player can go
showmoves :: Char -> Board -> Board
showmoves p (Board b) = Board $ map (\((r,c),ch) -> if isValidMove (Board b) p r c then ((r,c),'?') else ((r,c),ch)) b

--main "loop", top level of user input/output etc. 
playRev :: Bool -> Board -> IO ()
playRev isComp b =	do
				printOutB $ showmoves (getplayerChar isComp) b
				if (not isComp) then
					if playerCanGo b (getplayerChar isComp) then
						do
							putStrLn $ "Player " ++ [getplayerChar isComp] ++ "'s go"
							putStrLn "Enter row number:"
							x <- getX b
							putStrLn "Enter column number:"
							y <- getY b
							if isValidMove b (getplayerChar isComp) (read x) (read y) then
								playRev (not isComp) $ domove (read x) (read y) (getplayerChar isComp) b
								else
								do	putStrLn "invalid move."
									playRev isComp b
						else if playerCanGo b (getplayerChar (not isComp)) then playRev (not isComp) b
						else putStrLn $ "Winner: " ++ printWinner b
					else
						if playerCanGo b (getplayerChar isComp) then
							let mv= minimax' b (getplayerChar isComp) in
								playRev (not isComp) $domove (fst mv) (snd mv) (getplayerChar isComp) b
							else if playerCanGo b (getplayerChar (not isComp)) then playRev (not isComp) b
							else putStrLn $ "Winner: " ++ printWinner b
