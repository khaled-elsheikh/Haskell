data Term = Constant String | Var String  deriving (Eq,Show)
data Predict = Pre String [Term]  deriving (Eq,Show)
data Goal = Goal [Predict] deriving (Eq,Show)
data Rule = Rul Predict Goal deriving (Eq,Show)
data Solution = Sol String [(Term,Term)] deriving (Eq,Show)


cmp [] [] = True
cmp ((Constant x):xs) ((Constant y):ys) =
	if(x == y)  then cmp xs ys
		else False
cmp ((Var x):xs) ((y):ys) = 
	cmp xs ys
cmp ((Constant x):xs) ((Var y):ys) =
	 cmp xs ys
		

unify [] [] = []
unify((Var x):xs) ((Constant y):ys) = 
	[((Var x),(Constant y))]++ unify xs ys 
unify((x):xs) ((y):ys) = 
	unify xs ys 

unifyWithHead  (Pre x xs) (Pre y ys) =
	if(x == y && cmp xs ys) then 
		Sol "Yes" (unify  xs  ys)
	else
		Sol "NO" []

		
applySolSubInBody x (Goal []) = []
applySolSubInBody (Sol x y) (Goal ((Pre z t):zs)) =
   [Pre z (substitute1 y t)]++ (applySolSubInBody (Sol x y) (Goal zs))



substitute x [] = []
substitute  ((Var x,y):ys) ((Var z):zs) =
		if(x == z)then  y : substitute  ((Var x,y):ys) zs
			else    Var z : substitute  ((Var x ,  y):ys) zs
substitute  (y:ys) (z:zs) = 
		 z: substitute  (y:ys) zs



substitute1   [] x = x
substitute1  (y:ys) (z:zs) =
	substitute1  (ys) (substitute  (y:ys) (z:zs)) 	  	



change [] [] = []
change (x:xs) (y:ys) = [(y,x)] ++ (change xs ys)


getfact (Pre x x1) kb = (getfacts (Pre x x1) kb kb)			

getfacts x [] z  = []	
getfacts (Pre x x1) ((Rul (Pre y y1) z):xs) z2 =
	if (x==y)
	then if(z == (Goal [])) then  
         getfacts1 (Pre x x1) (Goal (applySolSubInBody (Sol "yes" (change x1 y1)) z)) z2
           else (getfacts1 (Pre x x1) (Goal (applySolSubInBody (Sol "yes" (change x1 y1)) z)) z2) ++ [Pre x x1]
	  else (getfacts  (Pre x x1) xs z2)

getfacts1 (Pre x y) (Goal []) a =[Pre x y]
getfacts1 (Pre x y) (Goal z)  a = (recgetfacts z a a)	

recgetfacts [] y z = []
recgetfacts (x:xs) y z = (getfacts x z z)++ (recgetfacts xs y z)	

checkYes (Sol x y) = 
	if(x == "Yes")
	then True
	else False 


getTerms [] = []
getTerms ((Sol x y):xs) = y++ getTerms xs

unifyWithKb x [] = []
unifyWithKb x ((Rul y y1):ys) =
		if(checkYes (unifyWithHead x y))then
	     [((unifyWithHead x y))]++ unifyWithKb x ys
		else unifyWithKb x ys
	    
concat1 y [] = []
concat1 (Sol y ys) [Sol "Yes" []] = [ys]
concat1 (Sol y ys) ((Sol x x1):xs) = [(ys++x1)]++ concat1 (Sol y ys) xs

subAndunify x y kb = 
	if((subAndunify1 (applySolSubInBody x y) kb) == [])
	then []
	else
       	concat1 x ((subAndunify1 (applySolSubInBody x y) kb))
 	
subAndunify1 (x:xs) kb = (unifyWithKb x kb) 


toSolution [] = []
toSolution (x:xs) = [(Sol "Yes" x)]++ toSolution xs

allSolution a b =  solve (getfact a b) b 

solve [] _ = []
solve (x:xs) kb =  solve1 (unifyWithKb x kb) xs kb []



solve1 a [] _ z = a
solve1 [] (x:xs) kb z = solve1 (toSolution z) xs kb []
solve1 (y:ys) (x:xs) kb z = solve1 ys (x:xs) kb ((z++((subAndunify y (Goal[x]) kb))))


 