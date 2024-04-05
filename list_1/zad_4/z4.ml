let scan f a s  = let rec h n = if n == 0 then f a (s 0) 
			    	else f (h (n - 1)) (s n)  in h
