let rec tabulate stm ?(x = 0) y = if (x > y) then []
				  else if (x == y) then [stm x]
				       else stm x :: tabulate stm ~x:(x + 1) y
