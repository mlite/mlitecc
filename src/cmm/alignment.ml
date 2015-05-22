type t = int * int
let init k = (0, k)
let add i (n, k) = (n + i, k)
let align k (n', k') = (0, k)
let rec gcd n m =
  if n > m then gcd m n
  else if n = 0 then m
  else gcd (m - n) n
let alignment (n, k) = gcd n k
