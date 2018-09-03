# Examples


## Matrix multiplication

TODO: Iteration
```
s = ((0, 1),
     (2, 3))
t = ((4, 5, 6),
     (7, 8, 9))

zip = f: Function(T, S, U) xs: Vector(N, T) ys: Vector(N, S) -> vs: Vector(N, U)
  vs <- (v..) # Consume all of v to vector
    v <- f x y # type v: Stream(N*M, U)
      x in xs
      y in ys

dot = v w -> zip * v w

trans = m: Matrix(N, M, T) -> n: Matrix(M, N, T)
  n <- (rows..)
  rows <- 


mul = a: Matrix(N, M, T) b: Matrix(M, N, T) -> c: Matrix(N, N)
  c <- (rows..)
  rows <- 

u <- mul s t

type u: Matrix(2, 3, Integer)
u = (( 7,  8,  9),
     (29, 34, 39))
```

## Fibonacci

TODO: Control Flow
```
fib 1 = 0
fib 2 = 1
fib = n -> a + b
  n1 <- n - 1
  n2 <- n - 2
  a <- fib n1
  b <- fib n2
```
