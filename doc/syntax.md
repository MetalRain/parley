# Syntax

Type inference handles most of cases.

## Data types

Type names (and aliases) are capitalized.

Integers
```
a = 5
b = 10
c <- a + b
type c = Integer
c = 15
```

Scalars
```
a = 100 / 10
b = 3.0
c = 3.33..
a = b * c
```

Vectors
```
v = (1, 2, 3)
type v = Vector(3, Integer)
```

Functions
```
f x y -> x + y
f = +
```

## Type aliases

Matrices
```
alias Matrix(N, M, T) = Vector(N, Vector(M, T))

m = ((1, 2, 3, 4),
     (2, 4, 5, 2),
     (4, 5, 3, 4))
type m = Matrix(3, 4, Integer)
```


## Function application

Function execution is separate from assignment
```
# Assignment
arg = 5
fun = otherFunc

# Execution
v <- fun arg arg arg
```


## Streams

Streams have length and type, evaluated lazily
```
xs: Stream(3, Integer) in (1, 2, 3)
```

Streams allow loading data in chunks
```
window: Stream(2000000, )
```


Streams can be their own consumers
```
repeat v: Vector(N, T) <- vs: Stream(Inf, T)
  vs in (v, vs..)
```

## Significant indentation

Same level expressions are parallel when possible:
```
f
g
```

Indentation implies ordering, inner parts finish before outer parts:
```
a <- b + c
  b <- h
  c <- i
```


Data dependencies linearize execution
```
b <- f a
c <- g a b
d <- h b a
```


Functions g and h depend on b:
```
Thread 1  | Thread 2
 f a      |
 g a b    | h b a
```


## Dependent types

Type check type invariants
```
mul = m: Matrix(N, M, Integer) n: Matrix(M, N, Integer) -> x: Mat(N, N, Integer)
  ...
```
