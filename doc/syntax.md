# Syntax

## Data types

Type names (and aliases) are capitalized.

Integers
```
a = -5
b = 10
```

Scalars
```
a = 100 / 10
b = 3 / 1
```

Vectors
```
v = (1, 2, 3)
u = ()
```

Functions
```
f = x: Integer y: Integer -> plus x y
id = a: T -> a
```

## Primitive vs Expression

Expression evaluation can take arbitary time, so primitives
and expressions are assigned with different syntax
```
# Primitive assignment
arg = 5
fun = otherFunc

# Expression assignment
v <- fun arg arg arg
```

## Significant indentation

Same level expressions are parallel when possible.
Indentation implies ordering, inner parts finish before outer parts:
```
a <- plus b c
  b <- h
  c <- i
```

Data dependencies linearize execution, in this case 
execution order is b, c, d. 
```
c <- g d b
d <- h b a
b <- f a
```

## WIP

### Type aliases

Matrices
```
alias Matrix(N, M, T) = Vector(N, Vector(M, T))

m = ((1, 2, 3, 4),
     (2, 4, 5, 2),
     (4, 5, 3, 4))
type m = Matrix(3, 4, Integer)
```

### Streams

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