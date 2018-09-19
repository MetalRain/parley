# Grammar

## Identifiers

Identifiers refer to values, identifier names must:
- have only alphanumeric values
- start with small letter

## Primitives

Language has following primitive types
- Integer
- Scalar
- Vector
- Function

### Assignment

Primitives are assigned to identifiers with =

```
a = (1, 2)
b = 45
c = 1 / 3
f = a b -> plus a b
```

### Integer

Integers are arbitrary size integers: -infinity to infinity.

### Scalar

Arbitrary precision rational numbers.

```
1 / 3
40 / 100
```

### Vector
Vectors are linear memory regions, values are accessed by zero indexed positions.

```
v = (1, 2, 3)

x <- idx v 2
# x = 3

y <- idx v 3
# Compilation error, since type v = Vector(3, Integer)
```

### Function

Functions return single value using inputs and previously assigned identifiers.
```
f = a: Integer -> plus a 1
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

## Future ideas


### Type aliases

Matrices
```
alias Matrix(N, M, T) = Vector(N, Vector(M, T))

m = ((1, 2, 3, 4),
     (2, 4, 5, 2),
     (4, 5, 3, 4))
type m = Matrix(3, 4, Integer)
```

### Recursion

```
# Recursion is fine
g = f: Function x: T -> h x
  h = y:  -> g f y
```

### Vector destructuring

Vectors can be destructured with v..
This is used for append, prepend, concat, variable amount argument application 
```
v = (1, 2, 3)
f v..
# Is same as
f 1 2 3

vv = (v.., v..)
# vv = (1, 2, 3, 1, 2, 3)

m = (v, v)
# m = ((1, 2, 3), (1, 2, 3))
```


### Streams

Streams are lazy value generators.

Stream construction from primitive:
```
v = (1, 2, 3, 4, 5, 6, 7)
x <- in v
```

Stream construction by recursion:
```
v <- in res
  res <- join (1, 2, 3) v
```

Streams can be their own consumers
```
repeat x: Vector(N, T) -> s: Stream(T)
  s <- in (v, s..)
```