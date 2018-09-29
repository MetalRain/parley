# Syntax

## Identifiers

Identifiers refer to values, identifier names must:
- have only alphanumeric values
- start with small letter

## Primitive types

Language has following primitive types:
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
# Compilation error, since v: Vector(3, Integer)
```

### Function

Functions return single value using inputs and previously assigned identifiers.
```
f = a: Integer -> plus a 1
```

## Expresions

All expressions are function evaluations that assign value to identifier.
```
a <- f b

stdout <- print 1
```

## Type system

Types can be either:
- [Primitive types](#Primitive%20types)
- [Data types](#Data%20types)
- [Nested types](#Nested%20types)
- [Type variables](#Type%20variables)
- [Type aliases](#Type%20aliases)

### Data types

Data types are primitives that are used as types.

Data types can be used to:
- create termination conditions to recursive functions.
- create own vector like types, such as enums and records.
```
fib n: 1 -> id 0
fib n: 2 -> id 1
fib n: Integer -> plus f1 f2
  n1 <- minus n 1
  n2 <- minus n 2
  f1 <- fib n1
  f2 <- fib n2
```

### Nested types

Nested types give name to list of other types.

Function is nested type, which defines argument types and output type.
```
f = a: Integer b: Integer -> plus a b
# f: Function(Integer, Integer, Integer)
```

Vector is a nested type, which defines vector length and type of contents.
```
v = (1, 2, 3)
# v: Vector(3, Integer)
```

### Type variables

If function doesn't care about type of argument, type variables can be used.
When type variable is data type it can also be used in expressions as primitives

```
length = v: Vector(n, t) -> n
# length: Function(Vector(n, t), Integer)
```

### Type aliases

Types can be composed by giving them less verbose aliases.

```
alias Vec3(t) = Vector(3, t)

alias Matrix(n, m, t) = Vector(n, Vector(m, t))

m = ((1, 2, 3, 4),
     (2, 4, 5, 2),
     (4, 5, 3, 4))
# m : Matrix(3, 4, Integer)
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

### Recursion

Functions can be recursive
```
mulplus = x: 0 y: Integer -> id y
mulplus = x: Integer y: Integer -> mulplus x1 y
  x1 <- minus x 1
```

## Future ideas

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