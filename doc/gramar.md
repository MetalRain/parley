# Grammar

## Identifiers

Identifiers refer to values, identifier names must:
- have only alphanumeric values
- start with small alpha

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
f = a b -> a + b
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

Functions are computations that yield result based on inputs alone.
```
f = a: Integer -> plus a 1
```

Function result depends on inputs, when there are no inputs there is no function.
Use identifiers instead.

```
# Invalid functions
-> 5
-> f 6

# Use idenfiers
a = 5
b <- f 6

# Even anonymous functions are invalid, since language doesn't have nested expressions
a b -> a + b

# Assign identifier to function
f = a: T b: S -> a + b
```

## Swarms

Indentation defines swarm of applications that should happen in parallel,
in this example b and c can execute at the same time.
```
x = 1
a <- plus b c
  b <- f x
  c <- g x
```

## WIP

### Function recursion

```
# Recursion is fine
g = f: Function x: T -> h x
  h = y -> g f y
```


### Destructuring

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
x in v
```

Stream construction by recursion:
```
v in (1, 2, 3, v..)
```

TODO: Consuming streams:
```
x from v
```