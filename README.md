# haskell-hasp

## atom

```lisp
> 1
1
> 2
2
```

## if

```lisp
> (if 1 2 3)
2
> (if 0 2 3)
3
```

### def

```lisp
> (def a 1)
1
> a
1
```

### lambda

```lisp
> (lambda (a) (if a 2 3))
#<lambda:a>
> ((lambda (a) (if a 2 3)) 1)
2
> ((lambda (a) (if a 2 3)) 0)
3

> (def f (lambda (a) (if a 2 3)))
#<lambda:a>
> (f 1)
2
> (f 0)
3
```
