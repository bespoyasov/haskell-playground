# Stepik Course on Haskell

Source code for tasks from [this course](https://stepik.org/course/75/syllabus).

## Haskell Operators Cheat Sheet

```
>>=     bind
>>      then
*>      then
->      to                a -> b: a to b
<-      bind              (as it desugars to >>=)
<$>     (f)map
<$      map-replace by    0 <$ f: "f map-replace by 0"
<*>     ap(ply)           (as it is the same as Control.Monad.ap)
$                         (none, just as " " [whitespace])
.       pipe to           a . b: "b pipe-to a"
!!      index
!       index / strict    a ! b: "a index b", foo !x: foo strict x
<|>     or / alternative  expr <|> term: "expr or term"
++      concat / plus / append
[]      empty list
:       cons
::      of type / as      f x :: Int: f x of type Int
\       lambda
@       as                go ll@(l:ls): go ll as l cons ls
~       lazy              go ~(a,b): go lazy pair a, b
```

And:

```
| sym  | pronunciation                                    |
|------|--------------------------------------------------|
| |    | "such that"                                      |
| <-   | "is drawn from"                                  |
| =    | "is defined to be" / "is defined as"             |
| ::   | "has type" / "of type" / "is of type"            |
| ->   | "a function that takes ... and returns a ..." /  |
|      |                          "function that maps" /  |
|      |                          "is a function from" /  |
|      |                                          "to"    |
| $    | "apply"                                          |
| _    | "whatever"                                       |
| !!   | "index"                                          |
| ++   | "concat"                                         |
| []   | "empty list"                                     |
| :    | "cons"                                           |
| \    | "lambda"                                         |
| =>   | "implies" / "then"                               |
| *>   | "then"                                           |
| <$>  | "fmap" / "dollar cyclops"                        |
| <$   | "map-replace by"                                 |
| <*>  | "ap" / "star cyclops"                            |
| .    | "pipe to" / "compose" / "dot"                    |
| <|>  | "or"                                             |
| @    | "as"                                             |
| ~    | "lazy"                                           |
| <=<  | "left fish"                                      |
```

Taken from [here](https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators).
