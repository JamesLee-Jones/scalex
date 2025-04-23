# Scalex

Scalex is a lexer generator that makes use of Scala's meta-programming support to allow for the declaration of Lexers 
in Scala source code. This means lexer specifications will be type-checked at compile time, causing malformed 
specifications to result in a compilation error instead of the production of an ill-typed lexer source file.

## Using Scalex

TODO: Add distribution and information

### A lexer for numeric expressions

Consider the following simple regular expressions representing the tokens for a simple numeric expression language:
```
number = (1|2|3|4|5|6|7|7|9)(0|1|2|3|4|5|6|7|7|9)*
plus = +
minus = - 
whitespace = ' '
```
Note that Scalex does not currently support the range construct. This will be added in a future version. For a grammar
outlining the currently supported regular expression constructs, see the [regex document](./docs/regex.md).

This could be converted into a lexer using Scalex as follows:
```scala 3
sealed trait Token
case object Number extends Token
case object Plus extends Token
case object Skip extends Token
```
First, we must define the tokens that the input string should be converted to. It is then possible to define the lexer:
```scala 3
val lexer =
  Lexer(
    "(1|2|3|4|5|6|7|7|9)(0|1|2|3|4|5|6|7|7|9)*" -> Number
    "+" -> Plus
    "-" -> Minus
    " " -> Skip
  )
```
An input can be lexed using the `.lex(input)` function that returns either an error if `input` is malformed, or a
`List[Token]` if lexing is successful. For example `lexer.lex("10+9)` would return `Right(List(Number, Plus, Number))`
whereas `lexer.lex(10+a)` would return `Left(...)`. 
