# Regular Expressions

A regular expression is a pattern that specifies strings that should be matched. 
These are used by the lexer generator to match certain strings that constitute a
token.

## Regular Expression Grammar

The following is the grammar for regular expressions used by the lexer generator.

```
regexp := regexp2
        | regexpr2 '|' regexp2

regexp2 := regexp1
         | regexp1 regexp 1
         
regexp1 := regexp0
         | regexp0 '*'
         
regexp0 := char
         | '(' regexp ')'
```

Here, `char` represents a single character.

## Currently Unsupported

The following common regular expression constructs are currently unsupported.
Support for these constructs will be added in future versions.

- Escape Sequnces - It is not currently possible to match escape sequences such as `\n`, `\r`.
- Match one or more - The `+` operator that matches one or more occurrences of a construct.
- Match zero or one - The `?` operator that matches zero or one occurrence of a construct.
- Sets - Sets of the form `[...]` for matching characters in a range.
- Complement - The `~` operator for matching the complement of a set.
- Set Complement - The `^` operator for matching the complement of the union of sets.



