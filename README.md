# Monkey Interpreter in OCaml

This project is an implementation of the Monkey programming language in OCaml, inspired by the book [Writing an Interpreter in Go](https://interpreterbook.com/) by Thorsten Ball.

# TODO
- [x] the Lexer
- [x] the Parser (NEEDS REFACTOR)
    - [x] Let Statements
    - [x] Return Statements
    - [x] Prefix Statements
    - [x] Infix Statements
    - [x] Precendence
    - [x] Booleans
    - [x] Precendence Groups
    - [x] If Expressions
    - [x] Function Literals
    - [x] Call Expressions
    - [x] Removing TODOs
- [x] the Abstract Syntax Tree (AST)
- [x] the Internal Object System
    - [x] Integeres
    - [x] Booleans
    - [x] Null
- [x] the Evaluator 
    - [x] Integeres
    - [x] Booleans
    - [x] Null
    - [x] Prefix Expressions
    - [x] Infix Expressions
    - [x] Conditionals
    - [x] Return Statements
    - [x] Error Handling
    - [x] Bindings & the Enviroment
- [ ] Extending the Interpreter
    - [x] Strings 
    - [x] Builtin functions 
    - [x] Arrays
    - [ ] Hushes
