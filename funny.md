# Funny Language

[[RU](funny.ru.md)|EN]

Funny allows the user to define a *module*. Module consists of the named [*functions*](#functions-and-types) and [*formulas*](#formula-definitions).

This module gets compiled into a WebAssembly module.

Some Funny sources can be found at [Lab 08 samples](./lab08/samples/) and [Lab 10 samples](./lab10/samples/).

## Funny Background and Rationale

The Funny language has been designed for a very specific purpose: be a toy language for the compilation and verification education purposes.
The following major goals were set for the language:

1. Simple to parse
2. Simple to compile to Wasm
3. Complex enough to showcase the typical features of the imperative programs verification

Goal #1 affected the keyword choice and general syntax: grammar contains no ambiguities on the definitions and statements level.

Goal #2 did discourage the goto statements presence and fancy types support.

Goal #3 did encourage us to:

- support multiple return values in functions (that have also appeared quite friendly to the Goal #2)
- force immutability for the function parameters
- order the clauses in the function declaration to make sure that variable scopes are obvious
- support formulas in addition to functions

All these micro-decisions aid the user in writing the function annotations.

## Language Basics

The Funny grammar is whitespace-agnostic, i.e. any number of consequitive spaces, tabs, and newlines does have the same semantics as a single space would. In the grammars below whitespace is permitted between any rules (including terminals) in the concatenation operation. For example, the grammar `"if" "("` matches both `"if ("` and `"if("`.

Comments in Funny start with double-slash (`//`) and end with a newline (`\r` or `\n`). Comments are considered whitespace as well - i.e. they split a lexical unit, but have no meaning otherwise.

### Constants

Funny supports the type `int` constants written as digit sequences, and the boolean constants `true` and `false`.

### Identifiers

Identifiers in Funny start with a latin letter, and can contain arbitrary number of letters and digits. No fancy stuff like underscores, verbatim identifier or national alphabets are permitted for the sake of the parser simplicity.

### Exceptions

Funny function can throw a few exceptions:

- division by zero throws
- array access out of bounds throws

Since Funny does not have any means for exception *handling*, these exception can only be caught in the external code calling the Funny module.

## Functions and Types

All of the functions defined in the Funny module are automatically exported.

The syntax of the function defintion is as follows:

```EBNF
function = identifier                           (* function name *)
    "(" [ variableDef {"," variableDef  } ] ")" (* function parameter(s) *)
    ["requires" predicate ]                     (* function precondition *)
    "returns" variableDef {"," variableDef }    (* function return value(s) *)
    ["ensures" predicate ]                      (* function postcondition *)
    [ "uses" variableDef {"," variableDef } ]   (* function local variable(s) *)
    statement;                                  (* function body *)

variableDef = 
  identifier (* variable name*)
    ":" variableType;

variableType = "int" | "int[]";
```

Each function does have zero or more input parameters and one or more output parameters.

Funny supports two data types for the inputs and outputs:

- `int` refers to a 32-bit signed (two's complement) integer
- `int[]` refers to an array of `int`

Function can optionally declare local variables.
Local variables can be only of the `int` type.

The names of function input parameters, output parameters, and local variables must be unique within the function.

Functions can call the other functions, including the direct and indirect recursion.

Function names should be unique within module; no overloads are permitted.

The following function takes two integers and returns two integers:

```funny
divide(a: int, b: int)
  returns q: int, r: int
...
```

The following function takes two integers, returns a single integer, and uses two local variables:

```funny
gcd(x: int, y: int)
  returns r: int
  uses a: int, b: int  // local variable declarations
```

Function precondition can use only the function parameters. If omitted, the precondition is assumed to be `true`, i.e. satisfied for all argument values. See [Predicates](#predicates) section for more detail on the predicate syntax.

Function postcondition can use function parameters and return values. If omitted, poscondition is assumed to be `false`, i.e. never satisfied.

> While it is tempting to represent the necessity to specify the postcondition for each function via syntax, this approach proves to be counter-productive.
  Forcing a stricter syntax provides small if any simplification of the semantic analysis phase, but delivers a substantial loss in the end-user functionality.
>
> Generally speaking, syntax errors are blocking the parse process; so a straightforward parser would never produce more than a single syntax error.
  Semantic errors, on the other hand, don't necessarily block the validation process, so the compiler can detect and report multiple problems at a single run.
  This provides so much better end-user experience that the real-world parsers do often opt in to intentionally use a substantially relaxed syntax compared to the official language specification. While general *parser error recovery* is way beyond this course scope, it worth mentioning at this tiny example.

## Formula Definitions

Formula is a special kind of function to be used in the predicates. It does never get compiled to the Wasm code, and is used only for the static verification:

```EBNF
formula = identifier                            (* formula name *)
    "(" [ variableDef {"," variableDef  } ] ")" (* formula parameter(s) *)
    "=>" predicate;                             (* formula body *)
```

## Statements

There are four statement types in Funny:

```EBNF
statement = assignment | conditional | loop | block;
```

### Assignment Statement

Assignment statement has the following form:

```EBNF
assignment = 
    varName "=" expr ";"                           (* simple assignment *)
    | arrayAccess "=" expr ";"                     (* modifying an array element *)
    | varName { "," varName } "=" functionCall ";" (* tuple assignment *)
```

The latter form is a tuple assignment; it applies to the functions returning multiple results. See the [Function Call](#function-call) subsection below for more detail.

Note that the function parameters are treated as read-only - those cannot be used on the left side of the assignment statement. This covers the arrays as well - an array-type parameter cannot be used on the left side of the assignment.

Both local variables and output parameters are treated as read-write.
Reading before the initial assignment is not an error, but the contents of the unitialized variables are undefined.

### Conditional Statement

The conditional statement has the following form:

```EBNF
conditional = "if" "(" condition ")" statement ["else" statement];
```

Nested conditional statements are right-associative. Here the `else` belongs to the second `if`:

```funny
if (x > 0) if (y < 0) z = 1; else z = 5;
// equivalent to:
if (x > 0) 
{ 
    if (y < 0) 
        z = 1; 
    else z = 5;
}
// NOT to:
if (x > 0) 
{ 
    if (y < 0) 
        z = 1; 
}
else 
    z = 5;
```

### Loop Statement

The loop statement has the following form:

```EBNF
while = "while" "(" condition ")" 
    [ "invariant" predicate ]
    statement;
```

If omitted, the loop invariant is assumed to be `true`. This clause is intentionally made optional for the same reasons as the [function](#functions-and-types) pre- and post-conditions.

### Block Statement

The block statement allows to group a few statements together:

```EBNF
block = "{" { statement } "}";
```

Note that semicolon is not a "statement separator" - it is a part of the assignment statement.

## Expressions

The Funny expressions are basically the same as in [Lab 03](./lab03/README.md), with two additional atoms:

```EBNF
expr = 
    functionCall  
  | arrayAccess
(* this part is the same arithmetic expression as in Lab 03 *)  
  | number
  | variable
  | "-" expr
  | expr "+" expr 
  | expr "*" expr
  | expr "-" expr
  | expr "/" expr
  | "(" expr ")";
```

The operation prioriries do follow the common convention: multiplicative operations (`*`, `/`) are of a higher priority than the additive ones (`+`, `-`); the unary `-` operation takes the highest precedence. All the binary operations are considered to be left-associative (calculated left-to-right).

### Function Call

The function call expression uses the positional arguments:

```EBNF
functionCall = identifier       (* function name *)
    "(" [ expr {"," expr} ] ")" (* function arguments *)
```

The function arguments are evaluated left-to-right.

**Note** that only a single-return functions can be used in expressions; the function returning multiple results can either be called from the external code, or in the [tuple assignment](#assignment-statement).

The types and number of the expressions in the function argument list must match the function parameter types and number. The function name must refer to an existing function defined in the module, or the built-in function [`length`](#length-function).

#### Length Function

The built-in `length` function returns an array length. It behaves as if it has been declared with the following signature:

```funny
length(a: int[]) returns l: int
```

### Array Access

Array access operation allows to read an array element value:

```EBNF
arrayAccess = varName "[" expr "]"
```

Variable referenced by the varName must be of an array type; expression used in the index must be of an integer type.
Array access outside of array bounds throws a runtime exception.

## Conditions

Conditional and while statements do use the *condition* constructs.
Those constructs represent the *boolean expressions*, built from the comparison expressions and `true`/`false` constants by applying conjunction, disjunction, and negation:

```EBNF
condition = 
  "true" | "false"
  | comparison
  | "not" condition
  | condition "and" condition
  | condition "or" condition
  | condition "->" condition
  | "(" condition ")";

comparison = 
  expr "==" expr
  | expr "!=" expr
  | expr ">=" expr
  | expr "<=" expr
  | expr ">" expr
  | expr "<" expr;

```

The boolean operation prioriries do follow the common convention: negations is evaluated first, then conjunction, then disjunction, then implication. Conjunction and disjunction are considered to be left-associative, implication is right-associative!
I.e. `a -> b -> c` is parsed as `a -> (b -> c)`, *not* as `(a -> b) -> c`.

## Predicates

Predicates in Funny are similar to the [conditions](#conditions).
Those are Boolean formulas as well, but the predicate atoms allow two additional constructs - [*quantifiers*](#quantifiers) and [*formula references*](#formula-references):

```EBNF
predicate = 
  quantifier
  | formulaRef
(* this part is similar to the condition *)
  | "true" | "false"
  | comparison
  | "not" predicate
  | predicate "and" predicate
  | predicate "or" predicate
  | "(" predicate ")";

```

### Quantifiers

The quantifier construct allows to declare the existential and the universal quantifiers in predicates.

```EBNF
quantifier = ("forall" | "exists") (* quantifier type *)
    "(" variableDef                (* predicate variable *)
        "|" predicate              (* predicate *)
     ")";
```

**Note** that the variable name declared in the quantifier must be unique within the scope that contains the quantifier:

- function precondition scope contains all function parameters
- function postcondition scope contains all function parameters and return values
- loop invariant scope contains all function parameters, return values, and local variables

### Formula References

Formula references behave similar to the function calls, used within predicates only. Formula references cannot be used in conditions nor in expressions.

```EBNF
formulaRef = identifier         (* formula name *)
    "(" [ expr {"," expr} ] ")" (* formula arguments *)
```

Formula reference must refer to an existing [formula definition](#formula-definitions).
