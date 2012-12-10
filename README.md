## Ivth

"Insignificance is our existence / Hear the litany of life's persistance."

-- Bolt Thrower, "The IVth Crusade"

### Overview

Ivth is a minimal language reminiscent of FORTH. The goal is to make
the initial language footprint as minimal as possible in the spirit of
a Turing Tarpit. However the language should be rich enough that it
can be bootstrapped into an actually useful language (going against
the spirit of most tarpits).

### Syntax

Ivth is stack-based. There is a data stack which holds 32-bit
integers, as well as a traditional call stack. All program
instructions and all program data consist of words, which are
continguous bytes of "not-whitespace".

Thus, there are a few simple ways to write hello world:

```
72 .c 69 .c 76 .c 76 .c 79 .c 32 .c 87 .c 79 .c 82 .c 76 .c 68 .c 10 .c
: # .c ; 10 68 76 82 79 87 32 79 76 76 69 72 # # # # # # # # # # #
0 10 68 76 82 79 87 32 79 76 76 69 72 dup 0br 4 .c 0 0br -7 drop
```

### Interpretation and Compilation

Like FORTH, the Ivth interpreter has two modes. It is either
interpreting or compiling. While interpreting each word is executed as
soon as it is seen. In compiling mode (used to define new words) only
"fast" words ("immediate" in FORTH) are executed.

Currently only `:` and `;` are fast. Until some of the problems with
the interpretation model are overcome it's not clear that writing
other fast words is very useful--if notx, we can remove the distinction
of these two modes and possible get ride of the `;` word as well.

### Built-in Words

There are currently 11 built-in words, although once Ivth is finished
this number should be much smaller. They are currently:

  * `(*` Used to start a comment (can be removed).
  * `copy` Put `s1` copies of `s2` on the stack.
  * `alt` Rotate the top `s1` entries of the stack left.
  * `0br` If `s1` is zero, add the next word to the current word pointer.
  * `nor` Return bitwise-nor of `s1` and `s2`
  * `+` Return the sum of `s1` and `s2`
  * `.c` Print the character value of `s1`
  * `.n` Print the decimal value of `s1` (can be bootstrapped/removed).
  * `.s` Print a representation of the stack (can be removed).
  * `:` Start defining a new word.
  : `;` End the definition of a new word.

Since 3 of the 11 can already be removed, the goal will be to reduce
the remaining 8. It would be nice to define more primitive parsing
manipulation words to bootstrap `(*`, `:`, and `;` but that might
require too much machinery in the interpreter.

If we gave up on boostrapping, we could remove `:` and `;` thus
getting rid of two more operators. Thus, of the 11 current operators
only 6 may be necessary.

### Problems

The `0br` word is ugly. Partially this is becuase the current design
reads a word ahead (like it does in FORTH) rather than reading the
address from the data stack.

A deeper problem is that we're not using memory addresses, `0br` can't
behave like a goto. This is unfortunate because it makes defining
words like `if` or higher-level loop constructs very difficult or
impossible (since `br0` inside a word definition can only jump through
that word's stack frame).

These problems are definitely solvable, and they don't impact Turing
completeness. They just prevent bootstrapping a reasonable control
flow.
