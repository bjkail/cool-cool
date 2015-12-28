# cool-cool
An interpreter for
[Classroom Object-Oriented Language](https://theory.stanford.edu/~aiken/software/cool/cool.html)
(Cool) written in Cool.

This interpreter can be run using the
[UVA Cool Interpreter](http://www.cs.virginia.edu/~weimer/2015-4610/cool.html).
A wrapper script is provided that allows multiple input files to be specified
to the Cool Interpreter, and another script is provided to pass all requisite
files for the interpreter itself to the Cool Interpreter:

    $ export COOL_INTERPRETER=path/to/interpreter/cool
    $ bin/cool test/interpret/hello-world.cl
    Hello, World!

The interpreter can also run itself:

    $ bin/cool --bootstrap test/interpret/hello-world.cl
    Hello, World!

## UVA Dialect

The interpreter can also be run using the UVA dialect of Cool:

    $ bin/cool test/interpret/compare-bool.cl
    ERROR: test/interpret/compare-bool.cl: line 3: left expression type 'Bool' is not type 'Int' for '<' expression
    ERROR: test/interpret/compare-bool.cl: line 3: right expression type 'Bool' is not type 'Int' for '<' expression
    $ bin/cool --uva test/interpret/compare-bool.cl
    true

The UVA dialect has the following differences from the default dialect:

1. A `\c` sequence within a string represents a literal backslash followed by
the character
([Strings](http://www.cs.virginia.edu/~weimer/2015-4610/cool-manual/node35.html)),
but `IO.out_string` interprets the `\n` and `\t` sequences as a linefeed and
tab respectively
([IO](http://www.cs.virginia.edu/~weimer/2015-4610/cool-manual/node28.html)).
1. The `<` and `<=` comparison operators can be used on operands of `Int`,
`String`, and `Bool`
([Arithmetic and Comparison Operations](http://www.cs.virginia.edu/~weimer/2015-4610/cool-manual/node25.html),
[Type Checking Rules](http://www.cs.virginia.edu/~weimer/2015-4610/cool-manual/node43.html),
[Operational Rules](http://www.cs.virginia.edu/~weimer/2015-4610/cool-manual/node48.html)).
1. A stack overflow runtime error occurs after 1000 outstanding method dispatch
and `new` expressions.
([Operational Rules](http://www.cs.virginia.edu/~weimer/2015-4610/cool-manual/node48.html)).
1. Errors are printed without a filename or stack trace.
