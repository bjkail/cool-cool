# cool-cool
An interpreter for
[Classroom Object-Oriented Language](https://theory.stanford.edu/~aiken/software/cool/cool.html)
(Cool) written in Cool.

This interpreter can be run using the
[Cool Interpreter](http://www.cs.virginia.edu/~weimer/2015-4610/cool.html).  A wrapper
script is provided that allows multiple input files to be specified to the Cool
Interpreter, and another script is provided to pass all requisite files for the
interpreter itself to the Cool Interpreter:

    $ export COOL_INTERPRETER=path/to/interpreter/cool
    $ bin/cool test/interpret/hello-world.cl
    Hello, World!

The interpreter can also run itself:

    $ bin/cool --bootstrap test/interpret/hello-world.cl
    Hello, World!
