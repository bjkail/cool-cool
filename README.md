# cool-cool
A compiler for
[Classroom Object-Oriented Language](https://theory.stanford.edu/~aiken/software/cool/cool.html)
(Cool) written in Cool.

This compiler can be run using the
[Cool Interpreter](http://www.cs.virginia.edu/~weimer/2015-4610/).  A wrapper
script is provided that allows multiple input files to be specified to the Cool
Interpreter, and another script is provided to pass all requisite files for the
compiler itself to the Cool Interpreter:

    $ export COOL_INTERPRETER=path/to/interpreter/cool
    $ bin/cool path/to/hello-world.cl
