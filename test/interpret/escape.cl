--test=
--test=--no-stdin
--test=--bootstrap
class Main {
   main() : Object {
      new IO.out_string("c [\\c] [c]\nbackslash [\\\\] [\\]\nbackspace [\\b] [\b]\ntab [\\t] [\t]\nnewline [\\n] [\n]\nformfeed [\\f] [\f]\nnewline [\\\n] [\
]\n")
   };
};
