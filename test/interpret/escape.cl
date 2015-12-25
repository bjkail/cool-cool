--test=
--test=--no-stdin
--test=--bootstrap
class Main {
   main() : Object {
      new IO.out_string("backslash [\\\\] [\\]\ntab [\\t] [\t]\nlinefeed [\\n] [\n]\n")
   };
};
