class Main {
   main() : Object {
      let program : ParsedProgram <- new Parser.init(new Tokenizer.init(new IOInputStream)).parse() in
         new IO.out_string(program.type_name()).out_string("\n")
   };
};
