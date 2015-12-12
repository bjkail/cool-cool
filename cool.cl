class Main {
   main() : Object {
      let is : IOInputStream <- new IOInputStream,
            listener : MainTokenizerListener <- new MainTokenizerListener.init(is),
            tokenizer : Tokenizer <- new Tokenizer.init(is).setListener(listener),
            parser : Parser <- new Parser.init(tokenizer),
            program : ParsedProgram <- parser.parse() in
         new IO.out_string(program.type_name()).out_string("\n")
   };
};

class MainTokenizerListener inherits TokenizerListener {
   is : IOInputStream;

   init(is_ : IOInputStream) : SELF_TYPE {{
      is <- is_;
      self;
   }};

   setEmptyLineEofCount(n : Int) : Object {
      is.setEmptyLineEofCount(n)
   };

   eof() : Object {
      is.reset()
   };
};
