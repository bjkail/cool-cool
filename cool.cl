class Main {
   main() : Object {
      let is : IOInputStream <- new IOInputStream,
            listener : MainTokenizerListener <- new MainTokenizerListener.init(is),
            tokenizer : Tokenizer <- new Tokenizer.init(is).setListener(listener),
            parser : Parser <- new Parser.init(tokenizer) in
         {
            parser.peekToken();

            let error : String <- listener.optionError() in
               if not error = "" then
                  new IO.out_string("ERROR: ").out_string(error).out_string("\n")
               else
                  if listener.lex() then
                     lex(tokenizer)
                  else
                     let program : ParsedProgram <- parser.parse() in
                        if not listener.parse() then
                           let analyzer : Analyzer <- new Analyzer.init(tokenizer.lineMap()),
                                 program : AnalyzedProgram <- analyzer.analyze(program) in
                              false
                        else false fi
                  fi
               fi;
         }
   };

   lex(tokenizer : Tokenizer) : Object {
      let continue : Bool <- true in
         while continue loop
            let token : Token <- tokenizer.next() in
               if not isvoid token.asEof() then
                  continue <- false
               else
                  let tokenError : TokenError <- token.asError() in
                     if not isvoid tokenError then
                        {
                           new IO.out_string("ERROR: ").out_string(tokenError.value()).out_string("\n");
                           continue <- false;
                        }
                     else false fi
               fi
         pool
   };
};

class MainTokenizerListener inherits TokenizerListener {
   is : IOInputStream;

   optionError : String;
   optionError() : String { optionError };

   lex : Bool;
   lex() : Bool { lex };

   parse : Bool;
   parse() : Bool { parse };

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

   handleOption(option : String) : Object {
      if optionError = "" then
         if option = "--lex" then
            lex <- true
         else
            if option = "--parse" then
               parse <- true
            else
               optionError <- option.concat(": unrecognized option")
            fi
         fi
      else false fi
   };
};
