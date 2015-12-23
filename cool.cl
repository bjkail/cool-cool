class Main {
   main() : Object {
      let io : IO <- new IO,
            is : IOInputStream <- new IOInputStream.init(io),
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
                     while isvoid parser.readToken().asEof() loop
                        false
                     pool
                  else
                     let program : ParsedProgram <- parser.parse() in
                        if if not isvoid program then
                              not listener.parse()
                           else false fi
                        then
                           let analyzer : Analyzer <- new Analyzer.init(tokenizer.lineMap()),
                                 program : AnalyzedProgram <- analyzer.analyze(program) in
                              if if not isvoid program then
                                    if not listener.analyze() then
                                       listener.interpret()
                                    else false fi
                                 else false fi
                              then
                                 let program : InterpreterProgram <- new InterpreterAnalyzer.init(tokenizer.lineMap()).analyze(program),
                                       value : InterpreterValue <- program.interpret(new IO) in
                                    if not isvoid value then
                                       case value of
                                          x : InterpreterErrorValue =>
                                             new IO.out_string("ERROR: ").out_string(x.value())
                                                   .out_string("\n").out_string(x.stack());
                                          x : Object => false;
                                       esac
                                    else false fi
                              else false fi
                        else false fi
                  fi
               fi;
         }
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

   analyze : Bool;
   analyze() : Bool { analyze };

   interpret : Bool;
   interpret() : Bool { interpret };

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
               if option = "--analyze" then
                  analyze <- true
               else
                  if option = "--interpret" then
                     interpret <- true
                  else
                     optionError <- option.concat(": unrecognized option")
                  fi
               fi
            fi
         fi
      else false fi
   };
};
