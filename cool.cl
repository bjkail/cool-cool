class Main {
   main() : Object {
      let rawIo : IO <- new IO,
            line : String <- rawIo.in_string(),
            stdio : Bool <- line = "--cool:stdio",
            io : ExtendedIO <-
               if stdio then
                  new MainStdinIO.init(rawIo)
               else
                  new MainBufferedLineIO.init(rawIo, line)
               fi,
            is : IOInputStream <- new IOInputStream.init(io),
            listener : MainTokenizerListener <- new MainTokenizerListener.init(is),
            tokenizer : Tokenizer <- new Tokenizer.init(is).setListener(listener),
            parser : Parser <- new MainParser.initMain(io, listener, tokenizer) in
         {
            parser.peekToken();

            let error : String <- listener.optionError() in
               if not error = "" then
                  io.out_string("ERROR: ").out_string(error).out_string("\n")
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
                           let lineMap : TokenizerLineMap <- tokenizer.lineMap(),
                                 uva : Bool <- listener.uva(),
                                 analyzer : Analyzer <- new MainAnalyzer.initMain(io, lineMap).setUva(uva),
                                 program : AnalyzedProgram <- analyzer.analyze(program) in
                              if if not isvoid program then
                                    not listener.analyze()
                                 else false fi
                              then
                                 {
                                    if listener.stdin() then
                                       while io.in_string() = "" loop
                                          false
                                       pool
                                    else false fi;

                                    let program : InterpreterProgram <- new InterpreterAnalyzer.setUva(uva).analyze(program),
                                          interpreter : Interpreter <- new Interpreter
                                             .init(lineMap, io, listener.stdin())
                                             .initDebug(listener.debug()),
                                          value : InterpreterValue <- interpreter.interpret(program) in
                                       if not isvoid value then
                                          case value of
                                             x : InterpreterErrorValue =>
                                                if uva then
                                                   let stack : String <- x.stack() in
                                                      if stack = "" then
                                                         io.out_string(x.value()).out_string("\n")
                                                      else
                                                         io.out_string("ERROR: ").out_string(stack)
                                                               .out_string(": Exception: ").out_string(x.value())
                                                               .out_string("\n")
                                                      fi
                                                else
                                                   io.out_string("ERROR: ").out_string(x.value())
                                                         .out_string("\n").out_string(x.stack())
                                                fi;
                                             x : Object => false;
                                          esac
                                       else false fi;
                                 }
                              else false fi
                        else false fi
                  fi
               fi;
         }
   };
};

class MainBufferedLineIO inherits ExtendedIO {
   io : IO <- new IO;
   line : String;
   usedLine : Bool;

   init(io_ : IO, line_ : String) : SELF_TYPE {{
      io <- io_;
      line <- line_;
      self;
   }};

   in_string() : String {
      if usedLine then
         self@ExtendedIO.in_string()
      else
         {
            usedLine <- true;
            line;
         }
      fi
   };
};

class MainStdinIO inherits ExtendedIO {
   backslash : String <- new StringUtil.backslash();
   literalEscape : Bool <- "\\".length() = 2;
   io : IO;

   init(io_ : IO) : SELF_TYPE {{
      io <- io_;
      self;
   }};

   out_string(s : String) : SELF_TYPE {
      if literalEscape then
         let i : Int,
               begin : Int,
               escapes : Int in
            {
               while i < s.length() loop
                  if s.substr(i, 1) = backslash then
                     let c : String <- s.substr(i + 1, 1) in
                        if c = backslash then
                           {
                              let length : Int <- i + 1 - begin in
                                 outString(s.substr(begin, length), length - escapes);
                              begin <- i + 2;
                              escapes <- 0;
                              i <- begin;
                           }
                        else
                           {
                              if if c = "n" then
                                    true
                                 else
                                    c = "t"
                                 fi
                              then
                                 escapes <- escapes + 1
                              else false fi;

                              i <- i + 2;
                           }
                        fi
                  else
                     i <- i + 1
                  fi
               pool;

               if begin < s.length() then
                  let length : Int <- s.length() - begin in
                     outString(s.substr(begin, length), length - escapes)
               else false fi;

               self;
            }
      else
         outString(s, s.length())
      fi
   };

   outString(s : String, length : Int) : SELF_TYPE {{
      if not length = 0 then
         io.out_string(">S:").out_int(length).out_string("\n").out_string(s)
      else false fi;
      self;
   }};

   out_int(i : Int) : SELF_TYPE {{
      io.out_string(">I:").out_int(i).out_string("\n");
      self;
   }};

   in_string() : String {{
      io.out_string("<\n");
      io.in_string();
   }};

   in_int() : Int {{
      io.out_string("<\n");
      io.in_int();
   }};
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

   uva : Bool;
   uva() : Bool { uva };

   stdin : Bool;
   stdin() : Bool { stdin };

   debug : StringMap <- new StringListMap;
   debug() : StringMap { debug };

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

   setUva(uva_ : Bool) : Object {
      uva <- uva_
   };

   error(s : String) : Object {
      if optionError = "" then
         optionError <- s
      else false fi
   };

   handleDebugOption(s : String) : Object {
      let i : Int,
            begin : Int in
         {
            while i < s.length() loop
               {
                  if s.substr(i, 1) = "," then
                     {
                        debug.putWithString(s.substr(begin, i - begin), true);
                        begin <- i + 1;
                     }
                  else false fi;

                  i <- i + 1;
               }
            pool;

            debug.putWithString(s.substr(begin, i - begin), true);
         }
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
                  if option = "--stdin" then
                     stdin <- true
                  else
                     let value : String in
                        {
                           let i : Int,
                                 continue : Bool <- i < option.length() in
                              while continue loop
                                 if option.substr(i, 1) = "=" then
                                    {
                                       value <- option.substr(i + 1, option.length() - i - 1);
                                       option <- option.substr(0, i);
                                       continue <- false;
                                    }
                                 else
                                    {
                                       i <- i + 1;
                                       continue <- i < option.length();
                                    }
                                 fi
                              pool;

                           if option = "--debug" then
                              handleDebugOption(value)
                           else
                              error(option.concat(": unrecognized option"))
                           fi;
                        }
                  fi
               fi
            fi
         fi
      else false fi
   };
};

class MainParser inherits Parser {
   io : IO;
   listener : MainTokenizerListener;

   initMain(io_ : IO, listener_ : MainTokenizerListener, tokenizer : Tokenizer) : SELF_TYPE {{
      io <- io_;
      listener <- listener_;
      init(tokenizer);
   }};

   reportError(line : Int, s : String) : Object {{
      io.out_string("ERROR: ");
      if listener.uva() then
         io.out_int(line).out_string(": ")
      else
         if not line = 0 then
            io.out_string(tokenizer.lineMap().lineToString(line)).out_string(": ")
         else false fi
      fi;
      io.out_string(s).out_string("\n");
   }};
};

class MainAnalyzer inherits Analyzer {
   io : IO;
   lineMap : TokenizerLineMap;

   initMain(io_ : IO, lineMap_ : TokenizerLineMap) : SELF_TYPE {{
      io <- io_;
      lineMap <- lineMap_;
      self;
   }};

   reportError(line : Int, s : String) : Object {{
      io.out_string("ERROR: ");
      if uva then
         io.out_int(line).out_string(": ")
      else
         if not line = 0 then
            io.out_string(lineMap.lineToString(line)).out_string(": ")
         else false fi
      fi;
      io.out_string(s).out_string("\n");
   }};
};
