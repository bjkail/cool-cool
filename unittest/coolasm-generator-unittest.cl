class Main inherits Test {
   test() : Object {{
      testBasic();
      testBlock();
      testIf();
   }};

   debug : Bool;

   interpretImpl(context : String, error : String, program : String, io : TestIO) : CoolasmInterpreter {
      let tokenizer : Tokenizer <- new Tokenizer.init(new TestStringInputStream.init(program)),
            parser : TestFailErrorParser <- new TestFailErrorParser.init(tokenizer).initTest(self, context),
            program : ParsedProgram <- parser.parse(),
            lineMap : TokenizerLineMap <- tokenizer.lineMap(),
            analyzer : Analyzer <- new TestFailErrorAnalyzer.initTest(self, context),
            program : AnalyzedProgram <- analyzer.analyze(program),
            program : CoolasmProgram <-
               let program : CoolasmProgram <- new CoolasmGenerator.generate(program) in
                  {
                     if debug then
                        new CoolasmWriter.init(new IO).write(program)
                     else false fi;
                     program;
                  },
            program : CoolasmInterpreterProgram <- new CoolasmInterpreterAnalyzer.setDebug(debug).analyze(program),
            interpreter : CoolasmInterpreter <- new CoolasmInterpreter.init(io) in
         {
            interpreter.setDebug(debug);
            assertBoolEquals(context.concat(" error"), error = "", interpreter.interpret(program));
            assertStringEquals(context.concat(" error"), error, interpreter.error());
            io.assert();
            interpreter;
         }
   };

   interpret(context : String, program : String, io : TestIO) : CoolasmInterpreter {
      interpretImpl(context, "", program, io)
   };

   testBasic() : Object {
      if begin("basic") then
         {
            interpret("basic", "class Main { main() : Object { new IO.out_string(\"a\") }; };",
                  newTestIO("basic", new Collection, new LinkedList.add("a")));
            interpret("basic", "class Main { main() : Object { new IO.out_int(0) }; };",
                  newTestIO("basic", new Collection, new LinkedList.add(0)));
         }
      else false fi
   };

   testBlock() : Object {
      if begin("block") then
         {
            interpret("block", "class Main { main() : Object {{ new IO.out_int(0); new IO.out_int(1); }}; };",
                  newTestIO("basic", new Collection, new LinkedList.add(0).add(1)));
         }
      else false fi
   };

   testIf() : Object {
      if begin("if") then
         {
            interpret("if true", "class Main { main() : Object { if true then new IO.out_string(\"a\") else false fi }; };",
                  newTestIO("if true", new Collection, new LinkedList.add("a")));
            interpret("if false", "class Main { main() : Object { if false then false else new IO.out_string(\"a\") fi }; };",
                  newTestIO("if false", new Collection, new LinkedList.add("a")));
         }
      else false fi
   };
};
