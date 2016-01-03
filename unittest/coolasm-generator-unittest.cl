class Main inherits Test {
   test() : Object {{
      testBasic();
      testBlock();
      testIf();
      testLet();
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

   interpretExpr(context : String, program : String, io : TestIO) : CoolasmInterpreter {
      interpret(context, "class Main { main() : Object { ".concat(program).concat(" }; };"), io)
   };

   testBasic() : Object {
      if begin("basic") then
         {
            interpretExpr("basic", "new IO.out_string(\"a\")",
                  newTestIO("basic", new Collection, new LinkedList.add("a")));
            interpretExpr("basic", "new IO.out_int(0)",
                  newTestIO("basic", new Collection, new LinkedList.add(0)));
         }
      else false fi
   };

   testBlock() : Object {
      if begin("block") then
         {
            interpretExpr("block", "{ new IO.out_int(0); new IO.out_int(1); }",
                  newTestIO("basic", new Collection, new LinkedList.add(0).add(1)));
         }
      else false fi
   };

   testIf() : Object {
      if begin("if") then
         {
            interpretExpr("if true", "if true then new IO.out_string(\"a\") else false fi",
                  newTestIO("if true", new Collection, new LinkedList.add("a")));
            interpretExpr("if false", "if false then false else new IO.out_string(\"a\") fi",
                  newTestIO("if false", new Collection, new LinkedList.add("a")));
         }
      else false fi
   };

   testLet() : Object {
      if begin("let") then
         {
            interpretExpr("let", "new IO.out_int(let a : Int in 0)",
                  newTestIO("let", new Collection, new LinkedList.add(0)));

            interpretExpr("int default", "new IO.out_int(let a : Int in a)",
                  newTestIO("int default", new Collection, new LinkedList.add(0)));
            interpretExpr("string default", "new IO.out_string(let a : String in a)",
                  newTestIO("string default", new Collection, new LinkedList.add("")));
            interpretExpr("bool default", "if let a : Bool in a then false else new IO.out_int(0) fi",
                  newTestIO("bool default", new Collection, new LinkedList.add(0)));

            interpretExpr("int initialization", "new IO.out_int(let a : Int <- 1 in a)",
                  newTestIO("int initialization", new Collection, new LinkedList.add(1)));
            interpretExpr("string initialization", "new IO.out_string(let a : String <- \"a\" in a)",
                  newTestIO("string initialization", new Collection, new LinkedList.add("a")));
            interpretExpr("bool initialization", "if let a : Bool <- true in a then new IO.out_int(0) else false fi",
                  newTestIO("bool initialization", new Collection, new LinkedList.add(0)));
         }
      else false fi
   };
};
