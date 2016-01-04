class Main inherits Test {
   test() : Object {{
      testBasic();
      testBlock();
      testIf();
      testLet();
      testCase();
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

   testCase() : Object {
      if begin("case") then
         {
            interpretExpr("void", "case let void : Object in void of x : Object => x; esac",
                  newTestIO("void", new Collection, new LinkedList.add("ERROR: 1: Exception: case on void\n")));
            interpretExpr("unmatched", "case 0 of x : Bool => x; esac",
                  newTestIO("unmatched", new Collection, new LinkedList.add("ERROR: 1: Exception: case branch not matched for type 'Int'\n")));

            interpretExpr("single", "new IO.out_int(case 1 of x : Int => x; esac)",
                  newTestIO("single", new Collection, new LinkedList.add(1)));
            interpretExpr("unrelated", "new IO.out_int(case 0 of x : Int => 1; x : String => 0; esac)",
                  newTestIO("unrelated", new Collection, new LinkedList.add(1)));
            interpretExpr("unrelated 2", "new IO.out_int(case 0 of x : String => 0; x : Int => 1; esac)",
                  newTestIO("unrelated 2", new Collection, new LinkedList.add(1)));
            interpretExpr("ordered", "new IO.out_int(case 0 of x : Int => 1; x : Object => 0; esac)",
                  newTestIO("ordered", new Collection, new LinkedList.add(1)));
            interpretExpr("unordered", "new IO.out_int(case 0 of x : Object => 0; x : Int => 1; esac)",
                  newTestIO("unordered", new Collection, new LinkedList.add(1)));

            interpretExpr("object", "case 0 of x : Object => new IO.out_int(1); esac",
                  newTestIO("object", new Collection, new LinkedList.add(1)));

            interpret("hierarchy", ""
                  .concat("class Main { main() : Object {{")
                  .concat("  new IO.out_int(case new A of x : A => 1; esac);")
                  .concat("  new IO.out_int(case new A of x : B => 2; esac);")
                  .concat("  new IO.out_int(case new A of x : C => 3; esac);")
                  .concat("}}; };")
                  .concat("class A inherits B { a : Int; };")
                  .concat("class B inherits C { b : Int; };")
                  .concat("class C { c : Int; };"),
                  newTestIO("hierarchy", new Collection, new LinkedList.add(1).add(2).add(3)));
         }
      else false fi
   };
};
