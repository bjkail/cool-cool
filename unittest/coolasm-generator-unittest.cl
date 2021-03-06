class Main inherits Test {
   test() : Object {{
      testBasic();
      testBlock();
      testIf();
      testLet();
      testCase();
      testAssignment();
      testNew();
      testDispatch();
      testUnary();
      testBinary();
      testBasicClasses();
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

   testAssignment() : Object {
      if begin("assignment") then
         {
            interpret("attribute",
                  "class Main { a : Int; main() : Object {{ a <- 1; new IO.out_int(a); }}; };",
                  newTestIO("attribute", new Collection, new LinkedList.add(1)));
            interpret("attribute value",
                  "class Main { a : Int; main() : Object { new IO.out_int(a <- 1) }; };",
                  newTestIO("attribute value", new Collection, new LinkedList.add(1)));

            interpretExpr("var", "new IO.out_int(let a : Int in { a <- 1; a; })",
                  newTestIO("var", new Collection, new LinkedList.add(1)));
            interpretExpr("var value", "new IO.out_int(let a : Int in a <- 1)",
                  newTestIO("var value", new Collection, new LinkedList.add(1)));

            interpret("argument",
                  "class Main { main() : Object { a(0) };\na(a : Int) : Object {{ a <- 1; new IO.out_int(a); }}; };",
                  newTestIO("argument", new Collection, new LinkedList.add(1)));
            interpret("argument value",
                  "class Main { main() : Object { a(0) }; a(a : Int) : Object { new IO.out_int(a <- 1) }; };",
                  newTestIO("argument value", new Collection, new LinkedList.add(1)));
         }
      else false fi
   };

   testNew() : Object {
      if begin("new") then
         {
            interpretExpr("bool", "new IO.out_int(if new Bool then 1 else 0 fi)",
                  newTestIO("bool", new Collection, new LinkedList.add(0)));

            interpretExpr("int", "new IO.out_int(new Int)",
                  newTestIO("int", new Collection, new LinkedList.add(0)));

            interpretExpr("string", "new IO.out_string(new String)",
                  newTestIO("string", new Collection, new LinkedList.add("")));

            interpret("self", "class Main { a : Int; main() : Object { new IO.out_int(new SELF_TYPE.a()) }; a() : Int { a }; };",
                  newTestIO("self", new Collection, new LinkedList.add(0)));
         }
      else false fi
   };

   testDispatch() : Object {
      if begin("dispatch") then
         {
            interpret("hierarchy", "class Main inherits A { x : Bool; }; class A { main() : Object { new IO.out_int(1) }; };",
                  newTestIO("hierarchy", new Collection, new LinkedList.add(1)));

            interpret("implicit self dispatch",
                  "class Main inherits A { main() : Object { a() }; a() : Object { new IO.out_int(2) }; };"
                  .concat("class A { a() : Object { new IO.out_int(1) }; };"),
                  newTestIO("implicit self dispatch", new Collection, new LinkedList.add(2)));

            interpret("self dispatch",
                  "class Main inherits A { main() : Object { self.a() }; a() : Object { new IO.out_int(2) }; };"
                  .concat("class A { a() : Object { new IO.out_int(1) }; };"),
                  newTestIO("self dispatch", new Collection, new LinkedList.add(2)));

            interpret("static dispatch",
                  "class Main inherits A { main() : Object { self@A.a() }; a() : Object { new IO.out_int(2) }; };"
                  .concat("class A { a() : Object { new IO.out_int(1) }; };"),
                  newTestIO("static dispatch", new Collection, new LinkedList.add(1)));

            interpret("override dispatch",
                  "class Main inherits A { main() : Object { self@A.a() }; a() : Object { new IO.out_int(2) }; };"
                  .concat("class A { a() : Object { new IO.out_int(1) }; };"),
                  newTestIO("override dispatch", new Collection, new LinkedList.add(1)));

            interpret("dispatch order",
                  "class Main inherits IO { main() : Object { out_int(3).a(out_int(2)).b(out_int(1)) };"
                  .concat("a(o : Object) : SELF_TYPE { out_int(4) }; b(o : Object) : SELF_TYPE { out_int(5) }; };"),
                  newTestIO("dispatch order", new Collection, new LinkedList.add(1).add(2).add(3).add(4).add(5)));

            interpret("dispatch void",
                  "class Main { main() : Int { let void : Main in void.void() }; void() : Int { 0 }; };",
                  newTestIO("dispatch void", new Collection, new LinkedList.add("ERROR: 1: Exception: dispatch on void\n")));

            interpret("dispatch arg",
                  "class Main { main() : Object { new IO.out_int(a(1)) }; a(a : Int) : Int { a }; };",
                  newTestIO("dispatch arg", new Collection, new LinkedList.add(1)));
            interpret("dispatch arg 2",
                  "class Main { main() : Object { new IO.out_int(a(1, 2)) }; a(a : Int, b : Int) : Int { b }; };",
                  newTestIO("dispatch arg 2", new Collection, new LinkedList.add(2)));
         }
      else false fi
   };

   testUnary() : Object {
      if begin("unary") then
         {
            interpretExpr("isvoid void", "if isvoid let a : Object in a then new IO.out_int(1) else false fi",
                  newTestIO("isvoid void", new Collection, new LinkedList.add(1)));
            interpretExpr("isvoid int", "if isvoid 0 then false else new IO.out_int(1) fi",
                  newTestIO("isvoid int", new Collection, new LinkedList.add(1)));
            interpretExpr("isvoid bool", "if isvoid false then false else new IO.out_int(1) fi",
                  newTestIO("isvoid bool", new Collection, new LinkedList.add(1)));
            interpretExpr("isvoid string", "if isvoid \"\" then false else new IO.out_int(1) fi",
                  newTestIO("isvoid string", new Collection, new LinkedList.add(1)));
            interpretExpr("isvoid self", "if isvoid self then false else new IO.out_int(1) fi",
                  newTestIO("isvoid self", new Collection, new LinkedList.add(1)));

            interpretExpr("complement", "new IO.out_int(~1)",
                  newTestIO("complement", new Collection, new LinkedList.add(~1)));

            interpretExpr("not false", "if not false then new IO.out_int(1) else false fi",
                  newTestIO("not false", new Collection, new LinkedList.add(1)));
            interpretExpr("not true", "if not true then false else new IO.out_int(1) fi",
                  newTestIO("not true", new Collection, new LinkedList.add(1)));
         }
      else false fi
   };

   testBinary() : Object {
      if begin("binary") then
         {
            interpretExpr("add", "new IO.out_int(1 + 2)",
                  newTestIO("add", new Collection, new LinkedList.add(3)));

            interpretExpr("subtract", "new IO.out_int(3 - 2)",
                  newTestIO("subtract", new Collection, new LinkedList.add(1)));

            interpretExpr("multiply", "new IO.out_int(2 * 3)",
                  newTestIO("multiply", new Collection, new LinkedList.add(6)));

            interpretExpr("divide", "new IO.out_int(6 / 3)",
                  newTestIO("divide", new Collection, new LinkedList.add(2)));

            interpretExpr("divide 0", "new IO.out_int(1 / 0)",
                  newTestIO("divide 0", new Collection, new LinkedList.add("ERROR: 1: Exception: divide by 0\n")));

            interpretExpr("less", "new IO.out_int(if 0 < 1 then 1 else 0 fi)",
                  newTestIO("less", new Collection, new LinkedList.add(1)));
            interpretExpr("less 2", "new IO.out_int(if 0 < 0 then 1 else 0 fi)",
                  newTestIO("less 2", new Collection, new LinkedList.add(0)));
            interpretExpr("less 3", "new IO.out_int(if 1 < 0 then 1 else 0 fi)",
                  newTestIO("less 3", new Collection, new LinkedList.add(0)));

            interpretExpr("less equal", "new IO.out_int(if 0 <= 1 then 1 else 0 fi)",
                  newTestIO("less equal", new Collection, new LinkedList.add(1)));
            interpretExpr("less equal 2", "new IO.out_int(if 0 <= 0 then 1 else 0 fi)",
                  newTestIO("less equal 2", new Collection, new LinkedList.add(1)));
            interpretExpr("less equal 3", "new IO.out_int(if 1 <= 0 then 1 else 0 fi)",
                  newTestIO("less equal 3", new Collection, new LinkedList.add(0)));

            interpretExpr("equal int", "new IO.out_int(if 0 = 0 then 1 else 0 fi)",
                  newTestIO("equal int", new Collection, new LinkedList.add(1)));
            interpretExpr("equal int new", "new IO.out_int(if 0 = new Int then 1 else 0 fi)",
                  newTestIO("equal int new", new Collection, new LinkedList.add(1)));
            interpretExpr("equal int 2", "new IO.out_int(if 0 = 1 then 1 else 0 fi)",
                  newTestIO("equal int 2", new Collection, new LinkedList.add(0)));

            interpretExpr("equal bool", "new IO.out_int(if false = false then 1 else 0 fi)",
                  newTestIO("equal bool", new Collection, new LinkedList.add(1)));
            interpretExpr("equal bool new", "new IO.out_int(if false = new Bool then 1 else 0 fi)",
                  newTestIO("equal bool new", new Collection, new LinkedList.add(1)));
            interpretExpr("equal bool 2", "new IO.out_int(if false = true then 1 else 0 fi)",
                  newTestIO("equal bool 2", new Collection, new LinkedList.add(0)));

            interpretExpr("equal string", "new IO.out_int(if \"a\" = \"a\" then 1 else 0 fi)",
                  newTestIO("equal string", new Collection, new LinkedList.add(1)));
            interpretExpr("equal string new", "new IO.out_int(if \"\" = new String then 1 else 0 fi)",
                  newTestIO("equal string new", new Collection, new LinkedList.add(1)));
            interpretExpr("equal string 2", "new IO.out_int(if \"\" = \"a\" then 1 else 0 fi)",
                  newTestIO("equal string 2", new Collection, new LinkedList.add(0)));

            interpretExpr("equal self", "new IO.out_int(if self = self then 1 else 0 fi)",
                  newTestIO("equal self", new Collection, new LinkedList.add(1)));
            interpretExpr("equal self new", "new IO.out_int(if self = new SELF_TYPE then 1 else 0 fi)",
                  newTestIO("equal self new", new Collection, new LinkedList.add(0)));

            interpretExpr("equal void", "new IO.out_int(if let void : Object in void = void then 1 else 0 fi)",
                  newTestIO("equal void", new Collection, new LinkedList.add(1)));
            interpretExpr("equal void int", "new IO.out_int(if let left : Object, right : Object <- 1 in left = right then 1 else 0 fi)",
                  newTestIO("equal void int", new Collection, new LinkedList.add(0)));
            interpretExpr("equal int void", "new IO.out_int(if let left : Object <- 1, right : Object in left = right then 1 else 0 fi)",
                  newTestIO("equal int void", new Collection, new LinkedList.add(0)));
            interpretExpr("equal void bool", "new IO.out_int(if let left : Object, right : Object <- false in left = right then 1 else 0 fi)",
                  newTestIO("equal void bool", new Collection, new LinkedList.add(0)));
            interpretExpr("equal bool void", "new IO.out_int(if let left : Object <- false, right : Object in left = right then 1 else 0 fi)",
                  newTestIO("equal bool void", new Collection, new LinkedList.add(0)));
            interpretExpr("equal void string", "new IO.out_int(if let left : Object, right : Object <- \"\" in left = right then 1 else 0 fi)",
                  newTestIO("equal void string", new Collection, new LinkedList.add(0)));
            interpretExpr("equal string void", "new IO.out_int(if let left : Object <- \"\", right : Object in left = right then 1 else 0 fi)",
                  newTestIO("equal string void", new Collection, new LinkedList.add(0)));
            interpretExpr("equal void new", "new IO.out_int(if let left : Object, right : Object <- new Object in left = right then 1 else 0 fi)",
                  newTestIO("equal void new", new Collection, new LinkedList.add(0)));
            interpretExpr("equal new void", "new IO.out_int(if let left : Object <- new Object, right : Object in left = right then 1 else 0 fi)",
                  newTestIO("equal new void", new Collection, new LinkedList.add(0)));

            interpretExpr("equal bool int", "new IO.out_int(if let left : Object <- false, right : Object <- 0 in left = right then 1 else 0 fi)",
                  newTestIO("equal bool int", new Collection, new LinkedList.add(0)));
            interpretExpr("equal int bool", "new IO.out_int(if let left : Object <- 0, right : Object <- false in left = right then 1 else 0 fi)",
                  newTestIO("equal int bool", new Collection, new LinkedList.add(0)));
         }
      else false fi
   };

   testBasicClasses() : Object {
      if begin("basicClasses") then
         {
            interpretExpr("self abort", "abort()",
                  newTestIO("self abort", new Collection, new LinkedList.add("abort\n")));
            interpretExpr("bool abort", "false.abort()",
                  newTestIO("bool abort", new Collection, new LinkedList.add("abort\n")));
            interpretExpr("int abort", "0.abort()",
                  newTestIO("int abort", new Collection, new LinkedList.add("abort\n")));
            interpretExpr("string abort", "\"\".abort()",
                  newTestIO("string abort", new Collection, new LinkedList.add("abort\n")));
            interpretExpr("object abort", "new Object.abort()",
                  newTestIO("object abort", new Collection, new LinkedList.add("abort\n")));

            interpretExpr("self type_name", "new IO.out_string(type_name())",
                  newTestIO("bool type_name", new Collection, new LinkedList.add("Main")));
            interpretExpr("bool type_name", "new IO.out_string(false.type_name())",
                  newTestIO("bool type_name", new Collection, new LinkedList.add("Bool")));
            interpretExpr("int type_name", "new IO.out_string(0.type_name())",
                  newTestIO("int type_name", new Collection, new LinkedList.add("Int")));
            interpretExpr("string type_name", "new IO.out_string(\"\".type_name())",
                  newTestIO("string type_name", new Collection, new LinkedList.add("String")));
            interpretExpr("object type_name", "new IO.out_string(new Object.type_name())",
                  newTestIO("object type_name", new Collection, new LinkedList.add("Object")));

            interpretExpr("bool copy", "new IO.out_int(if not false.copy() then 1 else 0 fi)",
                  newTestIO("bool copy", new Collection, new LinkedList.add(1)));
            interpretExpr("bool copy 2", "new IO.out_int(if not true.copy() then 1 else 0 fi)",
                  newTestIO("bool copy 2", new Collection, new LinkedList.add(0)));
            interpretExpr("int copy", "new IO.out_int(0.copy())",
                  newTestIO("int copy", new Collection, new LinkedList.add(0)));
            interpretExpr("int copy 2", "new IO.out_int(1.copy())",
                  newTestIO("int copy 2", new Collection, new LinkedList.add(1)));
            interpretExpr("string copy", "new IO.out_string(\"\".copy())",
                  newTestIO("string copy", new Collection, new LinkedList.add("")));
            interpretExpr("string copy 2", "new IO.out_string(\"a\".copy())",
                  newTestIO("string copy 2", new Collection, new LinkedList.add("a")));
            interpretExpr("object copy", "new IO.out_string(new Object.copy().type_name())",
                  newTestIO("object copy", new Collection, new LinkedList.add("Object")));
            interpret("attribute copy",
                  "class Main { a : A <- new A; b : Int; a() : Int { a.a() + b };"
                  .concat("main() : Object { let c : Main <- copy() in { a.a(); b <- 4; new IO.out_int(c.a()); } }; };")
                  .concat("class A { a : Int; a() : Int { a <- a + 1 }; };"),
                  newTestIO("attribute copy", new Collection, new LinkedList.add(2)));

            interpretExpr("in_string", "new IO.out_string(new IO.in_string())",
                  newTestIO("in_string", new LinkedList.add("a"), new LinkedList.add("a")));

            interpretExpr("in_int", "new IO.out_int(new IO.in_int())",
                  newTestIO("in_int", new LinkedList.add(123), new LinkedList.add(123)));

            interpretExpr("length 0", "new IO.out_int(\"\".length())",
                  newTestIO("length 0", new Collection, new LinkedList.add(0)));
            interpretExpr("length 1", "new IO.out_int(\"a\".length())",
                  newTestIO("length 1", new Collection, new LinkedList.add(1)));

            interpretExpr("concat 0 0", "new IO.out_string(\"\".concat(\"\"))",
                  newTestIO("concat 0 0", new Collection, new LinkedList.add("")));
            interpretExpr("concat 1 0", "new IO.out_string(\"a\".concat(\"\"))",
                  newTestIO("concat 1 0", new Collection, new LinkedList.add("a")));
            interpretExpr("concat 0 1", "new IO.out_string(\"\".concat(\"a\"))",
                  newTestIO("concat 0 1", new Collection, new LinkedList.add("a")));
            interpretExpr("concat 1 1", "new IO.out_string(\"a\".concat(\"b\"))",
                  newTestIO("concat 1 1", new Collection, new LinkedList.add("ab")));

            interpretExpr("substr begin low", "\"\".substr(~1, 0)",
                  newTestIO("substr begin low", new Collection, new LinkedList.add("ERROR: 0: Exception: String.substr out of range\n")));
            interpretExpr("substr begin high", "\"\".substr(1, 0)",
                  newTestIO("substr begin high", new Collection, new LinkedList.add("ERROR: 0: Exception: String.substr out of range\n")));
            interpretExpr("substr length low", "\"\".substr(0, ~1)",
                  newTestIO("substr length low", new Collection, new LinkedList.add("ERROR: 0: Exception: String.substr out of range\n")));
            interpretExpr("substr length high", "\"\".substr(0, 1)",
                  newTestIO("substr length high", new Collection, new LinkedList.add("ERROR: 0: Exception: String.substr out of range\n")));

            interpretExpr("substr 0 0 0", "new IO.out_string(\"\".substr(0, 0))",
                  newTestIO("substr 0 0 0", new Collection, new LinkedList.add("")));
            interpretExpr("substr 1 0 1", "new IO.out_string(\"a\".substr(0, 1))",
                  newTestIO("substr 1 0 1", new Collection, new LinkedList.add("a")));
            interpretExpr("substr 1 1 0", "new IO.out_string(\"a\".substr(1, 0))",
                  newTestIO("substr 1 1 0", new Collection, new LinkedList.add("")));
            interpretExpr("substr 2 0 1", "new IO.out_string(\"ab\".substr(0, 1))",
                  newTestIO("substr 2 0 1", new Collection, new LinkedList.add("a")));
            interpretExpr("substr 2 1 1", "new IO.out_string(\"ab\".substr(1, 1))",
                  newTestIO("substr 2 1 1", new Collection, new LinkedList.add("b")));
            interpretExpr("substr 2 0 2", "new IO.out_string(\"ab\".substr(0, 2))",
                  newTestIO("substr 2 0 2", new Collection, new LinkedList.add("ab")));
         }
      else false fi
   };
};
