class Main inherits Test {
   test() : Object {{
      testConstant();
      testBlock();
      testIf();
      testWhile();
      testLet();
      testCase();
      testAssignment();
      testNew();
      testInitialization();
      testDispatch();
      testStackOverflow();
      testUnary();
      testBinary();
      testBasicClasses();
   }};

   interpretIOImpl(context : String, io : TestIO, program : String, uva : Bool) : InterpreterValue {
      let tokenizer : Tokenizer <- new Tokenizer.init(new TestStringInputStream.init(program)).setUva(uva),
            parser : TestFailErrorParser <- new TestFailErrorParser.init(tokenizer).initTest(self, context),
            program : ParsedProgram <- parser.parse(),
            lineMap : TokenizerLineMap <- tokenizer.lineMap(),
            analyzer : Analyzer <- new TestFailErrorAnalyzer.initTest(self, context).setUva(uva),
            program : AnalyzedProgram <- analyzer.analyze(program),
            interpreterAnalyzer : InterpreterAnalyzer <- new InterpreterAnalyzer.setUva(uva),
            interpreter : Interpreter <- new Interpreter.init(lineMap, io, true),
            value : InterpreterValue <- interpreter.interpret(interpreterAnalyzer.analyze(program)) in
         {
            io.assert();
            value;
         }
   };

   interpretIO(context : String, io : TestIO, program : String) : InterpreterValue {
      interpretIOImpl(context, io, program, false)
   };

   interpretImpl(context : String, program : String, uva : Bool) : InterpreterValue {
      let empty : Collection <- new Collection,
            io : TestIO <- new TestIO.init(self, context, empty, empty) in
         interpretIOImpl(context, io, program, uva)
   };

   interpret(context : String, program : String) : InterpreterValue {
      interpretImpl(context, program, false)
   };

   interpretUva(context : String, program : String) : InterpreterValue {
      interpretImpl(context, program, true)
   };

   interpretExpr(context : String, program : String) : InterpreterValue {
      interpret(context, "class Main { main() : Object { ".concat(program).concat(" }; };"))
   };

   interpretUvaExpr(context : String, program : String) : InterpreterValue {
      interpretUva(context, "class Main { main() : Object { ".concat(program).concat(" }; };"))
   };

   getError(context : String, value : InterpreterValue) : InterpreterErrorValue {{
      assertVoid(context.concat(" type"), value.type());
      case value of x : InterpreterErrorValue => x; esac;
   }};

   interpretError(context : String, program : String) : InterpreterErrorValue {
      getError(context, interpret(context, program))
   };

   interpretErrorUva(context : String, program : String) : InterpreterErrorValue {
      getError(context, interpretUva(context, program))
   };

   interpretErrorExpr(context : String, program : String) : InterpreterErrorValue {
      getError(context, interpretExpr(context, program))
   };

   interpretErrorUvaExpr(context : String, program : String) : InterpreterErrorValue {
      getError(context, interpretUvaExpr(context, program))
   };

   assertErrorEquals(context : String, message : String, stack : String, error : InterpreterErrorValue) : Object {{
      assertStringEquals(context.concat(" message"), message, error.value());
      assertStringEquals(context.concat(" stack"), stack, error.stack());
   }};

   assertType(context : String, name : String, value : InterpreterValue) : Object {
      let type : InterpreterType <- value.type() in
         if isvoid type then
            failContext(context.concat(" error"), case value of x : InterpreterErrorValue => x.value(); esac)
         else
            assertStringEquals(context.concat(" type"), name, type.name())
         fi
   };

   getBool(context : String, value : InterpreterValue) : Bool {{
      assertType(context, "Bool", value);
      case value of x : InterpreterBoolValue => x.value(); esac;
   }};

   interpretBool(context : String, program : String) : Bool {
      getBool(context, interpret(context, program))
   };

   interpretBoolExpr(context : String, program : String) : Bool {
      getBool(context, interpretExpr(context, program))
   };

   interpretBoolUvaExpr(context : String, program : String) : Bool {
      getBool(context, interpretUvaExpr(context, program))
   };

   getInt(context : String, value : InterpreterValue) : Int {{
      assertType(context, "Int", value);
      case value of x : InterpreterIntValue => x.value(); esac;
   }};

   interpretInt(context : String, program : String) : Int {
      getInt(context, interpret(context, program))
   };

   interpretIntUva(context : String, program : String) : Int {
      getInt(context, interpretUva(context, program))
   };

   interpretIntExpr(context : String, program : String) : Int {
      getInt(context, interpretExpr(context, program))
   };

   interpretIntUvaExpr(context : String, program : String) : Int {
      getInt(context, interpretUvaExpr(context, program))
   };

   getString(context : String, value : InterpreterValue) : String {{
      assertType(context, "String", value);
      case value of x : InterpreterStringValue => x.value(); esac;
   }};

   interpretString(context : String, program : String) : String {
      getString(context, interpret(context, program))
   };

   interpretStringExpr(context : String, program : String) : String {
      getString(context, interpretExpr(context, program))
   };

   interpretStringUvaExpr(context : String, program : String) : String {
      getString(context, interpretUvaExpr(context, program))
   };

   getObject(context : String, type : String, value : InterpreterValue) : InterpreterObjectValue {{
      assertType(context, type, value);
      case value of x : InterpreterObjectValue => x; esac;
   }};

   interpretObject(context : String, type : String, program : String) : InterpreterObjectValue {
      getObject(context, type, interpret(context, program))
   };

   interpretObjectExpr(context : String, type : String, program : String) : InterpreterObjectValue {
      getObject(context, type, interpretExpr(context, program))
   };

   testConstant() : Object {
      if begin("constant") then
         {
            assertFalse("false", interpretBoolExpr("false", "false"));
            assertTrue("true", interpretBoolExpr("true", "true"));
            assertIntEquals("int", 1, interpretIntExpr("int", "1"));
            assertStringEquals("string", "a", interpretStringExpr("string", "\"a\""));
         }
      else false fi
   };

   testBlock() : Object {
      if begin("block") then
         {
            assertIntEquals("single", 1, interpretIntExpr("single", "{ 1; }"));
            assertIntEquals("multiple", 1, interpretInt("multiple",
                  "class Main { a : Int; main() : Object {{ a <- 1; a; }}; };"));
         }
      else false fi
   };

   testIf() : Object {
      if begin("if") then
         {
            assertIntEquals("then", 1, interpretIntExpr("then", "if true then 1 else 0 fi"));
            assertIntEquals("else", 1, interpretIntExpr("else", "if false then 0 else 1 fi"));
         }
      else false fi
   };

   testWhile() : Object {
      if begin("while") then
         {
            assertVoid("while 0", interpretExpr("while", "while false loop true pool"));
            assertVoid("while 1", interpret("while 1",
                  "class Main { a : Int; main() : Object { while a < 1 loop a <- 1 pool }; };"));
            assertIntEquals("while 1 var", 1, interpretInt("while 1 var",
                  "class Main { a : Int; main() : Object {{ while a < 1 loop a <- 1 pool; a; }}; };"));
            assertVoid("while 2", interpret("while 2",
                  "class Main { a : Int; main() : Object { while a < 2 loop a <- a + 1 pool }; };"));
            assertIntEquals("while 2 var", 2, interpretInt("while 2 var",
                  "class Main { a : Int; main() : Object {{ while a < 2 loop a <- a + 1 pool; a; }}; };"));
         }
      else false fi
   };

   testLet() : Object {
      if begin("let") then
         {
            assertIntEquals("let", 1, interpretIntExpr("let", "let a : Int in 1"));

            assertBoolEquals("let bool default", false, interpretBoolExpr("let", "let a : Bool in a"));
            assertIntEquals("let int default", 0, interpretIntExpr("let", "let a : Int in a"));
            assertStringEquals("let string default", "", interpretStringExpr("let", "let a : String in a"));

            assertBoolEquals("bool initialization", true, interpretBoolExpr("let", "let a : Bool <- true in a"));
            assertIntEquals("int initialization", 1, interpretIntExpr("let", "let a : Int <- 1 in a"));
            assertStringEquals("string initialization", "", interpretStringExpr("let", "let a : String <- \"\" in a"));

            assertBoolEquals("bool nested", true, interpretBoolExpr("let", "let a : Bool <- true in let a : Bool <- a in a"));
            assertIntEquals("int nested", 1, interpretIntExpr("let", "let a : Int <- 1 in let a : Int <- a in a"));
            assertStringEquals("string nested", "", interpretStringExpr("let", "let a : String <- \"\" in let a : String <- a in a"));

            assertIntEquals("reuse index", 1, interpretIntExpr("reuse index",
                  "{ let a : Int <- 1 in 0; let a : Int in a; }"));

            assertIntEquals("dispatch", 3, interpretInt("dispatch",
                  "class Main { main() : Object { let a : Int <- 1 in a() + a }; a() : Int { 2 }; };"));
            assertIntEquals("dispatch let", 3, interpretInt("dispatch",
                  "class Main { main() : Object { let a : Int <- 1 in a() + a };"
                  .concat("a() : Int { let a : Int <- 2 in a }; };")));
         }
      else false fi
   };

   testCase() : Object {
      if begin("case") then
         {
            assertErrorEquals("void",
                  "case on void",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("void", "case let a : Object in a of x : Int => 0; esac"));
            assertErrorEquals("uva void",
                  "case on void", "1",
                  interpretErrorUvaExpr("void", "case let a : Object in a of x : Int => 0; esac"));

            assertErrorEquals("unmatched",
                  "case branch not matched for type 'Int'",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("unmatched", "case 0 of x : Bool => x; esac"));
            assertErrorEquals("uva unmatched",
                  "case branch not matched for type 'Int'", "1",
                  interpretErrorUvaExpr("unmatched", "case 0 of x : Bool => x; esac"));

            assertIntEquals("single", 3, interpretIntExpr("single", "case 1 of x : Int => x + 2; esac"));
            assertIntEquals("unrelated", 1, interpretIntExpr("unrelated 2",
                  "case 0 of x : Int => 1; x : String => 0; esac"));
            assertIntEquals("unrelated 2", 1, interpretIntExpr("unrelated",
                  "case 0 of x : String => 0; x : Int => 1; esac"));
            assertIntEquals("ordered", 1, interpretIntExpr("ordered",
                  "case 0 of x : Int => 1; x : Object => 0; esac"));
            assertIntEquals("unordered", 1, interpretIntExpr("unordered",
                  "case 0 of x : Object => 0; x : Int => 1; esac"));
         }
      else false fi
   };

   testAssignment() : Object {
      if begin("assignment") then
         {
            assertIntEquals("attribute", 1, interpretInt("attribute",
                  "class Main { a : Int; main() : Object { a(a <- 1) }; a(x : Object) : Int { a }; };"));
            assertIntEquals("attribute value", 1, interpretInt("attribute value",
                  "class Main { a : Int; main() : Object { a <- 1 }; };"));

            assertIntEquals("var", 1, interpretIntExpr("var", "let a : Int in { a <- 1; a; }"));
            assertIntEquals("var value", 1, interpretIntExpr("var value", "let a : Int in a <- 1"));

            assertIntEquals("argument", 1, interpretInt("argument",
                  "class Main { main() : Int { a(0) }; a(a : Int) : Int {{ a <- 1; a; }}; };"));
            assertIntEquals("argument value", 1, interpretInt("argument",
                  "class Main { main() : Int { a(0) }; a(a : Int) : Int { a <- 1 }; };"));
         }
      else false fi
   };

   testNew() : Object {
      if begin("new") then
         {
            assertBoolEquals("bool", false, interpretBool("bool",
                  "class Main { main() : Object { new Bool }; };"));

            assertIntEquals("int", 0, interpretInt("int",
                  "class Main { main() : Object { new Int }; };"));

            assertStringEquals("string", "", interpretString("string",
                  "class Main { main() : Object { new String }; };"));

            interpretObject("object", "Object", "class Main { main() : Object { new Object }; };");

            interpretObject("main", "Main", "class Main { main() : Object { new Main }; };");

            interpretObjectExpr("self", "Main", "new SELF_TYPE");
            interpretObject("self inherits", "Main",
                  "class Main inherits A { main() : Object { a() }; };"
                  .concat("class A { a() : SELF_TYPE { new SELF_TYPE }; };"));
         }
      else false fi
   };

   testInitialization() : Object {
      if begin("initialization") then
         {
            assertBoolEquals("bool default", false, interpretBool("bool default",
                  "class Main { a : Bool; main() : Object { a }; };"));

            assertIntEquals("int default", 0, interpretInt("int default",
                  "class Main { a : Int; main() : Object { a }; };"));

            assertStringEquals("string default", "", interpretString("string default",
                  "class Main { a : String; main() : Object { a }; };"));

            assertVoid("object default", interpret("object default",
                  "class Main { a : Object; main() : Object { a }; };"));

            assertBoolEquals("bool", true, interpretBool("bool",
                  "class Main { a : Bool <- true; main() : Object { a }; };"));

            assertIntEquals("int", 1, interpretInt("int",
                  "class Main { a : Int <- 1; main() : Object { a }; };"));

            assertStringEquals("string", "a", interpretString("string",
                  "class Main { a : String <- \"a\"; main() : Object { a }; };"));

            interpretObject("object default", "Object",
                  "class Main { a : Object <- new Object; main() : Object { a }; };");

            assertIntEquals("indirect", 3, interpretInt("indirect",
                  "class A { a : Int <- 1; a() : Int { a }; };"
                  .concat("class Main { a : A <- new A; b : B <- new B; main() : Object { a.a() + b.b() }; };")
                  .concat("class B { b : Int <- 2; b() : Int { b }; };")));

            assertIntEquals("inherits", 1, interpretInt("inherits",
                  "class Main inherits A { main() : Object { a }; }; class A { a : Int <- 1; };"));

            assertIntEquals("self", 0, interpretInt("self",
                  "class Main { a : Int; main() : Object {{ new A; a; }}; }; class A { a : Int <- 1; };"));

            assertErrorEquals("dispatch void",
                  "dispatch on void for method 'void' in type 'Main'",
                  "\tat Main.b (line 1)\n"
                     .concat("\tat new Main (line 1)\n"),
                  interpretError("void dispatch",
                     "class Main { a : Main <- b(); b() : Main { a.void() }; void() : Main { a }; main() : Object { 0 }; };"));
         }
      else false fi
   };

   testDispatch() : Object {
      if begin("dispatch") then
         {
            assertIntEquals("dispatch", 1, interpretInt("dispatch",
                  "class Main inherits A { x : Bool; }; class A { main() : Int { 1 }; };"));

            assertIntEquals("implicit self dispatch", 2, interpretInt("implicit self dispatch",
                  "class Main inherits A { main() : Int { a() }; a() : Int { 2 }; };"
                  .concat("class A { a() : Int { 1 }; };")));

            assertIntEquals("self dispatch", 2, interpretInt("self dispatch",
                  "class Main inherits A { main() : Int { self.a() }; a() : Int { 2 }; };"
                  .concat("class A { a() : Int { 1 }; };")));

            assertIntEquals("static dispatch", 1, interpretInt("static dispatch",
                  "class Main inherits A { main() : Int { self@A.a() }; a() : Int { 2 }; };"
                  .concat("class A { a() : Int { 1 }; };")));

            assertIntEquals("override dispatch", 2, interpretInt("dispatch",
                  "class Main inherits A { main() : Int { a() }; b() : Int { 2 }; };"
                  .concat("class A { a() : Int { b() }; b() : Int { 1 }; };")));

            let io : TestIO <- new TestIO.init(self, "dispatch order", new Collection, new LinkedList.add(1).add(2).add(3).add(4).add(5)) in
                  interpretIO("dispatch order", io,
                        "class Main inherits IO { main() : Object { out_int(3).a(out_int(2)).b(out_int(1)) };"
                        .concat("a(o : Object) : SELF_TYPE { out_int(4) }; b(o : Object) : SELF_TYPE { out_int(5) }; };"));

            assertErrorEquals("dispatch void",
                  "dispatch on void for method 'void' in type 'Main'",
                  "\tat Main.a (line 1)\n"
                     .concat("\tat Main.main (line 1)\n"),
                  interpretError("void dispatch",
                     "class Main { a : Main; main() : Int { a() }; a() : Int { a.void() }; void() : Int { 0 }; };"));

            assertErrorEquals("uva dispatch void",
                  "dispatch on void for method 'void' in type 'Main'", "1",
                  interpretErrorUva("void dispatch",
                     "class Main { a : Main; main() : Int { a() }; a() : Int { a.void() }; void() : Int { 0 }; };"));

            assertIntEquals("dispatch arg", 1, interpretInt("dispatch",
                  "class Main { main() : Int { a(1) }; a(a : Int) : Int { a }; };"));
            assertVoid("dispatch arg void", interpret("dispatch arg void",
                  "class Main { a : Object; main() : Object { a(a) }; a(a : Object) : Object { a }; };"));
            assertIntEquals("dispatch args", 3, interpretInt("dispatch",
                  "class Main { main() : Int { a(1, 2) }; a(a : Int, b : Int) : Int { a + b }; };"));
            assertIntEquals("dispatch nested", 3, interpretInt("dispatch",
                  "class Main { main() : Int { a(1) }; a(a : Int) : Int { b(2) + a }; b(b : Int) : Int { b }; };"));
         }
      else false fi
   };

   testStackOverflow() : Object {
      if begin("stackOverflow") then
         {
            assertIntEquals("dispatch", 0, interpretInt("dispatch",
                  "class Main { main() : Object { a(2) }; a(a : Int) : Object { if a + 1 < 1000 then a(a + 1) else 0 fi }; };"));
            assertIntEquals("uva dispatch", 0, interpretIntUva("uva dispatch",
                  "class Main { main() : Object { a(2) }; a(a : Int) : Object { if a + 1 < 1000 then a(a + 1) else 0 fi }; };"));

            assertIntEquals("dispatch overflow", 0, interpretInt("dispatch overflow",
                  "class Main { main() : Object { a(1) }; a(a : Int) : Object { if a + 1 < 1000 then a(a + 1) else 0 fi }; };"));
            assertErrorEquals("uva dispatch overflow",
                  "stack overflow", "1",
                  interpretErrorUva("uva dispatch overflow",
                     "class Main { main() : Object { a(1) }; a(a : Int) : Object { if a + 1 < 1000 then a(a + 1) else 0 fi }; };"));

            assertIntEquals("basic dispatch", 0, interpretInt("basic dispatch",
                  "class Main { main() : Object { a(3) }; a(a : Int) : Object { if a + 1 < 1000 then a(a + 1) else \"\".length() fi }; };"));
            assertIntEquals("uva basic dispatch", 0, interpretIntUva("uva basic dispatch",
                  "class Main { main() : Object { a(3) }; a(a : Int) : Object { if a + 1 < 1000 then a(a + 1) else \"\".length() fi }; };"));

            assertIntEquals("basic dispatch overflow", 0, interpretInt("basic dispatch overflow",
                  "class Main { main() : Object { a(2) }; a(a : Int) : Object { if a + 1 < 1000 then a(a + 1) else \"\".length() fi }; };"));
            assertErrorEquals("uva basic dispatch overflow",
                  "stack overflow", "1",
                  interpretErrorUva("uva basic dispatch overflow",
                     "class Main { main() : Object { a(2) }; a(a : Int) : Object { if a + 1 < 1000 then a(a + 1) else \"\".length() fi }; };"));

            assertType("new", "A", interpret("new",
                  "class Main { main() : Object { new A }; };"
                  .concat("class A { a : Object <- a(3); a(a : Int) : Object { if a + 2 < 1000 then a(a + 1) else new B fi }; };")
                  .concat("class B { b : Int <- 1; };")));
            assertType("uva new", "A", interpretUva("uva new",
                  "class Main { main() : Object { new A }; };"
                  .concat("class A { a : Object <- a(3); a(a : Int) : Object { if a + 2 < 1000 then a(a + 1) else new B fi }; };")
                  .concat("class B { b : Int <- 1; };")));

            assertType("new overflow", "A", interpret("new overflow",
                  "class Main { main() : Object { new A }; };"
                  .concat("class A { a : Object <- a(2); a(a : Int) : Object { if a + 2 < 1000 then a(a + 1) else new B fi }; };")
                  .concat("class B { b : Int <- 1; };")));
            assertErrorEquals("uva new overflow",
                  "stack overflow", "1",
                  interpretErrorUva("uva new overflow",
                     "class Main { main() : Object { new A }; };"
                     .concat("class A { a : Object <- a(2); a(a : Int) : Object { if a + 2 < 1000 then a(a + 1) else new B fi }; };")
                     .concat("class B { b : Int <- 1; };")));

            assertType("simple new", "A", interpret("simple new",
                  "class Main { main() : Object { new A }; };"
                  .concat("class A { a : Object <- a(3); a(a : Int) : Object { if a + 2 < 1000 then a(a + 1) else new B fi }; };")
                  .concat("class B { b : Int; };")));
            assertType("uva simple new", "A", interpretUva("uva simple new",
                  "class Main { main() : Object { new A }; };"
                  .concat("class A { a : Object <- a(3); a(a : Int) : Object { if a + 2 < 1000 then a(a + 1) else new B fi }; };")
                  .concat("class B { b : Int; };")));

            assertType("simple new overflow", "A", interpret("simple new overflow",
                  "class Main { main() : Object { new A }; };"
                  .concat("class A { a : Object <- a(2); a(a : Int) : Object { if a + 2 < 1000 then a(a + 1) else new B fi }; };")
                  .concat("class B { b : Int; };")));
            assertErrorEquals("uva simple new overflow",
                  "stack overflow", "1",
                  interpretErrorUva("uva simple new overflow",
                     "class Main { main() : Object { new A }; };"
                     .concat("class A { a : Object <- a(2); a(a : Int) : Object { if a + 2 < 1000 then a(a + 1) else new B fi }; };")
                     .concat("class B { b : Int; };")));
         }
      else false fi
   };

   testUnary() : Object {
      if begin("unary") then
         {
            assertTrue("isvoid void", interpretBoolExpr("isvoid void", "isvoid let a : Object in a"));
            assertFalse("isvoid int", interpretBoolExpr("isvoid int", "isvoid 0"));
            assertFalse("isvoid bool", interpretBoolExpr("isvoid bool", "isvoid false"));
            assertFalse("isvoid string", interpretBoolExpr("isvoid string", "isvoid \"\""));
            assertFalse("isvoid object", interpretBoolExpr("isvoid object", "isvoid self"));

            assertIntEquals("complement", ~1, interpretIntExpr("complement", "~1"));

            assertTrue("not false", interpretBoolExpr("not false", "not false"));
            assertFalse("not true", interpretBoolExpr("not true", "not true"));
         }
      else false fi
   };

   testBinary() : Object {
      if begin("binary") then
         {
            assertIntEquals("add", 3, interpretIntExpr("add", "1 + 2"));
            assertIntEquals("add overflow", ~2147483647 - 1, interpretIntExpr("add overflow", "2147483647 + 1"));

            assertIntEquals("subtract", 1, interpretIntExpr("subtract", "3 - 2"));
            assertIntEquals("subtract underflow", 2147483647, interpretIntExpr("subtract overflow", "~2147483647 - 2"));

            assertIntEquals("multiply", 6, interpretIntExpr("multiply", "2 * 3"));

            assertIntEquals("divide", 2, interpretIntExpr("multiply", "6 / 3"));

            assertErrorEquals("divide 0",
                  "divide by 0",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("divide 0", "1 / 0"));
            assertErrorEquals("uva divide 0",
                  "divide by 0", "1",
                  interpretErrorUvaExpr("divide 0", "1 / 0"));

            assertTrue("less", interpretBoolExpr("less", "0 < 1"));
            assertFalse("less 2", interpretBoolExpr("less 2", "0 < 0"));
            assertFalse("less 3", interpretBoolExpr("less 3", "1 < 0"));

            assertTrue("uva less int", interpretBoolUvaExpr("uva less int", "0 < 1"));
            assertFalse("uva less int 2", interpretBoolUvaExpr("uva less int 2", "0 < 0"));
            assertFalse("uva less int 3", interpretBoolUvaExpr("uva less int 3", "1 < 0"));

            assertFalse("uva less bool", interpretBoolUvaExpr("uva less bool", "false < false"));
            assertTrue("uva less bool 2", interpretBoolUvaExpr("uva less bool 2", "false < true"));
            assertFalse("uva less bool 3", interpretBoolUvaExpr("uva less bool 3", "true < false"));

            assertFalse("uva less string", interpretBoolUvaExpr("uva less bool", "\"a\" < \"a\""));
            assertTrue("uva less string 2", interpretBoolUvaExpr("uva less string 2", "\"\" < \"a\""));
            assertFalse("uva less string 3", interpretBoolUvaExpr("uva less string 3", "\"a\" < \"\""));

            assertFalse("uva less self", interpretBoolUvaExpr("uva less self", "self < self"));
            assertFalse("uva less self new", interpretBoolUvaExpr("uva less self new", "self < new Main"));
            assertFalse("uva less self copy", interpretBoolUvaExpr("uva less self copy", "self < copy()"));

            assertFalse("uva less void", interpretBoolUvaExpr("uva less void", "let void : Object in void < void"));
            assertFalse("uva less void int", interpretBoolUvaExpr("uva less void int",
                  "let left : Object, right : Object <- 0 in left < right"));
            assertFalse("uva less int void", interpretBoolUvaExpr("uva less int void",
                  "let left : Object <- 0, right : Object in left < right"));
            assertFalse("uva less void bool", interpretBoolUvaExpr("uva less void bool",
                  "let left : Object, right : Object <- false in left < right"));
            assertFalse("uva less bool void", interpretBoolUvaExpr("uva less bool void",
                  "let left : Object <- false, right : Object in left < right"));
            assertFalse("uva less void string", interpretBoolUvaExpr("uva less void string",
                  "let left : Object, right : Object <- \"\" in left < right"));
            assertFalse("uva less string void", interpretBoolUvaExpr("uva less string void",
                  "let left : Object <- \"\", right : Object in left < right"));
            assertFalse("uva less void new", interpretBoolUvaExpr("uva less void new",
                  "let left : Object, right : Object <- new Object in left < right"));
            assertFalse("uva less new void", interpretBoolUvaExpr("uva less new void",
                  "let left : Object <- new Object, right : Object in left < right"));

            assertTrue("less equal", interpretBoolExpr("less equal", "0 <= 1"));
            assertTrue("less equal", interpretBoolExpr("less equal", "0 <= 0"));
            assertFalse("less equal", interpretBoolExpr("less equal", "1 <= 0"));

            assertTrue("uva less equal int", interpretBoolUvaExpr("uva less equal int", "0 <= 1"));
            assertTrue("uva less equal int 2", interpretBoolUvaExpr("uva less equal int 2", "0 <= 0"));
            assertFalse("uva less equal int 3", interpretBoolUvaExpr("uva less equal int 3", "1 <= 0"));

            assertTrue("uva less equal bool", interpretBoolUvaExpr("uva less bool", "false <= false"));
            assertTrue("uva less equal bool 2", interpretBoolUvaExpr("uva less bool 2", "false <= true"));
            assertFalse("uva less equal bool 3", interpretBoolUvaExpr("uva less bool 3", "true <= false"));

            assertTrue("uva less equal string", interpretBoolUvaExpr("uva less bool", "\"a\" <= \"a\""));
            assertTrue("uva less equal string 2", interpretBoolUvaExpr("uva less string 2", "\"\" <= \"a\""));
            assertFalse("uva less equal string 3", interpretBoolUvaExpr("uva less string 3", "\"a\" <= \"\""));

            assertTrue("uva less equal self", interpretBoolUvaExpr("uva less self", "self <= self"));
            assertFalse("uva less equal self new", interpretBoolUvaExpr("uva less self new", "self <= new Main"));
            assertFalse("uva less equal self copy", interpretBoolUvaExpr("uva less self copy", "self <= copy()"));

            assertTrue("uva less equal void", interpretBoolUvaExpr("uva less void", "let void : Object in void <= void"));
            assertFalse("uva less equal void int", interpretBoolUvaExpr("uva less void int",
                  "let left : Object, right : Object <- 0 in left < right"));
            assertFalse("uva less equal int void", interpretBoolUvaExpr("uva less int void",
                  "let left : Object <- 0, right : Object in left < right"));
            assertFalse("uva less equal void bool", interpretBoolUvaExpr("uva less void bool",
                  "let left : Object, right : Object <- false in left < right"));
            assertFalse("uva less equal bool void", interpretBoolUvaExpr("uva less bool void",
                  "let left : Object <- false, right : Object in left < right"));
            assertFalse("uva less equal void string", interpretBoolUvaExpr("uva less void string",
                  "let left : Object, right : Object <- \"\" in left < right"));
            assertFalse("uva less equal string void", interpretBoolUvaExpr("uva less string void",
                  "let left : Object <- \"\", right : Object in left < right"));
            assertFalse("uva less equal void new", interpretBoolUvaExpr("uva less void new",
                  "let left : Object, right : Object <- new Object in left < right"));
            assertFalse("uva less equal new void", interpretBoolUvaExpr("uva less new void",
                  "let left : Object <- new Object, right : Object in left < right"));

            assertTrue("equal int", interpretBoolExpr("equal int", "0 = 0"));
            assertTrue("equal int new", interpretBoolExpr("equal int new", "0 = new Int"));
            assertFalse("equal int 2", interpretBoolExpr("equal int 2", "0 = 1"));

            assertTrue("equal bool", interpretBoolExpr("equal bool", "false = false"));
            assertTrue("equal bool new", interpretBoolExpr("equal bool new", "false = new Bool"));
            assertFalse("equal bool 2", interpretBoolExpr("equal bool 2", "false = true"));

            assertTrue("equal string", interpretBoolExpr("equal bool", "\"a\" = \"a\""));
            assertTrue("equal string new", interpretBoolExpr("equal string new", "\"\" = new String"));
            assertFalse("equal string 2", interpretBoolExpr("equal string 2", "\"\" = \"a\""));

            assertTrue("equal self", interpretBoolExpr("equal self", "self = self"));
            assertFalse("equal self new", interpretBoolExpr("equal self new", "self = new Main"));
            assertFalse("equal self copy", interpretBoolExpr("equal self copy", "self = copy()"));

            assertTrue("equal void", interpretBoolExpr("equal void", "let void : Object in void = void"));
            assertFalse("equal void int", interpretBoolExpr("equal void int",
                  "let left : Object, right : Object <- 0 in left = right"));
            assertFalse("equal int void", interpretBoolExpr("equal int void",
                  "let left : Object <- 0, right : Object in left = right"));
            assertFalse("equal void bool", interpretBoolExpr("equal void bool",
                  "let left : Object, right : Object <- false in left = right"));
            assertFalse("equal bool void", interpretBoolExpr("equal bool void",
                  "let left : Object <- false, right : Object in left = right"));
            assertFalse("equal void string", interpretBoolExpr("equal void string",
                  "let left : Object, right : Object <- \"\" in left = right"));
            assertFalse("equal string void", interpretBoolExpr("equal string void",
                  "let left : Object <- \"\", right : Object in left = right"));
            assertFalse("equal void new", interpretBoolExpr("equal void new",
                  "let left : Object, right : Object <- new Object in left = right"));
            assertFalse("equal new void", interpretBoolExpr("equal new void",
                  "let left : Object <- new Object, right : Object in left = right"));
         }
      else false fi
   };

   testBasicClasses() : Object {
      if begin("basicClasses") then
         {
            assertErrorEquals("self abort",
                  "abort",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("self abort", "abort()"));

            assertErrorEquals("uva self abort",
                  "abort", "",
                  interpretErrorUvaExpr("uva self abort", "abort()"));

            assertErrorEquals("bool abort",
                  "abort",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("bool abort", "false.abort()"));

            assertErrorEquals("int abort",
                  "abort",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("int abort", "0.abort()"));

            assertErrorEquals("string abort",
                  "abort",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("string abort", "\"\".abort()"));

            assertErrorEquals("object abort",
                  "abort",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("object abort", "new Object.abort()"));

            assertStringEquals("bool type_name", "Bool", interpretStringExpr("bool type_name", "false.type_name()"));
            assertStringEquals("int type_name", "Int", interpretStringExpr("int type_name", "0.type_name()"));
            assertStringEquals("string type_name", "String", interpretStringExpr("string type_name", "\"\".type_name()"));
            assertStringEquals("object type_name", "Object", interpretStringExpr("object type_name", "new Object.type_name()"));
            assertStringEquals("main type_name", "Main", interpretStringExpr("main type_name", "type_name()"));

            assertFalse("false copy", interpretBoolExpr("bool copy", "false.copy()"));
            assertTrue("true copy", interpretBoolExpr("bool copy", "true.copy()"));
            assertIntEquals("int copy", 1, interpretIntExpr("int copy", "1.copy()"));
            assertStringEquals("string copy", "a", interpretStringExpr("string copy", "\"a\".copy()"));
            interpretObjectExpr("object copy", "Object", "new Object.copy()");
            assertIntEquals("main copy", 2, interpretInt("main copy",
                  "class Main {\na : A <- new A; b : Int;\na() : Int { a.a() + b };\n"
                  .concat("main() : Object { let c : Main <- copy() in { a.a(); b <- 4; c.a(); } }; };\n")
                  .concat("class A { a : Int; a() : Int { a <- a + 1 }; };")));

            interpretObjectExpr("main copy", "Main", "copy()");

            let io : TestIO <- new TestIO.init(self, "out_string", new Collection, new LinkedList.add("a")) in
               getObject("out_string", "IO",
                     interpretIO("out_string", io, "class Main { main() : Object { new IO.out_string(\"a\") }; };"));

            let out : Collection <-
                     if "\\".length() = 2 then
                        new LinkedList.add("a".concat(stringUtil.backslash())).add("b\nc")
                     else
                        new LinkedList.add("a\\b\nc")
                     fi,
                  io : TestIO <- new TestIO.init(self, "out_string", new Collection, out) in
               getObject("out_string", "IO",
                     interpretIO("out_string", io, "class Main { main() : Object { new IO.out_string(\"a\\\\b\\nc\") }; };"));
            let out : Collection <- new LinkedList.add("a\\b\nc"),
                  io : TestIO <- new TestIO.init(self, "out_string", new Collection, out) in
               getObject("out_string", "IO",
                     interpretIOImpl("out_string", io, "class Main { main() : Object { new IO.out_string(\"a\\\\b\\nc\") }; };", true));
            let io : TestIO <- new TestIO.init(self, "out_string override", new Collection, new Collection) in
               getObject("out_string override", "Main", interpretIO("out_string override", io,
                     "class Main inherits IO { main() : Object { out_string(\"\") }; out_string(s : String) : SELF_TYPE { self }; };"));

            let io : TestIO <- new TestIO.init(self, "out_int", new Collection, new LinkedList.add(1)) in
               getObject("out_int", "IO",
                     interpretIO("out_int", io, "class Main { main() : Object { new IO.out_int(1) }; };"));
            let io : TestIO <- new TestIO.init(self, "out_int override", new Collection, new Collection) in
               getObject("out_int override", "Main", interpretIO("out_int override", io,
                     "class Main inherits IO { main() : Object { out_int(0) }; out_int(i : Int) : SELF_TYPE { self }; };"));

            let io : TestIO <- new TestIO.init(self, "in_string", new LinkedList.add("a"), new Collection) in
               assertStringEquals("in_string", "a", getString("in_string",
                     interpretIO("in_string", io, "class Main { main() : Object { new IO.in_string() }; };")));
            let io : TestIO <- new TestIO.init(self, "in_string escape", new LinkedList.add(stringUtil.backslash()), new Collection) in
               assertTrue("in_string", getBool("in_string",
                     interpretIO("in_string", io, "class Main { main() : Object { new IO.in_string() = \"\\\\\" }; };")));
            let io : TestIO <- new TestIO.init(self, "in_string override", new Collection, new Collection) in
               assertStringEquals("in_string override", "", getString("in_string override", interpretIO("in_string override", io,
                     "class Main { main() : Object { in_string() }; in_string() : String { \"\" }; };")));

            let io : TestIO <- new TestIO.init(self, "in_int", new LinkedList.add(1), new Collection) in
               assertIntEquals("in_int", 1, getInt("in_int",
                     interpretIO("in_int", io, "class Main { main() : Object { new IO.in_int() }; };")));
            let io : TestIO <- new TestIO.init(self, "in_int override", new Collection, new Collection) in
               assertIntEquals("in_int override", 0, getInt("in_int override", interpretIO("in_int override", io,
                     "class Main { main() : Object { in_int() }; in_int() : Int { 0 }; };")));

            assertIntEquals("length 0", 0, interpretIntExpr("length 0", "\"\".length()"));
            assertIntEquals("length 1", 1, interpretIntExpr("length 1", "\"a\".length()"));
            assertIntEquals("length special", 7, interpretIntExpr("length linefeed", "\"a\\nb\\tc\\\\d\".length()"));
            assertIntEquals("length concat special", 2, interpretIntExpr("length linefeed", "\"\\n\".concat(\"\\n\").length()"));

            assertIntEquals("uva length 0", 0, interpretIntUvaExpr("length 0", "\"\".length()"));
            assertIntEquals("uva length 1", 1, interpretIntUvaExpr("length 1", "\"a\".length()"));
            assertIntEquals("uva length special", 10, interpretIntUvaExpr("length linefeed", "\"a\\nb\\tc\\\\d\".length()"));
            assertIntEquals("uva length concat special", 4, interpretIntUvaExpr("length linefeed", "\"\\n\".concat(\"\\n\").length()"));

            assertStringEquals("concat 0 0", "", interpretStringExpr("concat 0 0", "\"\".concat(\"\")"));
            assertStringEquals("concat 1 0", "a", interpretStringExpr("concat 1 0", "\"a\".concat(\"\")"));
            assertStringEquals("concat 0 1", "a", interpretStringExpr("concat 0 1", "\"\".concat(\"a\")"));
            assertStringEquals("concat 1 1", "ab", interpretStringExpr("concat 0 1", "\"a\".concat(\"b\")"));

            assertErrorEquals("substr begin low",
                  "substr(-1, 0) is out of range for string of length 0",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("substr begin low", "\"\".substr(~1, 0)"));
            assertErrorEquals("substr begin high",
                  "substr(1, 0) is out of range for string of length 0",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("substr begin high", "\"\".substr(1, 0)"));
            assertErrorEquals("substr length low",
                  "substr(0, -1) is out of range for string of length 0",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("substr length low", "\"\".substr(0, ~1)"));
            assertErrorEquals("substr length high",
                  "substr(0, 1) is out of range for string of length 0",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("substr length high", "\"\".substr(0, 1)"));
            assertErrorEquals("substr escapes begin high",
                  "substr(2, 0) is out of range for string of length 1",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("substr escapes begin high", "\"\\n\".substr(2, 0)"));
            assertErrorEquals("substr escapes length high",
                  "substr(0, 2) is out of range for string of length 1",
                  "\tat Main.main (line 1)\n",
                  interpretErrorExpr("substr escapes begin high", "\"\\n\".substr(0, 2)"));

            assertStringEquals("substr 0 0 0", "", interpretStringExpr("substr 0 0 0", "\"\".substr(0, 0)"));
            assertStringEquals("substr 1 0 1", "a", interpretStringExpr("substr 1 0 1", "\"a\".substr(0, 1)"));
            assertStringEquals("substr 1 1 0", "", interpretStringExpr("substr 1 1 0", "\"a\".substr(1, 0)"));
            assertStringEquals("substr 2 0 1", "a", interpretStringExpr("substr 2 1 1", "\"ab\".substr(0, 1)"));
            assertStringEquals("substr 2 1 1", "b", interpretStringExpr("substr 2 1 1", "\"ab\".substr(1, 1)"));
            assertStringEquals("substr 2 0 2", "ab", interpretStringExpr("substr 2 0 2", "\"ab\".substr(0, 2)"));
            assertStringEquals("substr escapes 0 4", "\b\t\n\f",
                  interpretStringExpr("substr escapes 0 4", "\"\\b\\t\\n\\f\".substr(0, 4)"));
            assertStringEquals("substr escapes 0 2", "\b\t",
                  interpretStringExpr("substr escapes 0 2", "\"\\b\\t\\n\\f\".substr(0, 2)"));
            assertStringEquals("substr escapes 1 2", "\t\n",
                  interpretStringExpr("substr escapes 1 2", "\"\\b\\t\\n\\f\".substr(1, 2)"));
            assertStringEquals("substr escapes 2 2", "\n\f",
                  interpretStringExpr("substr escapes 2 2", "\"\\b\\t\\n\\f\".substr(2, 2)"));
            assertIntEquals("substr escapes 0 4 length", 4,
                  interpretIntExpr("substr escapes 0 4 length", "\"\\b\\t\\n\\f\".substr(0, 4).length()"));
            assertIntEquals("substr escapes 0 2 length", 2,
                  interpretIntExpr("substr escapes 0 2 length", "\"\\b\\t\\n\\f\".substr(0, 2).length()"));
            assertIntEquals("substr escapes 1 2 length", 2,
                  interpretIntExpr("substr escapes 1 2 length", "\"\\b\\t\\n\\f\".substr(1, 2).length()"));
            assertIntEquals("substr escapes 2 2 length", 2,
                  interpretIntExpr("substr escapes 2 2 length", "\"\\b\\t\\n\\f\".substr(2, 2).length()"));

            assertErrorEquals("uva substr begin low",
                  "substr(-1, 0) is out of range for string of length 0", "0",
                  interpretErrorUvaExpr("uva substr begin low", "\"\".substr(~1, 0)"));
            assertErrorEquals("uva substr begin high",
                  "substr(1, 0) is out of range for string of length 0", "0",
                  interpretErrorUvaExpr("uva substr begin high", "\"\".substr(1, 0)"));
            assertErrorEquals("uva substr length low",
                  "substr(0, -1) is out of range for string of length 0", "0",
                  interpretErrorUvaExpr("uva substr length low", "\"\".substr(0, ~1)"));
            assertErrorEquals("uva substr length high",
                  "substr(0, 1) is out of range for string of length 0", "0",
                  interpretErrorUvaExpr("uva substr length high", "\"\".substr(0, 1)"));
            assertErrorEquals("uva substr escapes begin high",
                  "substr(3, 0) is out of range for string of length 2", "0",
                  interpretErrorUvaExpr("uva substr escapes begin high", "\"\\n\".substr(3, 0)"));
            assertErrorEquals("uva substr escapes length high",
                  "substr(0, 3) is out of range for string of length 2", "0",
                  interpretErrorUvaExpr("uva substr escapes begin high", "\"\\n\".substr(0, 3)"));

            assertStringEquals("uva substr 0 0 0", "", interpretStringUvaExpr("uva substr 0 0 0", "\"\".substr(0, 0)"));
            assertStringEquals("uva substr 1 0 1", "a", interpretStringUvaExpr("uva substr 1 0 1", "\"a\".substr(0, 1)"));
            assertStringEquals("uva substr 1 1 0", "", interpretStringUvaExpr("uva substr 1 1 0", "\"a\".substr(1, 0)"));
            assertStringEquals("uva substr 2 0 1", "a", interpretStringUvaExpr("uva substr 2 1 1", "\"ab\".substr(0, 1)"));
            assertStringEquals("uva substr 2 1 1", "b", interpretStringUvaExpr("uva substr 2 1 1", "\"ab\".substr(1, 1)"));
            assertStringEquals("uva substr 2 0 2", "ab", interpretStringUvaExpr("uva substr 2 0 2", "\"ab\".substr(0, 2)"));
            assertStringEquals("uva substr escapes 0 8", "\b\t\n\f",
                  interpretStringUvaExpr("uva substr escapes 0 4", "\"\\b\\t\\n\\f\".substr(0, 8)"));
            assertStringEquals("uva substr escapes 0 4", "\b\t",
                  interpretStringUvaExpr("uva substr escapes 0 2", "\"\\b\\t\\n\\f\".substr(0, 4)"));
            assertStringEquals("uva substr escapes 1 4", "b\t".concat(stringUtil.backslash()),
                  interpretStringUvaExpr("uva substr escapes 1 4", "\"\\b\\t\\n\\f\".substr(1, 4)"));
            assertStringEquals("uva substr escapes 2 4", "\t\n",
                  interpretStringUvaExpr("uva substr escapes 2 4", "\"\\b\\t\\n\\f\".substr(2, 4)"));
            assertIntEquals("uva substr escapes 0 8 length", 8,
                  interpretIntUvaExpr("uva substr escapes 0 8 length", "\"\\b\\t\\n\\f\".substr(0, 8).length()"));
            assertIntEquals("uva substr escapes 0 4 length", 4,
                  interpretIntUvaExpr("uva substr escapes 0 4 length", "\"\\b\\t\\n\\f\".substr(0, 4).length()"));
            assertIntEquals("uva substr escapes 1 4 length", 4,
                  interpretIntUvaExpr("uva substr escapes 1 4 length", "\"\\b\\t\\n\\f\".substr(1, 4).length()"));
            assertIntEquals("uva substr escapes 2 4 length", 4,
                  interpretIntUvaExpr("uva substr escapes 2 4 length", "\"\\b\\t\\n\\f\".substr(2, 4).length()"));
         }
      else false fi
   };
};
