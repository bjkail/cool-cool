class Main inherits Test {
   test() : Object {{
      testConstant();
      testNew();
      testInitialization();
      testDispatch();
      testBasicClasses();
   }};

   interpret(context : String, program : String) : InterpreterValue {
      let tokenizer : Tokenizer <- new Tokenizer.init(new TestStringInputStream.init(program)),
            parser : Parser <- new Parser.init(tokenizer),
            program : ParsedProgram <- parser.parse() in
         {
            assertNotVoid(context.concat(" parse"), program);
            let lineMap : TokenizerLineMap <- tokenizer.lineMap(),
                  analyzer : Analyzer <- new Analyzer.init(lineMap),
                  program : AnalyzedProgram <- analyzer.analyze(program) in
               {
                  assertNotVoid(context.concat(" analyze"), program);
                  new InterpreterAnalyzer.init(lineMap).analyze(program).interpret();
               };
         }
   };

   interpretExpr(context : String, program : String) : InterpreterValue {
      interpret(context, "class Main { main() : Object { ".concat(program).concat(" }; };"))
   };

   getError(context : String, value : InterpreterValue) : InterpreterErrorValue {{
      assertVoid(context.concat(" type"), value.type());
      case value of x : InterpreterErrorValue => x; esac;
   }};

   interpretError(context : String, program : String) : InterpreterErrorValue {
      getError(context, interpret(context, program))
   };

   interpretErrorExpr(context : String, program : String) : InterpreterErrorValue {
      getError(context, interpretExpr(context, program))
   };

   assertErrorEquals(context : String, message : String, stack : String, error : InterpreterErrorValue) : Object {{
      assertStringEquals(context.concat(" message"), message, error.value());
      assertStringEquals(context.concat(" stack"), stack, error.stack());
   }};

   getBool(context : String, value : InterpreterValue) : Bool {{
      assertStringEquals(context.concat(" type"), "Bool", value.type().name());
      case value of x : InterpreterBoolValue => x.value(); esac;
   }};

   interpretBool(context : String, program : String) : Bool {
      getBool(context, interpret(context, program))
   };

   interpretBoolExpr(context : String, program : String) : Bool {
      getBool(context, interpretExpr(context, program))
   };

   getInt(context : String, value : InterpreterValue) : Int {{
      assertStringEquals(context.concat(" type"), "Int", value.type().name());
      case value of x : InterpreterIntValue => x.value(); esac;
   }};

   interpretInt(context : String, program : String) : Int {
      getInt(context, interpret(context, program))
   };

   interpretIntExpr(context : String, program : String) : Int {
      getInt(context, interpretExpr(context, program))
   };

   getString(context : String, value : InterpreterValue) : String {{
      assertStringEquals(context.concat(" type"), "String", value.type().name());
      case value of x : InterpreterStringValue => x.value(); esac;
   }};

   interpretString(context : String, program : String) : String {
      getString(context, interpret(context, program))
   };

   interpretStringExpr(context : String, program : String) : String {
      getString(context, interpretExpr(context, program))
   };

   getObject(context : String, type : String, value : InterpreterValue) : InterpreterObjectValue {{
      assertStringEquals(context.concat(" type"), type, value.type().name());
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

            assertErrorEquals("dispatch void",
                  "dispatch on void for method 'void' in type 'Main'",
                  "\tat Main.a (line 1)\n"
                     .concat("\tat Main.main (line 1)\n"),
                  interpretError("void dispatch",
                     "class Main { a : Main; main() : Int { a() }; a() : Int { a.void() }; void() : Int { 0 }; };"));
         }
      else false fi
   };

   testBasicClasses() : Object {
      if begin("basicClasses") then
         {
            assertErrorEquals("self abort",
                  "abort",
                  "\tat Main.main (line 1)\n",
                  interpretError("self abort", "class Main { main() : Object { abort() }; };"));

            assertErrorEquals("bool abort",
                  "abort",
                  "\tat Main.main (line 1)\n",
                  interpretError("bool abort", "class Main { main() : Object { false.abort() }; };"));

            assertErrorEquals("int abort",
                  "abort",
                  "\tat Main.main (line 1)\n",
                  interpretError("int abort", "class Main { main() : Object { 0.abort() }; };"));

            assertErrorEquals("string abort",
                  "abort",
                  "\tat Main.main (line 1)\n",
                  interpretError("string abort", "class Main { main() : Object { \"\".abort() }; };"));

            assertErrorEquals("object abort",
                  "abort",
                  "\tat Main.main (line 1)\n",
                  interpretError("object abort", "class Main { main() : Object { new Object.abort() }; };"));

            assertStringEquals("bool type_name", "Bool", interpretStringExpr("bool type_name", "false.type_name()"));
            assertStringEquals("int type_name", "Int", interpretStringExpr("int type_name", "0.type_name()"));
            assertStringEquals("string type_name", "String", interpretStringExpr("string type_name", "\"\".type_name()"));
            assertStringEquals("object type_name", "Object", interpretStringExpr("object type_name", "new Object.type_name()"));
            assertStringEquals("main type_name", "Main", interpretStringExpr("main type_name", "type_name()"));
         }
      else false fi
   };
};
