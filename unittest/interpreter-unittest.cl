class Main inherits Test {
   test() : Object {{
      testConstant();
      testDispatch();
   }};

   interpret(context : String, program : String) : InterpreterValue {
      let tokenizer : Tokenizer <- new Tokenizer.init(new TestStringInputStream.init(program)),
            parser : Parser <- new Parser.init(tokenizer),
            program : ParsedProgram <- parser.parse() in
         {
            assertNotVoid(context.concat(" parse"), program);
            let analyzer : Analyzer <- new Analyzer.init(tokenizer.lineMap()),
                  program : AnalyzedProgram <- analyzer.analyze(program) in
               {
                  assertNotVoid(context.concat(" analyze"), program);
                  new InterpreterAnalyzer.analyze(program).interpret();
               };
         }
   };

   interpretExpr(context : String, program : String) : InterpreterValue {
      interpret(context, "class Main { main() : Object { ".concat(program).concat(" }; };"))
   };

   getError(context : String, value : InterpreterValue) : String {{
      assertVoid(context.concat(" type"), value.type());
      case value of x : InterpreterErrorValue => x.value(); esac;
   }};

   interpretError(context : String, program : String) : String {
      getError(context, interpret(context, program))
   };

   interpretErrorExpr(context : String, program : String) : String {
      getError(context, interpretExpr(context, program))
   };

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

            assertStringEquals("dispatch void",
                  "dispatch on void for method 'main' in type 'Main'",
                  interpretError("void dispatch", "class Main { a : Main; main() : Object { a.main() }; };"));
         }
      else false fi
   };
};
