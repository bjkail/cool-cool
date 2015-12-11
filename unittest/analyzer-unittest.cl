class Main inherits Test {
   test() : Object {{
      testRegister();
      testMain();
      testMethod();
      testExpr();
   }};

   assertAnalyzerErrorImpl(context : String, error : String, program : String) : Object {
      let tokenizer : Tokenizer <- new Tokenizer.init(new TestStringInputStream.init(program)),
            parser : Parser <- new Parser.init(tokenizer),
            analyzer : TestAnalyzer <- new TestAnalyzer.init(tokenizer.lineMap()).analyze(parser.parse()) in
         assertStringEquals(context, error, analyzer.errorString())
   };

   assertAnalyzerError(context : String, error : String, program : String) : Object {
      assertAnalyzerErrorImpl(context, error, "class Main { main() : Object { 0 }; }; ".concat(program))
   };

   testRegister() : Object {
      if begin("register") then
         {
            assertAnalyzerError("", "line 1: unexpected redefinition of class 'A'",
                  "class A { a : Bool; }; class A { a : Bool; };");
            assertAnalyzerError("", "line 1: undefined type 'B' for 'inherits'",
                  "class A inherits B { a : Bool; };");
            assertAnalyzerError("", "line 1: hierarchy of class 'A' contains a cycle",
                  "class A inherits B { a : Bool; }; class B inherits A { b : Bool; };");
            assertAnalyzerError("", "line 1: unexpected redefinition of attribute 'a' in class 'A'",
                  "class A { a : Bool; a : Bool; };");
            assertAnalyzerError("", "line 1: unexpected redefinition of attribute 'a' in class 'B'",
                  "class A { a : Bool; }; class B inherits A { a : Bool; };");
            assertAnalyzerError("", "line 1: unexpected redefinition of method 'a' in class 'A'",
                  "class A { a() : Object { 0 }; a() : Object { 0 }; };");
            assertAnalyzerError("", "line 1: unexpected redefinition of method 'a' in class 'B'",
                  "class A { a() : Object { 0 }; }; class B inherits A { a() : Object { 0 }; };");
         }
      else false fi
   };

   testMain() : Object {
      if begin("main") then
         {
            assertAnalyzerErrorImpl("", "expected class 'Main'",
                  "class A { a : Bool; };");
            assertAnalyzerErrorImpl("", "line 1: expected method 'main' in class 'Main'",
                  "class Main { a : Bool; };");
            assertAnalyzerErrorImpl("", "line 1: expected 0 formal parameters for method 'main' in class 'Main'",
                  "class Main { main(a : Bool) : Object { 0 }; };");
         }
      else false fi
   };

   testMethod() : Object {
      if begin("method") then
         {
            assertAnalyzerError("", "line 1: unexpected formal parameter 'self'",
                  "class A { a(self : Bool) : Object { 0 }; };");
         }
      else false fi
   };

   testExpr() : Object {
      if begin("expr") then
         {
            assertAnalyzerError("", "line 1: expression type 'Int' is not required type 'Bool' for predicate in 'if' expression",
                  "class A { a() : Object { if 0 then 0 else 0 fi }; };");
         }
      else false fi
   };
};

class TestAnalyzer inherits Analyzer {
   errorString : String;
   errorString() : String { errorString };

   error(s : String) : Object {{
      errorString <- s;
      error <- true;
   }};
};
