class Main inherits Test {
   test() : Object {{
      testRegister();
      testMain();
      testAttribute();
      testMethod();
      testExpr();
   }};

   newAnalyzer(context : String, program : String) : TestAnalyzer {
      let tokenizer : Tokenizer <- new Tokenizer.init(new TestStringInputStream.init(program)),
            parser : Parser <- new Parser.init(tokenizer),
            program : ParsedProgram <- parser.parse() in
         {
            assertNotVoid(context.concat(" program"), program);
            new TestAnalyzer.initTest(tokenizer.lineMap(), program);
         }
   };

   newAnalyzerDefaultMain(context : String, program : String) : TestAnalyzer {
      newAnalyzer(context, "class Main { main() : Object { false }; }; ".concat(program))
   };

   assertAnalyze(context : String, analyzer : TestAnalyzer) : AnalyzedProgram {
      let program : AnalyzedProgram <- analyzer.analyzeTest() in
         {
            assertStringEquals(context.concat(" error"), "", analyzer.errorString());
            assertNotVoid(context.concat(" program"), program);
            program;
         }
   };

   assertAnalyzerErrorImpl(context : String, error : String, program : String) : Object {
      let analyzer : TestAnalyzer <- newAnalyzer(context, program),
            program : AnalyzedProgram <- analyzer.analyzeTest() in
         {
            assertStringEquals(context, error, analyzer.errorString());
            assertVoid(context, program);
         }
   };

   assertAnalyzerError(context : String, error : String, program : String) : Object {
      assertAnalyzerErrorImpl(context, error, "class Main { main() : Object { 0 }; }; ".concat(program))
   };

   assertStringMapIteratorNext(context : String, key : String, iter : StringMapIterator) : Object {{
      assertTrue(context.concat(" next"), iter.next());
      assertStringEquals(context.concat(" key"), key, iter.key());
      iter.value();
   }};

   assertSameType(context : String, expected : AnalyzedType, actual : AnalyzedType) : Object {
      if not actual = expected then
         failContext(context, "expected=".concat(expected.name())
               .concat(", actual=").concat(actual.name()))
      else false fi
   };

   getIteratorNext(iter : Iterator) : Object {{
      assertTrue("getNext", iter.next());
      iter.get();
   }};

   testRegister() : Object {
      if begin("register") then
         {
            assertAnalyzerError("", "line 1: redefinition of class 'A'",
                  "class A { a : Bool; }; class A { a : Bool; };");
            assertAnalyzerError("", "line 1: undefined type 'B' for 'inherits'",
                  "class A inherits B { a : Bool; };");
            assertAnalyzerError("", "line 1: hierarchy of class 'A' contains a cycle",
                  "class A inherits B { a : Bool; }; class B inherits A { b : Bool; };");
            assertAnalyzerError("", "line 1: undefined type 'B' for attribute",
                  "class A { b : B; };");
            assertAnalyzerError("", "line 1: redefinition of attribute 'a' in class 'A'",
                  "class A { a : Bool; a : Bool; };");
            assertAnalyzerError("", "line 1: redefinition of attribute 'a' in class 'B'",
                  "class A { a : Bool; }; class B inherits A { a : Bool; };");
            assertAnalyzerError("", "line 1: undefined type 'B' for formal parameter #1",
                  "class A { a(b : B) : Object { 0 }; };");
            assertAnalyzerError("", "line 1: undefined type 'B' for return",
                  "class A { a() : B { 0 }; };");
            assertAnalyzerError("", "line 1: redefinition of method 'a' in class 'A'",
                  "class A { a() : Object { 0 }; a() : Object { 0 }; };");
            assertAnalyzerError("", "line 1: redefinition of method 'a' in class 'B' with 1 formal parameters is not the same as 0 in class 'A'",
                  "class A { a() : Object { 0 }; }; class B inherits A { a(b : Int) : Object { 0 }; };");
            assertAnalyzerError("", "line 1: redefinition of method 'a' in class 'B' with type 'Bool' for formal parameter #1 is not the same as type 'Int' in class 'A'",
                  "class A { a(a : Int) : Object { 0 }; }; class B inherits A { a(b : Bool) : Object { 0 }; };");
            assertAnalyzerError("", "line 1: redefinition of method 'a' in class 'B' with return type 'Int' is not the same as type 'Object' in class 'A'",
                  "class A { a() : Object { 0 }; }; class B inherits A { a() : Int { 0 }; };");

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("inherits",
                     "class A inherits B { b() : Object { false }; }; class B { b() : Object { false }; };"),
                  program : AnalyzedProgram <- assertAnalyze("inherits", analyzer) in
               assertSameType("inherits", program.getType("B"), program.getType("A").inheritsType());
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

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("main", ""),
                  program : AnalyzedProgram <- analyzer.analyzeTest(),
                  mainMethod : AnalyzedMethod <- program.mainMethod() in
               {
                  assertStringEquals("method id", "main", mainMethod.id());
                  assertStringEquals("main defining", "Main", mainMethod.definingType().name());
               };
         }
      else false fi
   };

   testAttribute() : Object {
      if begin("attribute") then
         {
            assertAnalyzerError("", "line 1: invalid attribute name 'self'",
                  "class A { self : Bool; };");
            assertAnalyzerError("", "line 1: expression type 'Int' does not conform to type 'Bool' of attribute 'a'",
                  "class A { a : Bool <- 0; };");

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("attribute",
                     "class A { a : Object; b : Int <- 0; };"),
                  program : AnalyzedProgram <- analyzer.analyzeTest(),
                  type : AnalyzedType <- program.getType("A") in
               {
                  let attr : AnalyzedAttribute <- type.getAttribute("a") in
                     {
                        assertSameType("a defining", type, attr.definingType());
                        assertStringEquals("a id", "a", attr.id());
                        assertSameType("a type", analyzer.objectType(), attr.type());
                        assertVoid("a expr", attr.expr());
                     };

                  let attr : AnalyzedAttribute <- type.getAttribute("b") in
                     {
                        assertSameType("b defining", type, attr.definingType());
                        assertStringEquals("b id", "b", attr.id());
                        assertSameType("b type", analyzer.intType(), attr.type());
                        assertNotVoid("b expr", attr.expr());
                     };
               };
         }
      else false fi
   };

   testMethod() : Object {
      if begin("method") then
         {
            assertAnalyzerError("", "line 1: invalid formal parameter name 'self'",
                  "class A { a(self : Bool) : Object { 0 }; };");
            assertAnalyzerError("", "line 1: expression type 'Int' does not conform to return type 'Bool' of method 'a'",
                  "class A { a() : Bool { 0 }; };");

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("method",
                     "class A { a() : Object { false }; b(a : Int) : Bool { false }; };"),
                  program : AnalyzedProgram <- analyzer.analyzeTest(),
                  type : AnalyzedType <- program.getType("A") in
               {
                  let method : AnalyzedMethod <- type.getMethod("a") in
                     {
                        assertSameType("a defining", type, method.definingType());
                        assertStringEquals("a id", "a", method.id());
                        assertSameType("a return", analyzer.objectType(), method.returnType());
                        assertIntEquals("a formals", 0, method.formalTypes().size());
                     };

                  let method : AnalyzedMethod <- type.getMethod("b") in
                     {
                        assertSameType("b defining", type, method.definingType());
                        assertStringEquals("b id", "b", method.id());
                        assertSameType("b return", analyzer.boolType(), method.returnType());

                        let formalIter : Iterator <- method.formalTypes().iterator() in
                           {
                              assertSameType("b formal", analyzer.intType(),
                                    case getIteratorNext(formalIter) of x : AnalyzedType => x; esac);
                              assertFalse("b formals", formalIter.next());
                           };
                     };
               };
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

   program : ParsedProgram;

   initTest(lineMap : TokenizerLineMap, program_ : ParsedProgram) : SELF_TYPE {{
      program <- program_;
      init(lineMap);
   }};

   error(s : String) : Object {
      if not error then
         {
            errorString <- s;
            error <- true;
         }
      else false fi
   };

   analyzeTest() : AnalyzedProgram {
      analyze(program)
   };
};
