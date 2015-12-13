class Main inherits Test {
   test() : Object {{
      testRegister();
      testMain();
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

            let analyzer : TestAnalyzer <- newAnalyzer("type", "class Main { main() : Object { false }; };"),
                  program : AnalyzedProgram <- analyzer.analyzeTest(),
                  mainMethod : AnalyzedMethod <- program.mainMethod(),
                  typeIter : Iterator <- program.types().iterator() in
               {
                  assertStringEquals("type main method", "main", mainMethod.id());

                  let type : AnalyzedType <- case getIteratorNext(typeIter) of x : AnalyzedType => x; esac,
                        methodIter : StringMapIterator <- type.methods().iterator() in
                     {
                        assertSameType("type main method", type, mainMethod.definingType());
                        assertTrue("method main", mainMethod = assertStringMapIteratorNext("method", "main", methodIter));
                        assertNotVoid("method copy", assertStringMapIteratorNext("method", "copy", methodIter));
                        assertNotVoid("method type_name", assertStringMapIteratorNext("method", "type_name", methodIter));
                        assertNotVoid("method abort", assertStringMapIteratorNext("method", "abort", methodIter));
                        assertFalse("method end", methodIter.next());
                     };

                  assertFalse("type", typeIter.next());
               };
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

   program : ParsedProgram;

   initTest(lineMap : TokenizerLineMap, program_ : ParsedProgram) : SELF_TYPE {{
      program <- program_;
      init(lineMap);
   }};

   error(s : String) : Object {{
      errorString <- s;
      error <- true;
   }};

   analyzeTest() : AnalyzedProgram {
      analyze(program)
   };
};
