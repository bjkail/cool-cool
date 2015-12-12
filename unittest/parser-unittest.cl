class Main inherits Test {
   test() : Object {{
      testProgram();
      testClass();
      testAttribute();
      testMethod();
      testExpr();
   }};

   newParser(program : String) : TestParser {
      new TestParser.init(new Tokenizer.init(new TestStringInputStream.init(program)))
   };

   assertParserError(context : String, error : String, program : String) : Object {
      let parser : TestParser <- newParser(program),
            program : ParsedProgram <- parser.parse() in
         {
            assertStringEquals(context, error, parser.errorString());
            assertVoid(context, program);
            program;
         }
   };

   testProgram() : Object {
      if begin("program") then
         {
            assertParserError("empty", "line 1: expected 'class' for program", "");
            assertParserError("class semi", "line 1: expected ';' after class definition",
                  "class A { a : A; }");
            assertParserError("trailing", "line 1: expected 'class' for program",
                  "class A { a : A; }; .");
            assertParserError("semi 2", "line 1: expected ';' after class definition",
                  "class A { a : A; }; class A { a : A; }");
         }
      else false fi
   };

   testClass() : Object {
      if begin("class") then
         {
            assertParserError("open", "line 1: expected '{' for class definition",
                  "class A");
            assertParserError("feature", "line 1: expected feature for class definition",
                  "class A {}");
            assertParserError("close", "line 1: expected '}' for class definition",
                  "class A { a : A;");
            assertParserError("inherits", "line 1: expected type for inherits",
                  "class A inherits x");
            assertParserError("feature", "line 1: expected '(' or ':' for feature",
                  "class A { a;");
            assertParserError("attribute", "line 1: expected ';' after feature",
                  "class A { a : A");
            assertParserError("method", "line 1: expected ';' after feature",
                  "class A { a() : A { a }");
         }
      else false fi
   };

   testAttribute() : Object {
      if begin("attribute") then
         {
            assertParserError("init", "line 1: expected type for attribute",
                  "class A { a :;");
            assertParserError("init", "line 1: expected expression for attribute initialization",
                  "class A { a : String <-;");
         }
      else false fi
   };

   testMethod() : Object {
      if begin("method") then
         {
            assertParserError("close", "line 1: expected ')' for method definition",
                  "class A { a( {");
            assertParserError("formal", "line 1: expected ')' for method definition",
                  "class A { a(, {");
            assertParserError("formal", "line 1: expected ':' for formal parameter type",
                  "class A { a(a {");
            assertParserError("formal", "line 1: expected type for formal parameter",
                  "class A { a(a : {");
            assertParserError("formal", "line 1: expected ')' for method definition",
                  "class A { a(a : A {");
            assertParserError("formal 2", "line 1: expected id for formal parameter",
                  "class A { a(a : A, {");
            assertParserError("formal 2", "line 1: expected ':' for formal parameter type",
                  "class A { a(a : A, a {");
            assertParserError("return", "line 1: expected ':' for method definition",
                  "class A { a() {");
            assertParserError("return", "line 1: expected type for method definition",
                  "class A { a() : {");
            assertParserError("open", "line 1: expected '{' for method definition",
                  "class A { a() : A");
            assertParserError("expr", "line 1: expected expression for method definition",
                  "class A { a() : A {");
            assertParserError("close", "line 1: expected '}' for method definition",
                  "class A { a() : A { a");
         }
      else false fi
   };

   getIteratorNext(iter : Iterator) : Object {{
      assertTrue("getNext", iter.next());
      iter.get();
   }};

   getCollectionFirst(coll : Collection) : Object {{
      getIteratorNext(coll.iterator());
   }};

   assertExpr(context : String, exprString : String) : ParsedExpr {
      let parser : TestParser <- newParser("class A { a : A <- ".concat(exprString).concat("; };")),
            program : ParsedProgram <- parser.parse() in
         {
            assertStringEquals(context.concat(" error"), "", parser.errorString());
            assertNotVoid(context.concat(" program"), program);
            let class_ : ParsedClass <- case getCollectionFirst(program.classes()) of x : ParsedClass => x; esac,
                  attr : ParsedAttribute <- case getCollectionFirst(class_.features()) of x : ParsedAttribute => x; esac in
               attr.expr();
         }
   };

   assertIdExpr(context : String, id : String, expr : ParsedExpr) : Object {
      case expr of
         expr : ParsedIdExpr =>
            assertStringEquals(context.concat(" id"), id, expr.id());
      esac
   };

   testExpr() : Object {
      if begin("expr") then
         {
            let expr : ParsedIdExpr <- case assertExpr("id", "a") of x : ParsedIdExpr => x; esac in
               assertStringEquals("id", "a", expr.id());

            let expr : ParsedDispatchExpr <- case assertExpr("dispatch", "a.b()") of x : ParsedDispatchExpr => x; esac in
               {
                  assertIdExpr("dispatch target", "a", expr.target());
                  assertStringEquals("dispatch", "", expr.type());
                  assertStringEquals("dispatch", "b", expr.id());
                  assertIntEquals("dispatch", 0, expr.arguments().size());
               };

            let expr : ParsedDispatchExpr <- case assertExpr("dispatch typed", "a@B.c()") of x : ParsedDispatchExpr => x; esac in
               {
                  assertIdExpr("dispatch typed target", "a", expr.target());
                  assertStringEquals("dispatch typed", "B", expr.type());
                  assertStringEquals("dispatch typed", "c", expr.id());
                  assertIntEquals("dispatch typed", 0, expr.arguments().size());
               };

            let expr : ParsedDispatchExpr <- case assertExpr("dispatch arg", "a.b(c)") of x : ParsedDispatchExpr => x; esac in
               {
                  assertIdExpr("dispatch arg target", "a", expr.target());
                  assertStringEquals("dispatch arg", "", expr.type());
                  assertStringEquals("dispatch arg", "b", expr.id());
                  let argIter : Iterator <- expr.arguments().iterator() in
                     {
                        assertIdExpr("dispatch arg", "c", case getIteratorNext(argIter) of x : ParsedExpr => x; esac);
                        assertFalse("dispatch arg end", argIter.next());
                     };
               };

            let expr : ParsedDispatchExpr <- case assertExpr("dispatch args", "a.b(c, d)") of x : ParsedDispatchExpr => x; esac in
               {
                  assertIdExpr("dispatch args target", "a", expr.target());
                  assertStringEquals("dispatch args", "", expr.type());
                  assertStringEquals("dispatch args", "b", expr.id());
                  let argIter : Iterator <- expr.arguments().iterator() in
                     {
                        assertIdExpr("dispatch args", "c", case getIteratorNext(argIter) of x : ParsedExpr => x; esac);
                        assertIdExpr("dispatch args", "d", case getIteratorNext(argIter) of x : ParsedExpr => x; esac);
                        assertFalse("dispatch args end", argIter.next());
                     };
               };
         }
      else false fi
   };
};

class TestParser inherits Parser {
   errorString : String;
   errorString() : String { errorString };

   error(s : String) : Bool {
      if error then
         false
      else
         {
            errorString <- tokenizer.lineMap().lineToString(tokenizer.line()).concat(": ").concat(s);
            error <- true;
            token <- new TokenEof;
            false;
         }
      fi
   };
};
