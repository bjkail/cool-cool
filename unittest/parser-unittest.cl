class Main inherits Test {
   test() : Object {{
      testProgram();
      testClass();
      testAttribute();
      testMethod();
      testExpr();
      testPrecedence();
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
         expr : ParsedIdExpr => assertStringEquals(context.concat(" id"), id, expr.id());
         expr : Object => failContext(context.concat(" id"), "type=".concat(expr.type_name()));
      esac
   };

   testExpr() : Object {
      if begin("expr") then
         {
            let expr : ParsedAssignmentExpr <- case assertExpr("assignment", "a <- b") of x : ParsedAssignmentExpr => x; esac in
               {
                  assertStringEquals("assignment", "a", expr.id());
                  assertIdExpr("assignment", "b", expr.expr());
               };

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

            let expr : ParsedDispatchExpr <- case assertExpr("dispatch self", "a()") of x : ParsedDispatchExpr => x; esac in
               {
                  assertVoid("dispatch self target", expr.target());
                  assertStringEquals("dispatch self", "", expr.type());
                  assertStringEquals("dispatch self", "a", expr.id());
                  assertIntEquals("dispatch self", 0, expr.arguments().size());
               };

            let expr : ParsedIfExpr <- case assertExpr("if", "if a then b else c fi") of x : ParsedIfExpr => x; esac in
               {
                  assertIdExpr("if expr", "a", expr.expr());
                  assertIdExpr("then expr", "b", expr.then_());
                  assertIdExpr("else expr", "c", expr.else_());
               };

            let expr : ParsedWhileExpr <- case assertExpr("while", "while a loop b pool") of x : ParsedWhileExpr => x; esac in
               {
                  assertIdExpr("while expr", "a", expr.expr());
                  assertIdExpr("loop expr", "b", expr.loop_());
               };

            let expr : ParsedBlockExpr <- case assertExpr("block 2", "{ a; }") of x : ParsedBlockExpr => x; esac,
                  exprIter : Iterator <- expr.exprs().iterator() in
               {
                  assertIdExpr("block 1", "a", case getIteratorNext(exprIter) of x : ParsedExpr => x; esac);
                  assertFalse("block 1 end", exprIter.next());
               };

            let expr : ParsedBlockExpr <- case assertExpr("block 2", "{ a; b; }") of x : ParsedBlockExpr => x; esac,
                  exprIter : Iterator <- expr.exprs().iterator() in
               {
                  assertIdExpr("block 2", "a", case getIteratorNext(exprIter) of x : ParsedExpr => x; esac);
                  assertIdExpr("block 2", "b", case getIteratorNext(exprIter) of x : ParsedExpr => x; esac);
                  assertFalse("block 2 end", exprIter.next());
               };

            let expr : ParsedLetExpr <- case assertExpr("let", "let a : A in b") of x : ParsedLetExpr => x; esac in
               {
                  let varIter : Iterator <- expr.vars().iterator() in
                     {
                        let var : ParsedVar <- case getIteratorNext(varIter) of x : ParsedVar => x; esac in
                           {
                              assertStringEquals("let var", "a", var.id());
                              assertStringEquals("let type", "A", var.type());
                              assertVoid("let var expr", var.expr());
                           };

                        assertFalse("let vars", varIter.next());
                     };

                  assertIdExpr("let", "b", expr.expr());
               };

            let expr : ParsedLetExpr <- case assertExpr("let assign", "let a : A <- b in c") of x : ParsedLetExpr => x; esac in
               {
                  let varIter : Iterator <- expr.vars().iterator() in
                     {
                        let var : ParsedVar <- case getIteratorNext(varIter) of x : ParsedVar => x; esac in
                           {
                              assertStringEquals("let assign var", "a", var.id());
                              assertStringEquals("let assign type", "A", var.type());
                              assertIdExpr("let assign var expr", "b", var.expr());
                           };

                        assertFalse("let assign vars", varIter.next());
                     };

                  assertIdExpr("let assign", "c", expr.expr());
               };

            let expr : ParsedLetExpr <- case assertExpr("let 2", "let a : A, b : B in c") of x : ParsedLetExpr => x; esac in
               {
                  let varIter : Iterator <- expr.vars().iterator() in
                     {
                        let var : ParsedVar <- case getIteratorNext(varIter) of x : ParsedVar => x; esac in
                           {
                              assertStringEquals("let 2 var", "a", var.id());
                              assertStringEquals("let 2 type", "A", var.type());
                              assertVoid("let 2 var 1 expr", var.expr());
                           };

                        let var : ParsedVar <- case getIteratorNext(varIter) of x : ParsedVar => x; esac in
                           {
                              assertStringEquals("let 2 var", "b", var.id());
                              assertStringEquals("let 2 type", "B", var.type());
                              assertVoid("let 2 var 2 expr", var.expr());
                           };

                        assertFalse("let", varIter.next());
                     };

                  assertIdExpr("let", "c", expr.expr());
               };

            let expr : ParsedCaseExpr <- case assertExpr("case", "case a of b : B => c; esac") of x : ParsedCaseExpr => x; esac in
               {
                  assertIdExpr("case", "a", expr.expr());

                  let varIter : Iterator <- expr.branches().iterator() in
                     {
                        let var : ParsedVar <- case getIteratorNext(varIter) of x : ParsedVar => x; esac in
                           {
                              assertStringEquals("case var", "b", var.id());
                              assertStringEquals("case type", "B", var.type());
                              assertIdExpr("case branch expr", "c", var.expr());
                           };

                        assertFalse("case vars", varIter.next());
                     };
               };

            let expr : ParsedCaseExpr <- case assertExpr("case 2", "case a of b : B => c; d : D => e; esac") of x : ParsedCaseExpr => x; esac in
               {
                  assertIdExpr("case 2", "a", expr.expr());

                  let varIter : Iterator <- expr.branches().iterator() in
                     {
                        let var : ParsedVar <- case getIteratorNext(varIter) of x : ParsedVar => x; esac in
                           {
                              assertStringEquals("case 2 var", "b", var.id());
                              assertStringEquals("case 2 type", "B", var.type());
                              assertIdExpr("case 2 branch 2 expr", "c", var.expr());
                           };

                        let var : ParsedVar <- case getIteratorNext(varIter) of x : ParsedVar => x; esac in
                           {
                              assertStringEquals("case 2 var", "d", var.id());
                              assertStringEquals("case 2 type", "D", var.type());
                              assertIdExpr("case 2 branch 2 expr", "e", var.expr());
                           };

                        assertFalse("case 2", varIter.next());
                     };
               };

            let expr : ParsedNewExpr <- case assertExpr("new", "new A") of x : ParsedNewExpr => x; esac in
               assertStringEquals("new", "A", expr.type());

            let expr : ParsedUnaryExpr <- case assertExpr("isvoid", "isvoid a") of x : ParsedUnaryExpr => x; esac in
               {
                  assertStringEquals("isvoid", "isvoid", expr.op());
                  assertIdExpr("isvoid", "a", expr.expr());
               };

            let expr : ParsedBinaryExpr <- case assertExpr("add", "a + b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("add", "+", expr.op());
                  assertIdExpr("add left", "a", expr.left());
                  assertIdExpr("add right", "b", expr.right());
               };

            let expr : ParsedBinaryExpr <- case assertExpr("subtract", "a - b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("subtract", "-", expr.op());
                  assertIdExpr("subtract left", "a", expr.left());
                  assertIdExpr("subtract right", "b", expr.right());
               };

            let expr : ParsedBinaryExpr <- case assertExpr("multiply", "a * b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("multiply", "*", expr.op());
                  assertIdExpr("multiply left", "a", expr.left());
                  assertIdExpr("multiply right", "b", expr.right());
               };

            let expr : ParsedBinaryExpr <- case assertExpr("divide", "a / b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("divide", "/", expr.op());
                  assertIdExpr("divide left", "a", expr.left());
                  assertIdExpr("divide right", "b", expr.right());
               };

            let expr : ParsedUnaryExpr <- case assertExpr("complement", "~a") of x : ParsedUnaryExpr => x; esac in
               {
                  assertStringEquals("complement", "~", expr.op());
                  assertIdExpr("complemenet", "a", expr.expr());
               };

            let expr : ParsedBinaryExpr <- case assertExpr("less", "a < b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("less", "<", expr.op());
                  assertIdExpr("less left", "a", expr.left());
                  assertIdExpr("less right", "b", expr.right());
               };

            let expr : ParsedBinaryExpr <- case assertExpr("lessEquals", "a <= b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("lessEquals", "<=", expr.op());
                  assertIdExpr("lessEquals left", "a", expr.left());
                  assertIdExpr("lessEquals right", "b", expr.right());
               };

            let expr : ParsedBinaryExpr <- case assertExpr("equals", "a = b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("equals", "=", expr.op());
                  assertIdExpr("equals left", "a", expr.left());
                  assertIdExpr("equals right", "b", expr.right());
               };

            let expr : ParsedUnaryExpr <- case assertExpr("not", "not a") of x : ParsedUnaryExpr => x; esac in
               {
                  assertStringEquals("not", "not", expr.op());
                  assertIdExpr("not", "a", expr.expr());
               };

            assertIdExpr("paren", "a", assertExpr("paren", "(a)"));

            let expr : ParsedIdExpr <- case assertExpr("id", "a") of x : ParsedIdExpr => x; esac in
               assertStringEquals("id", "a", expr.id());

            let expr : ParsedConstantIntExpr <- case assertExpr("int", "1") of x : ParsedConstantIntExpr => x; esac in
               assertIntEquals("int", 1, expr.value());

            let expr : ParsedConstantStringExpr <- case assertExpr("string", "\"a\"") of x : ParsedConstantStringExpr => x; esac in
               assertStringEquals("string", "a", expr.value());

            let expr : ParsedConstantBoolExpr <- case assertExpr("false", "false") of x : ParsedConstantBoolExpr => x; esac in
               assertFalse("false", expr.value());

            let expr : ParsedConstantBoolExpr <- case assertExpr("true", "true") of x : ParsedConstantBoolExpr => x; esac in
               assertTrue("true", expr.value());
         }
      else false fi
   };

   testPrecedence() : Object {
      if begin("precedence") then
         {
            -- "."

            case assertExpr("complement dispatch", "~a.b()") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("complementdispatch expr".concat(x.type_name()));
            esac;

            case assertExpr("isvoid dispatch", "isvoid a.b()") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("isvoid dispatch expr".concat(x.type_name()));
            esac;

            case assertExpr("multiply dispatch", "a * b.c()") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("multiply dispatch type=".concat(x.type_name()));
            esac;

            case assertExpr("divide dispatch", "a / b.c()") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("divide dispatch type=".concat(x.type_name()));
            esac;

            case assertExpr("add dispatch", "a + b.c()") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("add dispatch type=".concat(x.type_name()));
            esac;

            case assertExpr("subtract dispatch", "a + b.c()") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("subtract dispatch type=".concat(x.type_name()));
            esac;

            case assertExpr("lessEquals dispatch", "a <= b.c()") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("lessEquals dispatch type=".concat(x.type_name()));
            esac;

            case assertExpr("less dispatch", "a < b.c()") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("less dispatch type=".concat(x.type_name()));
            esac;

            case assertExpr("equals dispatch", "a = b.c()") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("equals dispatch type=".concat(x.type_name()));
            esac;

            case assertExpr("not dispatch", "not a.b()") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("not dispatch expr".concat(x.type_name()));
            esac;

            case assertExpr("assignment dispatch", "a <- b.c()") of
               x : ParsedAssignmentExpr => false;
               x : Object => fail("equals dispatch type=".concat(x.type_name()));
            esac;

            -- "~"

            case assertExpr("complement multiply", "~a * b") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("complement multiply type=".concat(x.type_name()));
            esac;

            case assertExpr("complement divide", "~a / b") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("complement divide type=".concat(x.type_name()));
            esac;

            case assertExpr("complement add", "~a + b") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("complement add type=".concat(x.type_name()));
            esac;

            case assertExpr("complement subtract", "~a - b") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("complement subtract type=".concat(x.type_name()));
            esac;

            case assertExpr("complement lessEquals", "~a <= b") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("complement lessEquals type=".concat(x.type_name()));
            esac;

            case assertExpr("complement less", "~a < b") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("complement less type=".concat(x.type_name()));
            esac;

            case assertExpr("complement equals", "~a = b") of
               x : ParsedBinaryExpr => false;
               x : Object => fail("complement equals type=".concat(x.type_name()));
            esac;

            case assertExpr("complement subtract", "~a <- b") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("complement assignment type=".concat(x.type_name()));
            esac;

            -- "*"

            let expr : ParsedBinaryExpr <- case assertExpr("multiply multiply", "a * b * c") of x : ParsedBinaryExpr => x; esac in
               assertIdExpr("multiply multiply", "c", expr.right());

            let expr : ParsedBinaryExpr <- case assertExpr("divide multiple", "a / b * c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("divide multiply", "*", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("add multiply", "a + b * c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("add multiply", "+", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("subtract multiply", "a - b * c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("subtract multiply", "-", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("lessEquals multiply", "a <= b * c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("lessEquals multiply", "<=", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("less multiply", "a < b * c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("less multiply", "<", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("equals multiply", "a = b * c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("equals multiply", "=", expr.op());

            case assertExpr("not multiply", "not a * b") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("not multiply type=".concat(x.type_name()));
            esac;

            case assertExpr("assignment multiply", "a <- b * c") of
               x : ParsedAssignmentExpr => false;
               x : Object => fail("assignment multiply type=".concat(x.type_name()));
            esac;

            -- "/"

            let expr : ParsedBinaryExpr <- case assertExpr("divide divide", "a / b / c") of x : ParsedBinaryExpr => x; esac in
               assertIdExpr("divide divide", "c", expr.right());

            let expr : ParsedBinaryExpr <- case assertExpr("multiply multiple", "a * b / c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("multiply divide", "/", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("add divide", "a + b / c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("add divide", "+", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("subtract divide", "a - b / c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("subtract divide", "-", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("lessEquals divide", "a <= b / c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("lessEquals divide", "<=", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("less divide", "a < b / c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("less divide", "<", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("equals divide", "a = b / c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("equals divide", "=", expr.op());

            case assertExpr("not divide", "not a / b") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("not divide type=".concat(x.type_name()));
            esac;

            case assertExpr("assignment divide", "a <- b / c") of
               x : ParsedAssignmentExpr => false;
               x : Object => fail("assignment divide type=".concat(x.type_name()));
            esac;

            -- "+"

            let expr : ParsedBinaryExpr <- case assertExpr("add add", "a + b + c") of x : ParsedBinaryExpr => x; esac in
               assertIdExpr("add add", "c", expr.right());

            let expr : ParsedBinaryExpr <- case assertExpr("subtract add", "a - b + c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("subtract add", "+", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("lessEquals add", "a <= b + c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("lessEquals add", "<=", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("less add", "a < b + c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("less add", "<", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("equals add", "a = b + c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("equals add", "=", expr.op());

            case assertExpr("not add", "not a + b") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("not add type=".concat(x.type_name()));
            esac;

            case assertExpr("assignment add", "a <- b + c") of
               x : ParsedAssignmentExpr => false;
               x : Object => fail("assignment add type=".concat(x.type_name()));
            esac;

            -- "-"

            let expr : ParsedBinaryExpr <- case assertExpr("subtract subtract", "a + b - c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("add subtract", "-", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("subtract subtract", "a - b - c") of x : ParsedBinaryExpr => x; esac in
               assertIdExpr("subtract subtract", "c", expr.right());

            let expr : ParsedBinaryExpr <- case assertExpr("lessEquals subtract", "a <= b - c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("lessEquals subtract", "<=", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("less subtract", "a < b - c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("less subtract", "<", expr.op());

            let expr : ParsedBinaryExpr <- case assertExpr("equals subtract", "a = b - c") of x : ParsedBinaryExpr => x; esac in
               assertStringEquals("equals subtract", "=", expr.op());

            case assertExpr("not subtract", "not a - b") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("not subtract type=".concat(x.type_name()));
            esac;

            case assertExpr("assignment subtract", "a <- b - c") of
               x : ParsedAssignmentExpr => false;
               x : Object => fail("assignment subtract type=".concat(x.type_name()));
            esac;

            -- "<="

            case assertExpr("not lessEquals", "not a <= b") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("not lessEquals type=".concat(x.type_name()));
            esac;

            case assertExpr("assignment lessEquals", "a <- b <= c") of
               x : ParsedAssignmentExpr => false;
               x : Object => fail("assignment lessEquals type=".concat(x.type_name()));
            esac;

            -- "<"

            case assertExpr("not less", "not a < b") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("not less type=".concat(x.type_name()));
            esac;

            case assertExpr("assignment less", "a <- b < c") of
               x : ParsedAssignmentExpr => false;
               x : Object => fail("assignment less type=".concat(x.type_name()));
            esac;

            -- "="

            case assertExpr("not equals", "not a = b") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("not equals type=".concat(x.type_name()));
            esac;

            case assertExpr("assignment equals", "a <- b = c") of
               x : ParsedAssignmentExpr => false;
               x : Object => fail("assignment equals type=".concat(x.type_name()));
            esac;

            -- "not"

            case assertExpr("not assignment", "not a <- b") of
               x : ParsedUnaryExpr => false;
               x : Object => fail("not assignment type=".concat(x.type_name()));
            esac;
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
