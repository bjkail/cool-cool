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
      new TestParser.init(new Tokenizer.init(new TestStringInputStream.init(program.concat("\n"))))
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

            let program : ParsedProgram <- newParser("class \nA\n { a : A; b() : B { c }; };").parse(),
                  classIter : Iterator <- program.classes().iterator() in
               {
                  let class_ : ParsedClass <- case getIteratorNext(classIter) of x : ParsedClass => x; esac in
                     {
                        assertStringEquals("class", "A", class_.type());
                        assertIntEquals("class line", 2, class_.line());

                        let featureIter : Iterator <- class_.features().iterator() in
                           {
                              case getIteratorNext(featureIter) of x : ParsedAttribute => x; esac;
                              case getIteratorNext(featureIter) of x : ParsedMethod => x; esac;
                              assertFalse("single", featureIter.next());
                           };
                     };

                  assertFalse("single", classIter.next());
               };

            let program : ParsedProgram <- newParser("class A { a : A; }; class B { b : B; };").parse(),
                  classIter : Iterator <- program.classes().iterator() in
               {
                  assertStringEquals("multiple", "A", case getIteratorNext(classIter) of x : ParsedClass => x.type(); esac);
                  assertStringEquals("multiple", "B", case getIteratorNext(classIter) of x : ParsedClass => x.type(); esac);
                  assertFalse("multiple", classIter.next());
               };
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

            let program : ParsedProgram <- newParser("class A { \nb\n : B; };").parse(),
                  class_ : ParsedClass <- case getCollectionFirst(program.classes()) of x : ParsedClass => x; esac,
                  featureIter : Iterator <- class_.features().iterator() in
               {
                  let attr : ParsedAttribute <- case getIteratorNext(featureIter) of x : ParsedAttribute => x; esac in
                     {
                        assertStringEquals("single", "b", attr.id());
                        assertStringEquals("single", "B", attr.type());
                        assertVoid("single", attr.expr());
                        assertIntEquals("single line", 2, attr.line());
                     };

                  assertFalse("single", featureIter.next());
               };

            let program : ParsedProgram <- newParser("class A { b : B <- c; };").parse(),
                  class_ : ParsedClass <- case getCollectionFirst(program.classes()) of x : ParsedClass => x; esac,
                  featureIter : Iterator <- class_.features().iterator() in
               {
                  let attr : ParsedAttribute <- case getIteratorNext(featureIter) of x : ParsedAttribute => x; esac in
                     {
                        assertStringEquals("expr", "b", attr.id());
                        assertStringEquals("expr", "B", attr.type());
                        assertNotVoid("expr", attr.expr());
                     };

                  assertFalse("single", featureIter.next());
               };

            let program : ParsedProgram <- newParser("class A { b : B; c : C; };").parse(),
                  class_ : ParsedClass <- case getCollectionFirst(program.classes()) of x : ParsedClass => x; esac,
                  featureIter : Iterator <- class_.features().iterator() in
               {
                  let attr : ParsedAttribute <- case getIteratorNext(featureIter) of x : ParsedAttribute => x; esac in
                     {
                        assertStringEquals("multiple", "b", attr.id());
                        assertStringEquals("multiple", "B", attr.type());
                     };

                  let attr : ParsedAttribute <- case getIteratorNext(featureIter) of x : ParsedAttribute => x; esac in
                     {
                        assertStringEquals("multiple", "c", attr.id());
                        assertStringEquals("multiple", "C", attr.type());
                     };

                  assertFalse("multiple", featureIter.next());
               };
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

            let program : ParsedProgram <- newParser("class A { \nb\n() : B { c }; };").parse(),
                  class_ : ParsedClass <- case getCollectionFirst(program.classes()) of x : ParsedClass => x; esac,
                  featureIter : Iterator <- class_.features().iterator() in
               {
                  let method : ParsedMethod <- case getIteratorNext(featureIter) of x : ParsedMethod => x; esac in
                     {
                        assertStringEquals("single", "b", method.id());
                        assertStringEquals("single", "B", method.returnType());
                        assertIntEquals("single formals", 0, method.formals().size());
                        assertIdExpr("single expr", "c", method.expr());
                        assertIntEquals("single line", 2, method.line());
                     };

                  assertFalse("single", featureIter.next());
               };

            let program : ParsedProgram <- newParser("class A { b(\nc\n : C) : B { e }; f(g : G, h : H) : F { i }; };").parse(),
                  class_ : ParsedClass <- case getCollectionFirst(program.classes()) of x : ParsedClass => x; esac,
                  featureIter : Iterator <- class_.features().iterator() in
               {
                  let method : ParsedMethod <- case getIteratorNext(featureIter) of x : ParsedMethod => x; esac in
                     {
                        assertStringEquals("multiple 1", "b", method.id());
                        assertStringEquals("multiple 1", "B", method.returnType());

                        let formalIter : Iterator <- method.formals().iterator() in
                           {
                              let formal : ParsedFormal <- case getIteratorNext(formalIter) of x : ParsedFormal => x; esac in
                                 {
                                    assertStringEquals("multiple 1 formal", "c", formal.id());
                                    assertStringEquals("multiple 1 formal", "C", formal.type());
                                    assertIntEquals("multiple 1 formal line", 2, formal.line());
                                 };

                              assertFalse("multiple 1 formals", formalIter.next());
                           };

                        assertIdExpr("multiple 1 expr", "e", method.expr());
                     };

                  let method : ParsedMethod <- case getIteratorNext(featureIter) of x : ParsedMethod => x; esac in
                     {
                        assertStringEquals("multiple 2", "f", method.id());
                        assertStringEquals("multiple 2", "F", method.returnType());

                        let formalIter : Iterator <- method.formals().iterator() in
                           {
                              let formal : ParsedFormal <- case getIteratorNext(formalIter) of x : ParsedFormal => x; esac in
                                 {
                                    assertStringEquals("multiple 2 formal", "g", formal.id());
                                    assertStringEquals("multiple 2 formal", "G", formal.type());
                                 };

                              let formal : ParsedFormal <- case getIteratorNext(formalIter) of x : ParsedFormal => x; esac in
                                 {
                                    assertStringEquals("multiple 2 formal", "h", formal.id());
                                    assertStringEquals("multiple 2 formal", "H", formal.type());
                                 };

                              assertFalse("multiple 1 formals", formalIter.next());
                           };

                        assertIdExpr("multiple 1 expr", "i", method.expr());
                     };

                  assertFalse("multiple", featureIter.next());
               };
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

   assertExprError(context : String, error : String, exprString : String) : Object {
      assertParserError(context, error, "class A { a : A <- ".concat(exprString))
   };

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
            assertExprError("assignment", "line 1: expected expression for assignment", "a <-");

            let expr : ParsedAssignmentExpr <- case assertExpr("assignment", "a \n<-\n b") of x : ParsedAssignmentExpr => x; esac in
               {
                  assertStringEquals("assignment", "a", expr.id());
                  assertIdExpr("assignment", "b", expr.expr());
                  assertIntEquals("assignment", 2, expr.line());
               };

            assertExprError("assignment", "line 1: expected id for dispatch method", "a.");
            assertExprError("assignment", "line 1: expected '(' for dispatch expression", "a.b");
            assertExprError("assignment", "line 1: expected expression for dispatch argument", "a.b(");
            assertExprError("assignment", "line 1: expected expression for dispatch argument", "a.b(c,");
            assertExprError("assignment", "line 1: expected ')' for dispatch expression", "a.b(c");

            let expr : ParsedDispatchExpr <- case assertExpr("dispatch", "a\n.\nb()") of x : ParsedDispatchExpr => x; esac in
               {
                  assertIdExpr("dispatch target", "a", expr.target());
                  assertStringEquals("dispatch", "", expr.type());
                  assertStringEquals("dispatch", "b", expr.id());
                  assertIntEquals("dispatch", 0, expr.arguments().size());
                  assertIntEquals("dispatch line", 2, expr.line());
               };

            assertExprError("dispatch typed", "line 1: expected type for static dispatch", "a@");
            assertExprError("dispatch typed", "line 1: expected id for dispatch method", "a@B.");
            assertExprError("dispatch typed", "line 1: expected '(' for dispatch expression", "a@B.c");
            assertExprError("dispatch typed", "line 1: expected expression for dispatch argument", "a@B.c(");
            assertExprError("dispatch typed", "line 1: expected expression for dispatch argument", "a@B.c(d,");
            assertExprError("dispatch typed", "line 1: expected ')' for dispatch expression", "a@B.c(d");

            let expr : ParsedDispatchExpr <- case assertExpr("dispatch typed", "a@B\n.\nc()") of x : ParsedDispatchExpr => x; esac in
               {
                  assertIdExpr("dispatch typed target", "a", expr.target());
                  assertStringEquals("dispatch typed", "B", expr.type());
                  assertStringEquals("dispatch typed", "c", expr.id());
                  assertIntEquals("dispatch typed", 0, expr.arguments().size());
                  assertIntEquals("dispatch typed line", 2, expr.line());
               };

            let expr : ParsedDispatchExpr <- case assertExpr("dispatch arg", "a.b(\nc\n)") of x : ParsedDispatchExpr => x; esac in
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

            assertExprError("dispatch", "line 1: expected expression for dispatch argument", "b(");
            assertExprError("dispatch", "line 1: expected expression for dispatch argument", "b(c,");
            assertExprError("dispatch", "line 1: expected ')' for dispatch expression", "b(c");

            let expr : ParsedDispatchExpr <- case assertExpr("dispatch self", "a()") of x : ParsedDispatchExpr => x; esac in
               {
                  assertVoid("dispatch self target", expr.target());
                  assertStringEquals("dispatch self", "", expr.type());
                  assertStringEquals("dispatch self", "a", expr.id());
                  assertIntEquals("dispatch self", 0, expr.arguments().size());
               };

            assertExprError("if", "line 1: expected expression for 'if'", "if");
            assertExprError("if", "line 1: expected 'then' for 'if' expression", "if a");
            assertExprError("if", "line 1: expected expression for 'then'", "if a then");
            assertExprError("if", "line 1: expected 'else' for 'if' expression", "if a then b");
            assertExprError("if", "line 1: expected expression for 'else'", "if a then b else");
            assertExprError("if", "line 1: expected 'fi' for 'if' expression", "if a then b else c");

            let expr : ParsedIfExpr <- case assertExpr("if", "\nif\n a then b else c fi") of x : ParsedIfExpr => x; esac in
               {
                  assertIdExpr("if expr", "a", expr.expr());
                  assertIdExpr("then expr", "b", expr.then_());
                  assertIdExpr("else expr", "c", expr.else_());
                  assertIntEquals("if line", 2, expr.line());
               };

            assertExprError("while", "line 1: expected expression for 'while'", "while");
            assertExprError("while", "line 1: expected 'loop' for 'while' expression", "while a");

            let expr : ParsedWhileExpr <- case assertExpr("while", "\nwhile\n a loop b pool") of x : ParsedWhileExpr => x; esac in
               {
                  assertIdExpr("while expr", "a", expr.expr());
                  assertIdExpr("loop expr", "b", expr.loop_());
                  assertIntEquals("while line", 2, expr.line());
               };

            assertExprError("block", "line 1: expected expression for block", "{");
            assertExprError("block", "line 1: expected ';' after expression in block", "{ a");
            assertExprError("block", "line 1: expected '}' for block expression", "{ a;");
            assertExprError("block", "line 1: expected ';' after expression in block", "{ a; a");

            let expr : ParsedBlockExpr <- case assertExpr("block 2", "\n{\n a; }") of x : ParsedBlockExpr => x; esac,
                  exprIter : Iterator <- expr.exprs().iterator() in
               {
                  assertIdExpr("block 1", "a", case getIteratorNext(exprIter) of x : ParsedExpr => x; esac);
                  assertFalse("block 1 end", exprIter.next());
                  assertIntEquals("block 1 line", 2, expr.line());
               };

            let expr : ParsedBlockExpr <- case assertExpr("block 2", "{ a; b; }") of x : ParsedBlockExpr => x; esac,
                  exprIter : Iterator <- expr.exprs().iterator() in
               {
                  assertIdExpr("block 2", "a", case getIteratorNext(exprIter) of x : ParsedExpr => x; esac);
                  assertIdExpr("block 2", "b", case getIteratorNext(exprIter) of x : ParsedExpr => x; esac);
                  assertFalse("block 2 end", exprIter.next());
               };

            assertExprError("let", "line 1: expected id for 'let' variable", "let");
            assertExprError("let", "line 1: expected ':' for 'let' variable type", "let a");
            assertExprError("let", "line 1: expected type for 'let' variable", "let a :");
            assertExprError("let", "line 1: expected 'in' for 'let' expression", "let a : A");
            assertExprError("let", "line 1: expected expression for 'let' variable initialization", "let a : A <-");
            assertExprError("let", "line 1: expected id for 'let' variable", "let a : A,");
            assertExprError("let", "line 1: expected ':' for 'let' variable type", "let a : A, a");
            assertExprError("let", "line 1: expected type for 'let' variable", "let a : A, a :");

            let expr : ParsedLetExpr <- case assertExpr("let", "\nlet\n \na\n : A in b") of x : ParsedLetExpr => x; esac in
               {
                  let varIter : Iterator <- expr.vars().iterator() in
                     {
                        let var : ParsedVar <- case getIteratorNext(varIter) of x : ParsedVar => x; esac in
                           {
                              assertStringEquals("let var", "a", var.id());
                              assertStringEquals("let type", "A", var.type());
                              assertVoid("let var expr", var.expr());
                              assertIntEquals("let var line", 4, var.line());
                           };

                        assertFalse("let vars", varIter.next());
                     };

                  assertIdExpr("let", "b", expr.expr());
                  assertIntEquals("let line", 2, expr.line());
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

            assertExprError("case", "line 1: expected expression for 'case'", "case");
            assertExprError("case", "line 1: expected 'of' for 'case' expression", "case a");
            assertExprError("case", "line 1: expected id for 'case' variable", "case a of");
            assertExprError("case", "line 1: expected ':' for 'case' variable type", "case a of a");
            assertExprError("case", "line 1: expected type for 'case' variable", "case a of a :");
            assertExprError("case", "line 1: expected '=>' for 'case' branch", "case a of a : A");
            assertExprError("case", "line 1: expected expression for 'case' branch", "case a of a : A =>");
            assertExprError("case", "line 1: expected ';' after 'case' branch", "case a of a : A => a");
            assertExprError("case", "line 1: expected 'esac' for 'case' expression", "case a of a : A => a;");
            assertExprError("case", "line 1: expected ':' for 'case' variable type", "case a of a : A => a; a");
            assertExprError("case", "line 1: expected type for 'case' variable", "case a of a : A => a; a :");
            assertExprError("case", "line 1: expected '=>' for 'case' branch", "case a of a : A => a; a : A");
            assertExprError("case", "line 1: expected expression for 'case' branch", "case a of a : A => a; a : A =>");
            assertExprError("case", "line 1: expected ';' after 'case' branch", "case a of a : A => a; a : A => a");

            let expr : ParsedCaseExpr <- case assertExpr("case", "\ncase\n a of \nb\n : B => c; esac") of x : ParsedCaseExpr => x; esac in
               {
                  assertIdExpr("case", "a", expr.expr());
                  assertIntEquals("case line", 2, expr.line());

                  let varIter : Iterator <- expr.branches().iterator() in
                     {
                        let var : ParsedVar <- case getIteratorNext(varIter) of x : ParsedVar => x; esac in
                           {
                              assertStringEquals("case var", "b", var.id());
                              assertStringEquals("case type", "B", var.type());
                              assertIdExpr("case branch expr", "c", var.expr());
                              assertIntEquals("case branch line", 4, var.line());
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

            assertExprError("new", "line 1: expected type for 'new' expression", "new");

            let expr : ParsedNewExpr <- case assertExpr("new", "\nnew\n A") of x : ParsedNewExpr => x; esac in
               {
                  assertStringEquals("new", "A", expr.type());
                  assertIntEquals("new line", 2, expr.line());
               };

            assertExprError("isvoid", "line 1: expected expression for 'isvoid'", "isvoid");

            let expr : ParsedUnaryExpr <- case assertExpr("isvoid", "\nisvoid\n a") of x : ParsedUnaryExpr => x; esac in
               {
                  assertStringEquals("isvoid", "isvoid", expr.op());
                  assertIdExpr("isvoid", "a", expr.expr());
                  assertIntEquals("isvoid line", 2, expr.line());
               };

            assertExprError("add", "line 1: expected expression for '+'", "a +");

            let expr : ParsedBinaryExpr <- case assertExpr("add", "a \n+\n b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("add", "+", expr.op());
                  assertIdExpr("add left", "a", expr.left());
                  assertIdExpr("add right", "b", expr.right());
                  assertIntEquals("add line", 2, expr.line());
               };

            assertExprError("subtract", "line 1: expected expression for '-'", "a -");

            let expr : ParsedBinaryExpr <- case assertExpr("subtract", "a \n-\n b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("subtract", "-", expr.op());
                  assertIdExpr("subtract left", "a", expr.left());
                  assertIdExpr("subtract right", "b", expr.right());
                  assertIntEquals("subtract line", 2, expr.line());
               };

            assertExprError("multiply", "line 1: expected expression for '*'", "a *");

            let expr : ParsedBinaryExpr <- case assertExpr("multiply", "a \n*\n b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("multiply", "*", expr.op());
                  assertIdExpr("multiply left", "a", expr.left());
                  assertIdExpr("multiply right", "b", expr.right());
                  assertIntEquals("multiply line", 2, expr.line());
               };

            assertExprError("divide", "line 1: expected expression for '/'", "a /");

            let expr : ParsedBinaryExpr <- case assertExpr("divide", "a \n/\n b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("divide", "/", expr.op());
                  assertIdExpr("divide left", "a", expr.left());
                  assertIdExpr("divide right", "b", expr.right());
                  assertIntEquals("divide line", 2, expr.line());
               };

            assertExprError("complement", "line 1: expected expression for '~'", "~");

            let expr : ParsedUnaryExpr <- case assertExpr("complement", "\n~\na") of x : ParsedUnaryExpr => x; esac in
               {
                  assertStringEquals("complement", "~", expr.op());
                  assertIdExpr("complemenet", "a", expr.expr());
                  assertIntEquals("complement line", 2, expr.line());
               };

            assertExprError("less", "line 1: expected expression for '<'", "a <");

            let expr : ParsedBinaryExpr <- case assertExpr("less", "a \n<\n b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("less", "<", expr.op());
                  assertIdExpr("less left", "a", expr.left());
                  assertIdExpr("less right", "b", expr.right());
                  assertIntEquals("less line", 2, expr.line());
               };

            assertExprError("lessEquals", "line 1: expected expression for '<='", "a <=");

            let expr : ParsedBinaryExpr <- case assertExpr("lessEquals", "a \n<=\n b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("lessEquals", "<=", expr.op());
                  assertIdExpr("lessEquals left", "a", expr.left());
                  assertIdExpr("lessEquals right", "b", expr.right());
                  assertIntEquals("lessEquals line", 2, expr.line());
               };

            assertExprError("equals", "line 1: expected expression for '='", "a =");

            let expr : ParsedBinaryExpr <- case assertExpr("equals", "a \n=\n b") of x : ParsedBinaryExpr => x; esac in
               {
                  assertStringEquals("equals", "=", expr.op());
                  assertIdExpr("equals left", "a", expr.left());
                  assertIdExpr("equals right", "b", expr.right());
                  assertIntEquals("equals line", 2, expr.line());
               };

            assertExprError("not", "line 1: expected expression for 'not'", "not");

            let expr : ParsedUnaryExpr <- case assertExpr("not", "\nnot\n a") of x : ParsedUnaryExpr => x; esac in
               {
                  assertStringEquals("not", "not", expr.op());
                  assertIdExpr("not", "a", expr.expr());
                  assertIntEquals("not line", 2, expr.line());
               };

            assertExprError("paren", "line 1: expected expression for parenthetical expression", "(");
            assertExprError("paren", "line 1: expected ')' for parenthetical expression", "(a");

            assertIdExpr("paren", "a", assertExpr("paren", "(a)"));

            let expr : ParsedIdExpr <- case assertExpr("id", "\na\n") of x : ParsedIdExpr => x; esac in
               {
                  assertStringEquals("id", "a", expr.id());
                  assertIntEquals("id line", 2, expr.line());
               };

            let expr : ParsedConstantIntExpr <- case assertExpr("int", "\n1\n") of x : ParsedConstantIntExpr => x; esac in
               {
                  assertIntEquals("int", 1, expr.value());
                  assertIntEquals("int line", 2, expr.line());
               };

            let expr : ParsedConstantStringExpr <- case assertExpr("string", "\n\"a\"\n") of x : ParsedConstantStringExpr => x; esac in
               {
                  assertStringEquals("string", "a", expr.value());
                  assertIntEquals("string escapes", 0, expr.escapes());
                  assertIntEquals("string line", 2, expr.line());
               };

            let expr : ParsedConstantStringExpr <- case assertExpr("string linefeed", "\n\"\\n\"\n") of x : ParsedConstantStringExpr => x; esac in
               {
                  assertStringEquals("string linefeed", "\n", expr.value());
                  assertIntEquals("string linefeed escapes", "\n".length() - 1, expr.escapes());
                  assertIntEquals("string linefeed line", 2, expr.line());
               };

            let expr : ParsedConstantBoolExpr <- case assertExpr("false", "\nfalse\n") of x : ParsedConstantBoolExpr => x; esac in
               {
                  assertFalse("false", expr.value());
                  assertIntEquals("false line", 2, expr.line());
               };

            let expr : ParsedConstantBoolExpr <- case assertExpr("true", "\ntrue\n") of x : ParsedConstantBoolExpr => x; esac in
               {
                  assertTrue("true", expr.value());
                  assertIntEquals("true line", 2, expr.line());
               };
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

   reportError(line : Int, s : String) : Object {
      if not error then
         errorString <- "line ".concat(new StringUtil.fromInt(line)).concat(": ").concat(s)
      else false fi
   };
};
