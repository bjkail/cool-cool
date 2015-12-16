class Main inherits Test {
   test() : Object {{
      testRegister();
      testMain();
      testAttribute();
      testMethod();
      testExpr();
      testSelf();
      testSelfType();
      testBasicClasses();
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

   newAnalyzerExpr(context : String, program : String) : TestAnalyzer {
      newAnalyzer(context, "class Main { main() : Object { 0 }; newAnalyzerExpr : Object <- ".concat(program).concat("; };"))
   };

   assertAnalyze(context : String, analyzer : TestAnalyzer) : AnalyzedProgram {
      let program : AnalyzedProgram <- analyzer.analyzeTest() in
         {
            assertStringEquals(context.concat(" error"), "", analyzer.errorString());
            assertNotVoid(context.concat(" program"), program);
            program;
         }
   };

   assertAnalyzeExpr(context : String, analyzer : TestAnalyzer) : AnalyzedExpr {
      let program : AnalyzedProgram <- assertAnalyze(context, analyzer),
            type : AnalyzedType <- program.mainMethod().containingType(),
            attr : AnalyzedAttribute <- type.getAttribute("newAnalyzerExpr") in
         attr.expr()
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

   assertAnalyzerExprError(context : String, error : String, program : String) : Object {
      assertAnalyzerErrorImpl(context, error, "class Main { main() : Object { 0 }; assertAnalyzerExprError : Object <- ".concat(program).concat("; };"))
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
                  assertStringEquals("main defining", "Main", mainMethod.containingType().name());
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
                        assertSameType("a defining", type, attr.containingType());
                        assertStringEquals("a id", "a", attr.id());
                        assertSameType("a type", analyzer.objectType(), attr.type());
                        assertVoid("a expr", attr.expr());
                     };

                  let attr : AnalyzedAttribute <- type.getAttribute("b") in
                     {
                        assertSameType("b defining", type, attr.containingType());
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
            assertAnalyzerError("", "line 1: expression type 'Int' does not conform to return type 'Bool' of method 'a'",
                  "class A { a() : Bool { 0 }; };");

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("method",
                     "class A { a() : Object { false }; b(a : Int) : Bool { false }; };"),
                  program : AnalyzedProgram <- analyzer.analyzeTest(),
                  type : AnalyzedType <- program.getType("A") in
               {
                  let method : AnalyzedMethod <- type.getMethod("a") in
                     {
                        assertSameType("a defining", type, method.containingType());
                        assertStringEquals("a id", "a", method.id());
                        assertSameType("a return", analyzer.objectType(), method.returnType());
                        assertIntEquals("a formals", 0, method.formalTypes().size());
                     };

                  let method : AnalyzedMethod <- type.getMethod("b") in
                     {
                        assertSameType("b defining", type, method.containingType());
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
            let analyzer : TestAnalyzer <- newAnalyzerExpr("false", "false"),
                  expr : AnalyzedConstantBoolExpr <- case assertAnalyzeExpr("false", analyzer) of x : AnalyzedConstantBoolExpr => x; esac in
               {
                  assertSameType("false", analyzer.boolType(), expr.type());
                  assertFalse("false value", expr.value());
               };

            let analyzer : TestAnalyzer <- newAnalyzerExpr("true", "true"),
                  expr : AnalyzedConstantBoolExpr <- case assertAnalyzeExpr("true", analyzer) of x : AnalyzedConstantBoolExpr => x; esac in
               {
                  assertSameType("true", analyzer.boolType(), expr.type());
                  assertTrue("true value", expr.value());
               };

            let analyzer : TestAnalyzer <- newAnalyzerExpr("int", "1"),
                  expr : AnalyzedConstantIntExpr <- case assertAnalyzeExpr("int", analyzer) of x : AnalyzedConstantIntExpr => x; esac in
               {
                  assertSameType("int", analyzer.intType(), expr.type());
                  assertIntEquals("int value", 1, expr.value());
               };

            let analyzer : TestAnalyzer <- newAnalyzerExpr("string", "\"a\""),
                  expr : AnalyzedConstantStringExpr <- case assertAnalyzeExpr("string", analyzer) of x : AnalyzedConstantStringExpr => x; esac in
               {
                  assertSameType("string", analyzer.stringType(), expr.type());
                  assertStringEquals("string value", "a", expr.value());
               };

            let analyzer : TestAnalyzer <- newAnalyzer("new", "class Main { main() : Object { new Main }; };"),
                  program : AnalyzedProgram <- assertAnalyze("new", analyzer),
                  mainMethod : AnalyzedMethod <- program.mainMethod(),
                  expr : AnalyzedNewExpr <- case mainMethod.expr() of x : AnalyzedNewExpr => x; esac in
               assertSameType("new", mainMethod.containingType(), expr.type());

            assertAnalyzerError("", "line 1: argument 1 type 'Int' does not conform to type 'Bool' of formal parameter in method 'a' in class 'A' in dispatch expression",
                  "class A { a(a : Bool) : Object { a(0) }; };");

            let analyzer : TestAnalyzer <- newAnalyzer("dispatch",
                     "class Main { main() : Object { new A.a(0) }; }; class A { a(b : Int) : Bool { false }; };"),
                  program : AnalyzedProgram <- assertAnalyze("dispatch", analyzer),
                  mainMethod : AnalyzedMethod <- program.mainMethod(),
                  expr : AnalyzedDispatchExpr <- case mainMethod.expr() of x : AnalyzedDispatchExpr => x; esac in
               {
                  case expr.expr() of x : AnalyzedNewExpr => x; esac;
                  assertSameType("dispatch", analyzer.boolType(), expr.type());

                  let method : AnalyzedMethod <- expr.method() in
                     if not method = program.getType("A").getMethod("a") then
                        failContext("dispatch method", "expected=A.a, actual="
                              .concat(method.containingType().name())
                              .concat(".").concat(method.id()))
                     else false fi;

                  let argIter : Iterator <- expr.arguments().iterator() in
                     {
                        case getIteratorNext(argIter) of x : AnalyzedConstantIntExpr => x; esac;
                        assertFalse("dispatch args", argIter.next());
                     };
               };

            assertAnalyzerError("", "line 1: undefined type 'B' for static dispatch expression",
                  "class A { a(a : Bool) : Object { a@B.b() }; };");
            assertAnalyzerError("", "line 1: expression type 'Bool' does not conform to static type 'Int' in dispatch expression",
                  "class A { a(a : Bool) : Object { a@Int.copy() }; };");

            let analyzer : TestAnalyzer <- newAnalyzer("static dispatch",
                     "class Main inherits A { main() : Object { new Main@A.a(0) }; a(b : Int) : Bool { false }; };"
                     .concat("class A { a(b : Int) : Bool { false }; };")),
                  program : AnalyzedProgram <- assertAnalyze("static dispatch", analyzer),
                  mainMethod : AnalyzedMethod <- program.mainMethod(),
                  expr : AnalyzedDispatchExpr <- case mainMethod.expr() of x : AnalyzedDispatchExpr => x; esac in
               {
                  case expr.expr() of x : AnalyzedNewExpr => x; esac;
                  assertSameType("static dispatch", analyzer.boolType(), expr.type());

                  let method : AnalyzedMethod <- expr.method() in
                     if not method = program.getType("A").getMethod("a") then
                        failContext("static dispatch method", "expected=A.a, actual="
                              .concat(method.containingType().name())
                              .concat(".").concat(method.id()))
                     else false fi;

                  let argIter : Iterator <- expr.arguments().iterator() in
                     {
                        case getIteratorNext(argIter) of x : AnalyzedConstantIntExpr => x; esac;
                        assertFalse("static dispatch", argIter.next());
                     };
               };

            assertAnalyzerError("", "line 1: expression type 'Int' is not type 'Bool' for predicate in 'if' expression",
                  "class A { a() : Object { if 0 then 0 else 0 fi }; };");

            let analyzer : TestAnalyzer <- newAnalyzer("if",
                     "class Main { main() : Object { if true then 0 else 0 fi }; };"),
                  program : AnalyzedProgram <- assertAnalyze("dispatch", analyzer),
                  mainMethod : AnalyzedMethod <- program.mainMethod(),
                  expr : AnalyzedIfExpr <- case mainMethod.expr() of x : AnalyzedIfExpr => x; esac in
               {
                  case expr.then_() of x : AnalyzedConstantIntExpr => x; esac;
                  case expr.else_() of x : AnalyzedConstantIntExpr => x; esac;
                  assertSameType("if", analyzer.intType(), expr.type());
               };

            let analyzer : TestAnalyzer <- newAnalyzer("if",
                     "class Main { main() : Object { if true then 0 else false fi }; };"),
                  program : AnalyzedProgram <- assertAnalyze("if", analyzer),
                  mainMethod : AnalyzedMethod <- program.mainMethod(),
                  expr : AnalyzedIfExpr <- case mainMethod.expr() of x : AnalyzedIfExpr => x; esac in
               assertSameType("if", analyzer.objectType(), expr.type());

            let analyzer : TestAnalyzer <- newAnalyzer("if",
                     "class Main { main() : Object { if true then new A else new B fi }; }; "
                     .concat("class A inherits B { a : Bool; }; class B { b : Bool; };")),
                  program : AnalyzedProgram <- assertAnalyze("if", analyzer),
                  mainMethod : AnalyzedMethod <- program.mainMethod(),
                  expr : AnalyzedIfExpr <- case mainMethod.expr() of x : AnalyzedIfExpr => x; esac in
               assertSameType("if", program.getType("B"), expr.type());

            let analyzer : TestAnalyzer <- newAnalyzer("block",
                     "class Main { main() : Object {{ 0; }}; }; "),
                  program : AnalyzedProgram <- assertAnalyze("if", analyzer),
                  mainMethod : AnalyzedMethod <- program.mainMethod(),
                  expr : AnalyzedBlockExpr <- case mainMethod.expr() of x : AnalyzedBlockExpr => x; esac in
               {
                  assertSameType("block", analyzer.intType(), expr.type());

                  let exprIter : Iterator <- expr.exprs().iterator() in
                     {
                        case getIteratorNext(exprIter) of x : AnalyzedConstantIntExpr => x; esac;
                        assertFalse("block exprs", exprIter.next());
                     };
               };

            let analyzer : TestAnalyzer <- newAnalyzer("block 2",
                     "class Main { main() : Object {{ 0; false; }}; }; "),
                  program : AnalyzedProgram <- assertAnalyze("block 2", analyzer),
                  mainMethod : AnalyzedMethod <- program.mainMethod(),
                  expr : AnalyzedBlockExpr <- case mainMethod.expr() of x : AnalyzedBlockExpr => x; esac in
               {
                  assertSameType("block 2", analyzer.boolType(), expr.type());

                  let exprIter : Iterator <- expr.exprs().iterator() in
                     {
                        case getIteratorNext(exprIter) of x : AnalyzedConstantIntExpr => x; esac;
                        case getIteratorNext(exprIter) of x : AnalyzedConstantBoolExpr => x; esac;
                        assertFalse("block 2 exprs", exprIter.next());
                     };
               };

            assertAnalyzerExprError("let", "line 1: undefined variable 'a'",
                  "let a : Object <- a in 0");
            assertAnalyzerExprError("", "line 1: expression type 'Bool' does not conform to type 'Int' of variable 'a' in 'let' expression",
                  "let a : Int <- false in 0");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("let", "let b : Bool in b"),
                  expr : AnalyzedLetExpr <- case assertAnalyzeExpr("let", analyzer) of x : AnalyzedLetExpr => x; esac in
               {
                  case expr.expr() of x : AnalyzedObjectExpr => x; esac;
                  assertSameType("let", analyzer.boolType(), expr.type());

                  let varIter : Iterator <- expr.vars().iterator() in
                     {
                        let var : AnalyzedLetVar <- case getIteratorNext(varIter) of x : AnalyzedLetVar => x; esac in
                           {
                              case var.object() of x : AnalyzedVarObject => x; esac;
                              assertVoid("let var expr", var.expr());
                           };

                        assertFalse("let vars", varIter.next());
                     };
               };

            let analyzer : TestAnalyzer <- newAnalyzerExpr("let 2", "let b : Bool, c : Bool <- b in c"),
                  expr : AnalyzedLetExpr <- case assertAnalyzeExpr("let 2", analyzer) of x : AnalyzedLetExpr => x; esac in
               {
                  case expr.expr() of x : AnalyzedObjectExpr => x; esac;
                  assertSameType("let 2", analyzer.boolType(), expr.type());

                  let varIter : Iterator <- expr.vars().iterator() in
                     {
                        let var : AnalyzedLetVar <- case getIteratorNext(varIter) of x : AnalyzedLetVar => x; esac in
                           {
                              case var.object() of x : AnalyzedVarObject => x; esac;
                              assertVoid("let 2 var 1 expr", var.expr());
                           };

                        let var : AnalyzedLetVar <- case getIteratorNext(varIter) of x : AnalyzedLetVar => x; esac in
                           {
                              case var.object() of x : AnalyzedVarObject => x; esac;
                              case var.expr() of x : AnalyzedObjectExpr => x; esac;
                           };

                        assertFalse("let 2 vars", varIter.next());
                     };
               };

            assertAnalyzerExprError("case", "line 1: duplicate type 'Int' in 'case' expression",
                  "case 0 of a : Int => a; a : Int => a; esac");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("case", "case 0 of a : Bool => a; esac"),
                  expr : AnalyzedCaseExpr <- case assertAnalyzeExpr("case", analyzer) of x : AnalyzedCaseExpr => x; esac in
               {
                  case expr.expr() of x : AnalyzedConstantIntExpr => x; esac;
                  assertSameType("case", analyzer.boolType(), expr.type());

                  let branchIter : Iterator <- expr.branches().iterator() in
                     {
                        let branch : AnalyzedCaseBranch <- case getIteratorNext(branchIter) of x : AnalyzedCaseBranch => x; esac in
                           {
                              assertSameType("case branch", analyzer.boolType(), branch.checkType());
                              case branch.expr() of x : AnalyzedObjectExpr => x; esac;
                           };

                        assertFalse("case branches", branchIter.next());
                     };
               };

            let analyzer : TestAnalyzer <- newAnalyzerExpr("case 2", "case 0 of a : Bool => a; a : Int => a; esac"),
                  expr : AnalyzedCaseExpr <- case assertAnalyzeExpr("case", analyzer) of x : AnalyzedCaseExpr => x; esac in
               {
                  case expr.expr() of x : AnalyzedConstantIntExpr => x; esac;
                  assertSameType("case 2", analyzer.objectType(), expr.type());

                  let branchIter : Iterator <- expr.branches().iterator() in
                     {
                        let branch : AnalyzedCaseBranch <- case getIteratorNext(branchIter) of x : AnalyzedCaseBranch => x; esac in
                           {
                              assertSameType("case 2 branch 1", analyzer.boolType(), branch.checkType());
                              case branch.expr() of x : AnalyzedObjectExpr => x; esac;
                           };

                        let branch : AnalyzedCaseBranch <- case getIteratorNext(branchIter) of x : AnalyzedCaseBranch => x; esac in
                           {
                              assertSameType("case 2 branch 2", analyzer.intType(), branch.checkType());
                              case branch.expr() of x : AnalyzedObjectExpr => x; esac;
                           };

                        assertFalse("case 2 branches", branchIter.next());
                     };
               };

            let analyzer : TestAnalyzer <- newAnalyzer("case join",
                     "class Main { main() : Object { case 0 of a : A => a; b : B => b; esac }; }; "
                     .concat("class A inherits B { a : Bool; }; class B { b : Bool; };")),
                  program : AnalyzedProgram <- assertAnalyze("dispatch", analyzer),
                  mainMethod : AnalyzedMethod <- program.mainMethod(),
                  expr : AnalyzedCaseExpr <- case mainMethod.expr() of x : AnalyzedCaseExpr => x; esac in
               assertSameType("if", program.getType("B"), expr.type());

            assertAnalyzerError("", "line 1: expression type 'Int' is not type 'Bool' for predicate in 'while' expression",
                  "class A { a() : Object { while 0 loop 0 pool }; };");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("while", "while false loop 0 pool"),
                  expr : AnalyzedWhileExpr <- case assertAnalyzeExpr("while", analyzer) of x : AnalyzedWhileExpr => x; esac in
               {
                  case expr.expr() of x : AnalyzedConstantBoolExpr => x; esac;
                  case expr.loop_() of x : AnalyzedConstantIntExpr => x; esac;
                  assertSameType("while", analyzer.objectType(), expr.type());
               };

            let analyzer : TestAnalyzer <- newAnalyzerExpr("isvoid", "isvoid 0"),
                  expr : AnalyzedUnaryExpr <- case assertAnalyzeExpr("isvoid", analyzer) of x : AnalyzedUnaryExpr => x; esac in
               {
                  assertStringEquals("isvoid", "isvoid", expr.op());
                  assertSameType("isvoid", analyzer.boolType(), expr.type());
                  case expr.expr() of x : AnalyzedConstantIntExpr => x; esac;
               };

            assertAnalyzerExprError("", "line 1: expression type 'Int' is not type 'Bool' for 'not' expression",
                  "not 0");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("not", "not false"),
                  expr : AnalyzedUnaryExpr <- case assertAnalyzeExpr("not", analyzer) of x : AnalyzedUnaryExpr => x; esac in
               {
                  assertStringEquals("not", "not", expr.op());
                  assertSameType("not", analyzer.boolType(), expr.type());
                  case expr.expr() of x : AnalyzedConstantBoolExpr => x; esac;
               };

            assertAnalyzerExprError("", "line 1: left expression type 'Bool' is not type 'Int' for '<' expression",
                  "false < 0");
            assertAnalyzerExprError("", "line 1: right expression type 'Bool' is not type 'Int' for '<' expression",
                  "0 < false");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("less", "0 < \"\".length()"),
                  expr : AnalyzedBinaryExpr <- case assertAnalyzeExpr("less", analyzer) of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("less", "<", expr.op());
                  assertSameType("less", analyzer.boolType(), expr.type());
                  case expr.left() of x : AnalyzedConstantIntExpr => x; esac;
                  case expr.right() of x : AnalyzedDispatchExpr => x; esac;
               };

            assertAnalyzerExprError("", "line 1: left expression type 'Bool' is not type 'Int' for '<=' expression",
                  "false <= 0");
            assertAnalyzerExprError("", "line 1: right expression type 'Bool' is not type 'Int' for '<=' expression",
                  "0 <= false");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("lessEqual", "0 <= \"\".length()"),
                  expr : AnalyzedBinaryExpr <- case assertAnalyzeExpr("lessEqual", analyzer) of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("lessEqual", "<=", expr.op());
                  assertSameType("lessEqual", analyzer.boolType(), expr.type());
                  case expr.left() of x : AnalyzedConstantIntExpr => x; esac;
                  case expr.right() of x : AnalyzedDispatchExpr => x; esac;
               };

            assertAnalyzerExprError("", "line 1: expression type 'Bool' is not type 'Int' for '~' expression",
                  "~false");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("complement", "~1"),
                  expr : AnalyzedUnaryExpr <- case assertAnalyzeExpr("complement", analyzer) of x : AnalyzedUnaryExpr => x; esac in
               {
                  assertStringEquals("complement", "~", expr.op());
                  assertSameType("complement", analyzer.intType(), expr.type());
                  case expr.expr() of x : AnalyzedConstantIntExpr => x; esac;
               };

            assertAnalyzerExprError("", "line 1: left expression type 'Bool' is not type 'Int' for '*' expression",
                  "false * 0");
            assertAnalyzerExprError("", "line 1: right expression type 'Bool' is not type 'Int' for '*' expression",
                  "0 * false");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("multiply", "0 * \"\".length()"),
                  expr : AnalyzedBinaryExpr <- case assertAnalyzeExpr("multiply", analyzer) of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("multiply", "*", expr.op());
                  assertSameType("multiply", analyzer.intType(), expr.type());
                  case expr.left() of x : AnalyzedConstantIntExpr => x; esac;
                  case expr.right() of x : AnalyzedDispatchExpr => x; esac;
               };

            assertAnalyzerExprError("", "line 1: left expression type 'Bool' is not type 'Int' for '+' expression",
                  "false + 0");
            assertAnalyzerExprError("", "line 1: right expression type 'Bool' is not type 'Int' for '+' expression",
                  "0 + false");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("add", "0 + \"\".length()"),
                  expr : AnalyzedBinaryExpr <- case assertAnalyzeExpr("add", analyzer) of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("add", "+", expr.op());
                  assertSameType("add", analyzer.intType(), expr.type());
                  case expr.left() of x : AnalyzedConstantIntExpr => x; esac;
                  case expr.right() of x : AnalyzedDispatchExpr => x; esac;
               };

            assertAnalyzerExprError("", "line 1: left expression type 'Bool' is not type 'Int' for '-' expression",
                  "false - 0");
            assertAnalyzerExprError("", "line 1: right expression type 'Bool' is not type 'Int' for '-' expression",
                  "0 - false");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("subtract", "0 - \"\".length()"),
                  expr : AnalyzedBinaryExpr <- case assertAnalyzeExpr("subtract", analyzer) of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("subtract", "-", expr.op());
                  assertSameType("subtract", analyzer.intType(), expr.type());
                  case expr.left() of x : AnalyzedConstantIntExpr => x; esac;
                  case expr.right() of x : AnalyzedDispatchExpr => x; esac;
               };

            assertAnalyzerExprError("", "line 1: left expression type 'Bool' is not type 'Int' for '/' expression",
                  "false / 0");
            assertAnalyzerExprError("", "line 1: right expression type 'Bool' is not type 'Int' for '/' expression",
                  "0 / false");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("divide", "0 / \"\".length()"),
                  expr : AnalyzedBinaryExpr <- case assertAnalyzeExpr("divide", analyzer) of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("divide", "/", expr.op());
                  assertSameType("divide", analyzer.intType(), expr.type());
                  case expr.left() of x : AnalyzedConstantIntExpr => x; esac;
                  case expr.right() of x : AnalyzedDispatchExpr => x; esac;
               };

            assertAnalyzerExprError("", "line 1: left expression type 'Int' is not the same as right expression type 'String' in '=' expression",
                  "0 = \"\"");
            assertAnalyzerExprError("", "line 1: left expression type 'String' is not the same as right expression type 'Int' in '=' expression",
                  "\"\" = 0");
            assertAnalyzerExprError("", "line 1: left expression type 'Int' is not the same as right expression type 'Bool' in '=' expression",
                  "0 = false");
            assertAnalyzerExprError("", "line 1: left expression type 'Bool' is not the same as right expression type 'Int' in '=' expression",
                  "false = 0");
            assertAnalyzerExprError("", "line 1: left expression type 'String' is not the same as right expression type 'Bool' in '=' expression",
                  "\"\" = false");
            assertAnalyzerExprError("", "line 1: left expression type 'Bool' is not the same as right expression type 'String' in '=' expression",
                  "false = \"\"");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("equal", "0 = \"\".length()"),
                  expr : AnalyzedBinaryExpr <- case assertAnalyzeExpr("equal", analyzer) of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("equal", "=", expr.op());
                  assertSameType("equal", analyzer.boolType(), expr.type());
                  case expr.left() of x : AnalyzedConstantIntExpr => x; esac;
                  case expr.right() of x : AnalyzedDispatchExpr => x; esac;
               };

            let analyzer : TestAnalyzer <- newAnalyzerExpr("equal", "\"\" = 0.type_name()"),
                  expr : AnalyzedBinaryExpr <- case assertAnalyzeExpr("equal", analyzer) of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("equal", "=", expr.op());
                  assertSameType("equal", analyzer.boolType(), expr.type());
                  case expr.left() of x : AnalyzedConstantStringExpr => x; esac;
                  case expr.right() of x : AnalyzedDispatchExpr => x; esac;
               };

            let analyzer : TestAnalyzer <- newAnalyzerExpr("equal", "false = not false"),
                  expr : AnalyzedBinaryExpr <- case assertAnalyzeExpr("equal", analyzer) of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("equal", "=", expr.op());
                  assertSameType("equal", analyzer.boolType(), expr.type());
                  case expr.left() of x : AnalyzedConstantBoolExpr => x; esac;
                  case expr.right() of x : AnalyzedUnaryExpr => x; esac;
               };

            let analyzer : TestAnalyzer <- newAnalyzerExpr("equal", "new Object = new Object"),
                  expr : AnalyzedBinaryExpr <- case assertAnalyzeExpr("equal", analyzer) of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("equal", "=", expr.op());
                  assertSameType("equal", analyzer.boolType(), expr.type());
                  case expr.left() of x : AnalyzedNewExpr => x; esac;
                  case expr.right() of x : AnalyzedNewExpr => x; esac;
               };

            let analyzer : TestAnalyzer <- newAnalyzer("equal",
                     "class Main { main() : Object { new A = new B }; };"
                     .concat("class A { a : Bool; }; class B { b : Bool; };")),
                  program : AnalyzedProgram <- assertAnalyze("equal", analyzer),
                  expr : AnalyzedBinaryExpr <- case program.mainMethod().expr() of x : AnalyzedBinaryExpr => x; esac in
               {
                  assertStringEquals("equal", "=", expr.op());
                  assertSameType("equal", analyzer.boolType(), expr.type());
                  case expr.left() of x : AnalyzedNewExpr => x; esac;
                  case expr.right() of x : AnalyzedNewExpr => x; esac;
               };
         }
      else false fi
   };

   testSelf() : Object {
      if begin("self") then
         {
            assertAnalyzerExprError("", "line 1: invalid assignment to 'self' variable",
                  "self <- new SELF_TYPE");
            assertAnalyzerExprError("", "line 1: redefinition of 'self' variable in 'let' expression",
                  "let self : Object in 0");
            assertAnalyzerExprError("", "line 1: redefinition of 'self' variable in 'case' expression",
                  "case 0 of self : Object => 0; esac");
            assertAnalyzerError("", "line 1: invalid formal parameter name 'self'",
                  "class A { a(self : Object) : Object { 0 }; };");

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("new", "class A { a : Object <- self; };"),
                  program : AnalyzedProgram <- assertAnalyze("reference", analyzer),
                  type : AnalyzedType <- program.getType("A"),
                  expr : AnalyzedObjectExpr <- case type.getAttribute("a").expr() of x : AnalyzedObjectExpr => x; esac in
               {
                  case expr.object() of x : AnalyzedSelfObject => x; esac;
                  assertSameType("reference", type.selfTypeType(), expr.type());
               };
         }
      else false fi
   };

   testSelfType() : Object {
      if begin("selfType") then
         {
            assertAnalyzerError("", "line 1: definition of type 'SELF_TYPE'",
                  "class SELF_TYPE { a : Bool; };");
            assertAnalyzerError("", "line 1: invalid type 'SELF_TYPE' for 'inherits'",
                  "class A inherits SELF_TYPE { a : Bool; };");
            assertAnalyzerError("", "line 1: invalid type 'SELF_TYPE' for formal parameter #1",
                  "class A { a(b : SELF_TYPE) : Object { 0 }; };");
            assertAnalyzerError("", "line 1: invalid type 'SELF_TYPE' for static dispatch expression",
                  "class A { a(a : A) : Object { a@SELF_TYPE.copy() }; };");
            assertAnalyzerExprError("", "line 1: invalid type 'SELF_TYPE' for 'case' branch",
                  "case 0 of x : SELF_TYPE => x; esac");

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("new", "class A { a : Object <- new SELF_TYPE; };"),
                  program : AnalyzedProgram <- assertAnalyze("new", analyzer),
                  type : AnalyzedType <- program.getType("A"),
                  attr : AnalyzedAttribute <- type.getAttribute("a") in
               assertSameType("new", type.selfTypeType(), attr.expr().type());

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("return", "class A { a() : SELF_TYPE { self }; b() : A { a() }; };"),
                  program : AnalyzedProgram <- assertAnalyze("return", analyzer),
                  type : AnalyzedType <- program.getType("A"),
                  methodA : AnalyzedMethod <- type.getMethod("a"),
                  methodB : AnalyzedMethod <- type.getMethod("b") in
               {
                  assertSameType("return", type.selfTypeType(), methodA.returnType());
                  assertSameType("return expr", type, methodB.expr().type());
               };

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("let", "class A { a() : Object { let a : SELF_TYPE in a }; };"),
                  program : AnalyzedProgram <- assertAnalyze("let", analyzer),
                  type : AnalyzedType <- program.getType("A"),
                  method : AnalyzedMethod <- type.getMethod("a") in
               assertSameType("let", type.selfTypeType(), method.expr().type());

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("attribute", "class A { a : SELF_TYPE; };"),
                  program : AnalyzedProgram <- assertAnalyze("attribute", analyzer),
                  type : AnalyzedType <- program.getType("A"),
                  attr : AnalyzedAttribute <- type.getAttribute("a") in
               assertSameType("attribute", type.selfTypeType(), attr.type());

            assertAnalyzerError("", "line 1: expression type 'A' does not conform to return type 'SELF_TYPE' of method 'a'",
                  "class A { a() : SELF_TYPE { a() }; };");

            assertAnalyze("method", newAnalyzerDefaultMain("method", "class A { a : SELF_TYPE <- let b : SELF_TYPE in b; };"));
            assertAnalyze("method", newAnalyzerDefaultMain("method", "class A { a() : Object { let a : SELF_TYPE in a.a() }; };"));
            assertAnalyze("method", newAnalyzerDefaultMain("method", "class A { a() : Object { new SELF_TYPE = new SELF_TYPE }; };"));
         }
      else false fi
   };

   testBasicClasses() : Object {
      if begin("basicClasses") then
         {
            -- Not specified, but...
            assertAnalyzerError("", "line 1: redefinition of class 'Object'",
                  "class Object { a : Bool; };");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("abort", "new Object.abort()"),
                  expr : AnalyzedExpr <- assertAnalyzeExpr("abort", analyzer) in
               assertSameType("abort", analyzer.objectType(), expr.type());

            let analyzer : TestAnalyzer <- newAnalyzerExpr("type_name", "new Object.type_name()"),
                  expr : AnalyzedExpr <- assertAnalyzeExpr("type_name", analyzer) in
               assertSameType("type_name", analyzer.stringType(), expr.type());

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("copy", "class A { a : Object <- new A.copy(); };"),
                  program : AnalyzedProgram <- assertAnalyze("copy", analyzer),
                  type : AnalyzedType <- program.getType("A"),
                  expr : AnalyzedExpr <- type.getAttribute("a").expr() in
               assertSameType("copy", type, expr.type());

            assertAnalyzerError("", "line 1: redefinition of class 'IO'",
                  "class IO { a : Bool; };");

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("out_string", "class A inherits IO { a : Object <- new A.out_string(\"\"); };"),
                  program : AnalyzedProgram <- assertAnalyze("out_string", analyzer),
                  type : AnalyzedType <- program.getType("A"),
                  expr : AnalyzedExpr <- type.getAttribute("a").expr() in
               assertSameType("out_string", type, expr.type());

            let analyzer : TestAnalyzer <- newAnalyzerDefaultMain("out_int", "class A inherits IO { a : Object <- new A.out_int(1); };"),
                  program : AnalyzedProgram <- assertAnalyze("out_int", analyzer),
                  type : AnalyzedType <- program.getType("A"),
                  expr : AnalyzedExpr <- type.getAttribute("a").expr() in
               assertSameType("out_int", type, expr.type());

            let analyzer : TestAnalyzer <- newAnalyzerExpr("in_string", "new IO.in_string()"),
                  expr : AnalyzedExpr <- assertAnalyzeExpr("in_string", analyzer) in
               assertSameType("in_string", analyzer.stringType(), expr.type());

            let analyzer : TestAnalyzer <- newAnalyzerExpr("in_int", "new IO.in_int()"),
                  expr : AnalyzedExpr <- assertAnalyzeExpr("in_int", analyzer) in
               assertSameType("in_int", analyzer.intType(), expr.type());

            assertAnalyzerError("", "line 1: invalid type 'Int' for 'inherits'",
                  "class A inherits Int { a : Bool; };");
            assertAnalyzerError("", "line 1: redefinition of class 'Int'",
                  "class Int { a : Bool; };");

            assertAnalyzerError("", "line 1: invalid type 'String' for 'inherits'",
                  "class A inherits String { a : Bool; };");
            assertAnalyzerError("", "line 1: redefinition of class 'String'",
                  "class String { a : Bool; };");

            let analyzer : TestAnalyzer <- newAnalyzerExpr("length", "\"\".length()"),
                  expr : AnalyzedExpr <- assertAnalyzeExpr("length", analyzer) in
               assertSameType("length", analyzer.intType(), expr.type());

            let analyzer : TestAnalyzer <- newAnalyzerExpr("concat", "\"\".concat(\"\")"),
                  expr : AnalyzedExpr <- assertAnalyzeExpr("concat", analyzer) in
               assertSameType("concat", analyzer.stringType(), expr.type());

            let analyzer : TestAnalyzer <- newAnalyzerExpr("substr", "\"\".substr(0, 0)"),
                  expr : AnalyzedExpr <- assertAnalyzeExpr("substr", analyzer) in
               assertSameType("substr", analyzer.stringType(), expr.type());

            assertAnalyzerError("", "line 1: invalid type 'Bool' for 'inherits'",
                  "class A inherits Bool { a : Bool; };");
            assertAnalyzerError("", "line 1: redefinition of class 'Bool'",
                  "class Bool { a : Bool; };");
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
