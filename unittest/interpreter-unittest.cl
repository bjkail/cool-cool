class Main inherits Test {
   test() : Object {{
      testBasic();
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

   testBasic() : Object {
      if begin("basic") then
         {
            let value : InterpreterBoolValue <- case interpretExpr("int", "false") of x : InterpreterBoolValue => x; esac in
               assertFalse("false", value.value());

            let value : InterpreterBoolValue <- case interpretExpr("int", "true") of x : InterpreterBoolValue => x; esac in
               assertTrue("true", value.value());

            let value : InterpreterIntValue <- case interpretExpr("int", "1") of x : InterpreterIntValue => x; esac in
               assertIntEquals("int", 1, value.value());

            let value : InterpreterStringValue <- case interpretExpr("int", "\"a\"") of x : InterpreterStringValue => x; esac in
               assertStringEquals("string", "a", value.value());
         }
      else false fi
   };
};
