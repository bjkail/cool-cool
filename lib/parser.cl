class ParsedNode {
   line : Int;
   line() : Int { line };
};

class ParsedProgram {
   classes : Collection <- new LinkedList;
   classes() : Collection { classes };

   init(classes_ : Collection) : SELF_TYPE {{
      classes <- classes_;
      self;
   }};
};

class ParsedClass inherits ParsedNode {
   type : String;
   type() : String { type };

   inherits_ : String;
   inherits_() : String { inherits_ };

   features : Collection <- new LinkedList;
   features() : Collection { features };

   init(line_ : Int, type_ : String, inherits__ : String, features_ : Collection) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      inherits_ <- inherits__;
      features <- features_;
      self;
   }};
};

class ParsedFeature inherits ParsedNode {
   id : String;
   id() : String { id };

   asAttribute() : ParsedAttribute { let void : ParsedAttribute in void };
   asMethod() : ParsedMethod { let void : ParsedMethod in void };
};

class ParsedAttribute inherits ParsedFeature {
   type : String;
   type() : String { type };

   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   init(line_ : Int, id_ : String, type_ : String, expr_ : ParsedExpr) : SELF_TYPE {{
      line <- line_;
      id <- id_;
      type <- type_;
      expr <- expr_;
      self;
   }};

   asAttribute() : ParsedAttribute { self };
};

class ParsedFormal inherits ParsedNode {
   id : String;
   id() : String { id };

   type : String;
   type() : String { type };

   init(line_ : Int, id_ : String, type_ : String) : SELF_TYPE {{
      line <- line_;
      id <- id_;
      type <- type_;
      self;
   }};
};

class ParsedMethod inherits ParsedFeature {
   formals : Collection;
   formals() : Collection { formals };

   returnType : String;
   returnType() : String { returnType };

   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   init(line_ : Int, id_ : String, formals_ : Collection, returnType_ : String, expr_ : ParsedExpr) : SELF_TYPE {{
      line <- line_;
      id <- id_;
      formals <- formals_;
      returnType <- returnType_;
      expr <- expr_;
      self;
   }};

   asMethod() : ParsedMethod { self };
};

class ParsedExprVisitor {
   visitBlock(expr : ParsedBlockExpr) : Object { new Object.abort() };
   visitIf(expr : ParsedIfExpr) : Object { new Object.abort() };
   visitWhile(expr : ParsedWhileExpr) : Object { new Object.abort() };
   visitLet(expr : ParsedLetExpr) : Object { new Object.abort() };
   visitCase(expr : ParsedCaseExpr) : Object { new Object.abort() };
   visitAssignment(expr : ParsedAssignmentExpr) : Object { new Object.abort() };
   visitId(expr : ParsedIdExpr) : Object { new Object.abort() };
   visitNew(expr : ParsedNewExpr) : Object { new Object.abort() };
   visitDispatch(expr : ParsedDispatchExpr) : Object { new Object.abort() };
   visitUnary(expr : ParsedUnaryExpr) : Object { new Object.abort() };
   visitBinary(expr : ParsedBinaryExpr) : Object { new Object.abort() };
   visitConstantBool(expr : ParsedConstantBoolExpr) : Object { new Object.abort() };
   visitConstantInt(expr : ParsedConstantIntExpr) : Object { new Object.abort() };
   visitConstantString(expr : ParsedConstantStringExpr) : Object { new Object.abort() };
};

class ParsedExpr inherits ParsedNode {
   accept(visitor : ParsedExprVisitor) : Object { new Object.abort() };
};

class ParsedErrorExpr inherits ParsedExpr {
   x() : Bool { false };
};

class ParsedBlockExpr inherits ParsedExpr {
   exprs : Collection;
   exprs() : Collection { exprs };

   init(line_ : Int, exprs_ : Collection) : SELF_TYPE {{
      line <- line_;
      exprs <- exprs_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitBlock(self) };
};

class ParsedIfExpr inherits ParsedExpr {
   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   then_ : ParsedExpr;
   then_() : ParsedExpr { then_ };

   else_ : ParsedExpr;
   else_() : ParsedExpr { else_ };

   init(line_ : Int, expr_ : ParsedExpr, then__ : ParsedExpr, else__ : ParsedExpr) : SELF_TYPE {{
      line <- line_;
      expr <- expr_;
      then_ <- then__;
      else_ <- else__;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitIf(self) };
};

class ParsedWhileExpr inherits ParsedExpr {
   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   loop_ : ParsedExpr;
   loop_() : ParsedExpr { loop_ };

   init(line_ : Int, expr_ : ParsedExpr, loop__ : ParsedExpr) : SELF_TYPE {{
      line <- line_;
      expr <- expr_;
      loop_ <- loop__;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitWhile(self) };
};

class ParsedVar inherits ParsedNode {
   id : String;
   id() : String { id };

   type : String;
   type() : String { id };

   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   init(line_ : Int, id_ : String, type_ : String, expr_ : ParsedExpr) : SELF_TYPE {{
      line <- line_;
      id <- id_;
      type <- type_;
      expr <- expr_;
      self;
   }};
};

class ParsedLetExpr inherits ParsedExpr {
   vars : Collection;
   vars() : Collection { vars };

   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   init(line_ : Int, vars_ : Collection, expr_ : ParsedExpr) : SELF_TYPE {{
      line <- line_;
      vars <- vars_;
      expr <- expr_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitLet(self) };
};

class ParsedCaseExpr inherits ParsedExpr {
   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   branches : Collection;
   branches() : Collection { branches };

   init(line_ : Int, expr_ : ParsedExpr, branches_ : Collection) : SELF_TYPE {{
      line <- line_;
      expr <- expr_;
      branches <- branches_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitCase(self) };
};

class ParsedAssignmentExpr inherits ParsedExpr {
   id : String;
   id() : String { id };

   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   init(line_ : Int, id_ : String, expr_ : ParsedExpr) : SELF_TYPE {{
      line <- line_;
      id <- id_;
      expr <- expr_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitAssignment(self) };
};

class ParsedIdExpr inherits ParsedExpr {
   id : String;
   id() : String { id };

   init(line_ : Int, id_ : String) : SELF_TYPE {{
      line <- line_;
      id <- id_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitId(self) };
};

class ParsedNewExpr inherits ParsedExpr {
   type : String;
   type() : String { type };

   init(line_ : Int, type_ : String) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitNew(self) };
};

class ParsedDispatchExpr inherits ParsedExpr {
   target : ParsedExpr;
   target() : ParsedExpr { target };

   type : String;
   type() : String { type };

   id : String;
   id() : String { id };

   arguments : Collection;
   arguments() : Collection { arguments };

   init(line_ : Int, target_ : ParsedExpr, type_ : String, id_ : String, arguments_ : Collection) : SELF_TYPE {{
      line <- line_;
      target <- target_;
      type <- type_;
      id <- id_;
      arguments <- arguments_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitDispatch(self) };
};

class ParsedUnaryExpr inherits ParsedExpr {
   op : String;
   op() : String { op };

   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   init(line_ : Int, op_ : String, expr_ : ParsedExpr) : SELF_TYPE {{
      line <- line_;
      op <- op_;
      expr <- expr_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitUnary(self) };
};

class ParsedBinaryExpr inherits ParsedExpr {
   op : String;
   op() : String { op };

   left : ParsedExpr;
   left() : ParsedExpr { left };

   right : ParsedExpr;
   right() : ParsedExpr { right };

   init(line_ : Int, op_ : String, left_ : ParsedExpr, right_ : ParsedExpr) : SELF_TYPE {{
      line <- line_;
      op <- op_;
      left <- left_;
      right <- right_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitBinary(self) };
};

class ParsedConstantBoolExpr inherits ParsedExpr {
   value : Bool;
   value() : Bool { value };

   init(line_ : Int, value_ : Bool) : SELF_TYPE {{
      line <- line_;
      value <- value_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitConstantBool(self) };
};

class ParsedConstantIntExpr inherits ParsedExpr {
   value : Int;
   value() : Int { value };

   init(line_ : Int, value_ : Int) : SELF_TYPE {{
      line <- line_;
      value <- value_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitConstantInt(self) };
};

class ParsedConstantStringExpr inherits ParsedExpr {
   value : String;
   value() : String { value };

   init(line_ : Int, value_ : String) : SELF_TYPE {{
      line <- line_;
      value <- value_;
      self;
   }};

   accept(visitor : ParsedExprVisitor) : Object { visitor.visitConstantString(self) };
};

class Parser {
   stringUtil : StringUtil <- new StringUtil;
   tokenizer : Tokenizer;
   token : Token;
   error : Bool;

   init(tokenizer_ : Tokenizer) : SELF_TYPE {{
      tokenizer <- tokenizer_;
      self;
   }};

   readTokenImpl() : Token {
      let token : Token <- tokenizer.next(),
            tokenError : TokenError <- token.asError() in
         {
            while not isvoid tokenError loop
               {
                  error(tokenError.value());
                  token <- tokenizer.next();
                  tokenError <- token.asError();
               }
            pool;
            token;
         }
   };

   readToken() : Token {
      if isvoid token then
         readTokenImpl()
      else
         let result : Token <- token in
            {
               token <- let void : Token in void;
               result;
            }
      fi
   };

   skipToken() : Bool {{
      if isvoid token then
         abort()
      else
         token <- let void : Token in void
      fi;
      true;
   }};

   peekToken() : Token {
      if isvoid token then
         token <- readTokenImpl()
      else
         token
      fi
   };

   line() : Int {
      tokenizer.line()
   };

   error(s : String) : Bool {{
      if not error then
         {
            new IO.out_string("PARSE ERROR: ")
                  .out_string(tokenizer.lineMap().lineToString(tokenizer.line()))
                  .out_string(": ")
                  .out_string(s)
                  .out_string("\n");

            error <- true;
         }
      else false fi;

      token <- new TokenEof;
      false;
   }};

   errorToken(token : Token, s : String) : Bool {
      --error(s.concat(" DEBUG=".concat(token.type_name()).concat(":").concat(token.toString())))
      error(s)
   };

   tryParseKeyword(op : String) : Bool {
      let tokenKeyword : TokenKeyword <- peekToken().asKeyword() in
         if not isvoid tokenKeyword then
            if tokenKeyword.value() = op then
               skipToken()
            else
               false
            fi
         else
            false
         fi
   };

   parseKeyword(value : String, where : String) : Bool {
      let token : Token <- readToken(),
            tokenKeyword : TokenKeyword <- token.asKeyword() in
         if if not isvoid tokenKeyword then
               tokenKeyword.value() = value
            else
               false
         fi then
            true
         else
            errorToken(token, "expected '".concat(value).concat("'").concat(where))
         fi
   };

   tryParsePunct(value : String) : Bool {
      let token : Token <- peekToken(),
            tokenPunct : TokenPunct <- token.asPunct() in
         if not isvoid tokenPunct then
            if tokenPunct.value() = value then
               skipToken()
            else
               false
            fi
         else
            false
         fi
   };

   parsePunct(value : String, where : String) : Bool {
      let token : Token <- readToken(),
            tokenPunct : TokenPunct <- token.asPunct() in
         if if not isvoid tokenPunct then
               tokenPunct.value() = value
            else
               false
         fi then
            true
         else
            errorToken(token, "expected '".concat(value).concat("'").concat(where))
         fi
   };

   tryParseBinaryOp(value : String) : Bool {
      let token : Token <- peekToken(),
            tokenBinaryOp : TokenBinaryOp <- token.asBinaryOp() in
         if not isvoid tokenBinaryOp then
            if tokenBinaryOp.value() = value then
               skipToken()
            else
               false
            fi
         else
            false
         fi
   };

   parseType(where : String) : String {
      let token : Token <- readToken(),
            tokenType : TokenType <- token.asType() in
         if not isvoid tokenType then
            tokenType.value()
         else
            {
               errorToken(token, "expected type".concat(where));
               "";
            }
         fi
   };

   tryParseId() : String {
      let token : Token <- peekToken(),
            tokenId : TokenId <- token.asId() in
         if not isvoid tokenId then
            {
               skipToken();
               tokenId.value();
            }
         else
            ""
         fi
   };

   parseId(where : String) : String {
      let token : Token <- readToken(),
            tokenId : TokenId <- token.asId() in
         if not isvoid tokenId then
            tokenId.value()
         else
            {
               errorToken(token, "expected id".concat(where));
               "";
            }
         fi
   };

   -- Parse after "("
   parseDispatchArguments(line : Int, target : ParsedExpr, type : String, id : String) : ParsedExpr {
      let arguments : Collection <- new LinkedList in
         {
            if not tryParsePunct(")") then
               {
                  arguments.add(parseExpr(" for dispatch argument"));
                  while tryParsePunct(",") loop
                     arguments.add(parseExpr(" for dispatch argument"))
                  pool;
                  parsePunct(")", " for dispatch expression");
               }
            else false fi;

            new ParsedDispatchExpr.init(line, target, type, id, arguments);
         }
   };

   -- Parse after "."
   parseDispatch(line : Int, target : ParsedExpr, type : String) : ParsedExpr {
      let id : String <- parseId(" for dispatch method") in
         {
            parsePunct("(", " for dispatch expression");
            parseDispatchArguments(line, target, type, id);
         }
   };

   parseIfExpr() : ParsedExpr {
      let expr : ParsedExpr <- parseExpr(" for 'if'"),
            line : Int <- line() in
         {
            parseKeyword("then", " for 'if' expression");
            let then_ : ParsedExpr <- parseExpr(" for 'then'") in
               {
                  parseKeyword("else", " for 'if' expression");
                  let else_ : ParsedExpr <- parseExpr(" for 'else'") in
                     {
                        parseKeyword("fi", " for 'if' expression");
                        new ParsedIfExpr.init(line, expr, then_, else_);
                     };
               };
         }
   };

   parseWhileExpr() : ParsedExpr {
      let expr : ParsedExpr <- parseExpr(" for 'while'"),
            line : Int <- line() in
         {
            parseKeyword("loop", " for 'while' expression");
            let loop_ : ParsedExpr <- parseExpr(" for 'loop'") in
               {
                  parseKeyword("pool", " for 'while' expression");
                  new ParsedWhileExpr.init(line, expr, loop_);
               };
         }
   };

   parseLetVar() : ParsedVar {
      let id : String <- parseId(" for 'let' variable"),
            line : Int <- line() in
         {
            parsePunct(":", " for 'let' variable type");
            let type : String <- parseType(" for 'let' variable"),
                  expr : ParsedExpr in
               {
                  if tryParsePunct("<-") then
                     expr <- parseExpr(" for 'let' variable initialization")
                  else false fi;

                  new ParsedVar.init(line, id, type, expr);
               };
         }
   };

   parseLetExpr() : ParsedExpr {
      let line : Int <- line(),
            vars : Collection <- new LinkedList.add(parseLetVar()) in
         {
            while tryParsePunct(",") loop
               vars.add(parseLetVar())
            pool;

            parseKeyword("in", " for 'let' expression");
            let expr : ParsedExpr <- parseExpr(" for 'let' expression") in
               new ParsedLetExpr.init(line, vars, expr);
         }
   };

   parseCaseBranch() : ParsedVar {
      let id : String <- parseId(" for 'case' branch"),
            line : Int <- line() in
         {
            parsePunct(":", " for 'case' branch");
            let type : String <- parseType(" for 'case' branch") in
               {
                  parsePunct("=>", " for 'case' branch");
                  let expr : ParsedExpr <- parseExpr(" for 'case' branch") in
                     new ParsedVar.init(line, id, type, expr);
               };
         }
   };

   parseCaseExpr() : ParsedExpr {
      let line : Int <- line(),
            expr : ParsedExpr <- parseExpr(" for 'case' expression") in
         {
            parseKeyword("of", " for 'case' expression");
            let branches : Collection <- new LinkedList.add(parseCaseBranch()) in
               {
                  parsePunct(";", " after 'case' branch");
                  while not tryParseKeyword("esac") loop
                     {
                        branches.add(parseCaseBranch());
                        parsePunct(";", " after 'case' branch");
                     }
                  pool;

                  new ParsedCaseExpr.init(line, expr, branches);
               };
         }
   };

   parseKeywordExpr(value : String) : ParsedExpr {
      if value = "if" then
         parseIfExpr()
      else
         if value = "while" then
            parseWhileExpr()
         else
            if value = "let" then
               parseLetExpr()
            else
               if value = "case" then
                  parseCaseExpr()
               else
                  if value = "new" then
                     new ParsedNewExpr.init(line(), parseType(" for 'new'"))
                  else
                     if value = "isvoid" then
                        new ParsedUnaryExpr.init(line(), "isvoid", parseExprImpl(" for 'isvoid'", 6))
                     else
                        if value = "not" then
                           new ParsedUnaryExpr.init(line(), "not", parseExprImpl(" for 'not'", 2))
                        else
                           if value = "true" then
                              new ParsedConstantBoolExpr.init(line(), true)
                           else
                              if value = "false" then
                                 new ParsedConstantBoolExpr.init(line(), false)
                              else
                                 let void : ParsedExpr in void
                              fi
                           fi
                        fi
                     fi
                  fi
               fi
            fi
         fi
      fi
   };

   parseBlock() : ParsedExpr {
      let line : Int <- line(),
            exprs : Collection <- new LinkedList.add(parseExpr(" for block")) in
         {
            parsePunct(";", " after expression in block");
            while not if tryParsePunct("}") then
                  true
               else
                  not isvoid peekToken().asEof()
            fi loop
               {
                  exprs.add(parseExpr(" for block"));
                  parsePunct(";", " after expression in block");
               }
            pool;

            new ParsedBlockExpr.init(line, exprs);
         }
   };

   -- Parse after "("
   parseParenthetical() : ParsedExpr {
      let expr : ParsedExpr <- parseExpr(" for parenthetical expression") in
         {
            parsePunct(")", " for parenthetical expression");
            expr;
         }
   };

   parsePunctExpr(value : String) : ParsedExpr {
      if value = "~" then
         new ParsedUnaryExpr.init(line(), "~", parseExprImpl(" for complement", 7))
      else
         if value = "{" then
            parseBlock()
         else
            if value = "(" then
               parseParenthetical()
            else
               let void : ParsedExpr in void
            fi
         fi
      fi
   };

   parseSimpleExpr(where : String) : ParsedExpr {
      let token : Token <- readToken(),
            line : Int <- tokenizer.line(),
            tokenId : TokenId <- token.asId() in
         if not isvoid tokenId then
            let id : String <- tokenId.value(),
                  tokenPunct : TokenPunct <- peekToken().asPunct() in
               if not isvoid tokenPunct then
                  if tokenPunct.value() = "(" then
                     {
                        skipToken();
                        parseDispatchArguments(line, let void : ParsedExpr in void, "", id);
                     }
                  else
                     if tokenPunct.value() = "<-" then
                        {
                           skipToken();
                           new ParsedAssignmentExpr.init(line(), id, parseExprImpl(" for assignment", 1));
                        }
                     else
                        new ParsedIdExpr.init(line(), id)
                     fi
                  fi
               else
                  new ParsedIdExpr.init(line(), id)
               fi

         else
            let tokenKeyword : TokenKeyword <- token.asKeyword() in
               if not isvoid tokenKeyword then
                  let expr : ParsedExpr <- parseKeywordExpr(tokenKeyword.value()) in
                     if isvoid expr then
                        {
                           errorToken(token, "expected expression".concat(where));
                           new ParsedErrorExpr;
                        }
                     else
                        expr
                     fi

               else
                  let tokenPunct : TokenPunct <- token.asPunct() in
                     if not isvoid tokenPunct then
                        let expr : ParsedExpr <- parsePunctExpr(tokenPunct.value()) in
                           if isvoid expr then
                              {
                                 errorToken(token, "expected expression".concat(where));
                                 new ParsedErrorExpr;
                              }
                           else
                              expr
                           fi
                     else
                        let tokenInteger : TokenInteger <- token.asInteger() in
                           if not isvoid tokenInteger then
                              new ParsedConstantIntExpr.init(line(), tokenInteger.value())
                           else
                              let tokenString : TokenString <- token.asString() in
                                 if not isvoid tokenString then
                                    new ParsedConstantStringExpr.init(line(), tokenString.value())
                                 else
                                    {
                                       errorToken(token, "expected expression".concat(where));
                                       new ParsedErrorExpr;
                                    }
                                 fi
                           fi
                     fi
               fi
         fi
   };

   parsePostfixExpr(where : String) : ParsedExpr {
      let expr : ParsedExpr <- parseSimpleExpr(where),
            continue : Bool <- true in
         {
            while continue loop
               let tokenPunct : TokenPunct <- peekToken().asPunct() in
                  if not isvoid tokenPunct then
                     if tokenPunct.value() = "." then
                        {
                           skipToken();
                           expr <- parseDispatch(tokenizer.line(), expr, "");
                        }
                     else
                        if tokenPunct.value() = "@" then
                           {
                              skipToken();
                              let type : String <- parseType(" for static dispatch type") in
                                 {
                                    parsePunct(".", " for dispatch method");
                                    expr <- parseDispatch(tokenizer.line(), expr, type);
                                 };
                           }
                        else
                           continue <- false
                        fi
                     fi
                  else
                     continue <- false
                  fi
            pool;

            expr;
         }
   };

   parseExprImpl(where : String, minExpr : Int) : ParsedExpr {
      -- Precedence:
      -- 1. "<-"
      -- 2. "not"
      -- 3. "<=", "<", "="
      -- 4. "+", "-"
      -- 5. "*", "/"
      -- 6. "isvoid"
      -- 7. "~"
      -- 8. "@"
      -- 9. "."

      -- [right, op, opLine, left, ...]
      let stack : LinkedList <- new LinkedList.addFirst(new TokenBinaryOp).addFirst(parsePostfixExpr(where)) in
         {
            --new IO.out_string("> parseExprImpl\n")
            let tokenBinaryOp : TokenBinaryOp <- peekToken().asBinaryOp() in
               while not isvoid tokenBinaryOp loop
                  {
                     --new IO.out_string(": parseExprImpl: op=").out_string(tokenBinaryOp.toString()).out_string("\n");
                     skipToken();
                     let line : Int <- line(),
                           expr : ParsedExpr <- parsePostfixExpr("") in
                        {
                           while tokenBinaryOp.prec() < case stack.get(1) of x : TokenBinaryOp => x.prec(); esac loop
                              let right : ParsedExpr <- case stack.removeFirst() of x : ParsedExpr => x; esac,
                                    tokenBinaryOp0 : TokenBinaryOp <- case stack.removeFirst() of x : TokenBinaryOp => x; esac,
                                    tokenBinaryOp0Line : Int <- case stack.removeFirst() of x : Int => x; esac,
                                    left : ParsedExpr <- case stack.removeFirst() of x : ParsedExpr => x; esac in
                                 stack.addFirst(new ParsedBinaryExpr.init(tokenBinaryOp0Line, tokenBinaryOp0.value(), left, right))
                           pool;

                           stack.addFirst(line);
                           stack.addFirst(tokenBinaryOp);
                           stack.addFirst(expr);
                        };

                     tokenBinaryOp <- peekToken().asBinaryOp();
                  }
               pool;

            --new IO.out_string(": parseExprImpl: final reduce\n");
            while not 0 = case stack.get(1) of x : TokenBinaryOp => x.prec(); esac loop
               let right : ParsedExpr <- case stack.removeFirst() of x : ParsedExpr => x; esac,
                     tokenBinaryOp0 : TokenBinaryOp <- case stack.removeFirst() of x : TokenBinaryOp => x; esac,
                     tokenBinaryOp0Line : Int <- case stack.removeFirst() of x : Int => x; esac,
                     left : ParsedExpr <- case stack.removeFirst() of x : ParsedExpr => x; esac in
                  stack.addFirst(new ParsedBinaryExpr.init(tokenBinaryOp0Line, tokenBinaryOp0.value(), left, right))
            pool;

            --new IO.out_string("< parseExprImpl, peek=").out_string(peekToken().toString()).out_string("\n");
            case stack.removeFirst() of x : ParsedExpr => x; esac;
         }
   };

   parseExpr(where : String) : ParsedExpr {
      parseExprImpl(where, 0)
   };

   -- Parse after "("
   parseMethod(line : Int, id : String) : ParsedMethod {
      let formals : Collection <- new LinkedList,
            returnType : String,
            expr : ParsedExpr in
         {
            let id : String <- tryParseId(),
                  continue : Bool <- not id = "" in
               while continue loop
                  {
                     let line : Int <- line() in
                        if parsePunct(":", " for formal parameter type") then
                           let type : String <- parseType(" for formal parameter") in
                              formals.add(new ParsedFormal.init(line, id, type))
                        else false fi;

                     if tryParsePunct(",") then
                        id <- parseId(" for formal parameter")
                     else
                        continue <- false
                     fi;
                  }
               pool;

            parsePunct(")", " for method definition");
            parsePunct(":", " for method definition");
            returnType <- parseType(" for method definition");
            parsePunct("{", " for method definition");
            expr <- parseExpr(" for method definition");
            parsePunct("}", " for method definition");

            new ParsedMethod.init(line, id, formals, returnType, expr);
         }
   };

   -- Parse after ":"
   parseAttribute(line : Int, id : String) : ParsedAttribute {
      let type : String <- parseType(" for attribute"),
            expr : ParsedExpr in
         {
            if tryParsePunct("<-") then
               expr <- parseExpr(" for attribute initialization")
            else false fi;

            new ParsedAttribute.init(line, id, type, expr);
         }
   };

   parseFeature() : ParsedFeature {
      let id : String <- tryParseId(),
            line : Int <- line() in
         if id = "" then
            let void : ParsedFeature in void
         else
            let feature : ParsedFeature,
                  token : Token <- readToken(),
                  tokenPunct : TokenPunct <- token.asPunct() in
               {
                  if not isvoid tokenPunct then
                     if tokenPunct.value() = "(" then
                        feature <- parseMethod(line, id)
                     else
                        if tokenPunct.value() = ":" then
                           feature <- parseAttribute(line, id)
                        else false fi
                     fi
                  else false fi;

                  if isvoid feature then
                     errorToken(token, "expected '(' or ':' for feature")
                  else
                     parsePunct(";", " after feature")
                  fi;

                  feature;
               }
         fi
   };

   parseClass() : ParsedClass {
      let type : String <- parseType(" for class definition"),
            line : Int <- line(),
            inherits_ : String,
            features : Collection <- new LinkedList in
         {
            if tryParseKeyword("inherits") then
               inherits_ <- parseType(" for inherits")
            else false fi;

            if parsePunct("{", " for class definition") then
               {
                  let feature : ParsedFeature <- parseFeature() in
                     if isvoid feature then
                        errorToken(peekToken(), "expected feature for class definition")
                     else
                        while not isvoid feature loop
                           {
                              features.add(feature);
                              feature <- parseFeature();
                           }
                        pool
                     fi;

                  parsePunct("}", " for class definition");
               }
            else false fi;

            new ParsedClass.init(line, type, inherits_, features);
         }
   };

   parse() : ParsedProgram {
      let classes : Collection <- new LinkedList in
         {
            let moreFiles : Bool <- true in
               while moreFiles loop
                  {
                     if parseKeyword("class", " for program") then
                        {
                           classes.add(parseClass());
                           parsePunct(";", " after class definition");
                        }
                     else false fi;

                     let continue : Bool <- true in
                        while continue loop
                           if tryParseKeyword("class") then
                              {
                                 classes.add(parseClass());
                                 parsePunct(";", " after class definition");
                              }
                           else
                              let tokenEof : TokenEof <- peekToken().asEof() in
                                 if isvoid tokenEof then
                                    errorToken(token, "expected 'class' for program")
                                 else
                                    {
                                       skipToken();
                                       moreFiles <- tokenEof.moreFiles();
                                       continue <- false;
                                    }
                                 fi
                           fi
                        pool;
                  }
               pool;

            if error then
               let void : ParsedProgram in void
            else
               new ParsedProgram.init(classes)
            fi;
         }
   };
};
