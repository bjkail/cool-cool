class ParsedProgram {
   classes : Collection <- new LinkedList;
   classes() : Collection { classes };

   init(classes_ : Collection) : SELF_TYPE {{
      classes <- classes_;
      self;
   }};
};

class ParsedClass {
   id : String;
   id() : String { id };

   inherits_ : String;
   inherits_() : String { inherits_ };

   features : Collection <- new LinkedList;
   features() : Collection { features };

   init(id_ : String, inherits__ : String, features_ : Collection) : SELF_TYPE {{
      id <- id_;
      inherits_ <- inherits__;
      features <- features_;
      self;
   }};
};

class ParsedFeature {
   id : String;
   id() : String { id };

   asAttribute() : ParsedAttribute { let void : ParsedAttribute in void };
   asMethod() : ParsedMethod { let void : ParsedMethod in void };
};

class ParsedAttribute inherits ParsedFeature {
   type : String;
   expr : ParsedExpr;

   init(id_ : String, type_ : String, expr_ : ParsedExpr) : SELF_TYPE {{
      id <- id_;
      type <- type_;
      expr <- expr_;
      self;
   }};

   asAttribute() : ParsedAttribute { self };
};

class ParsedFormal {
   id : String;
   id() : String { id };

   type : String;
   type() : String { type };

   init(id_ : String, type_ : String) : SELF_TYPE {{
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

   init(id_ : String, formals_ : Collection, returnType_ : String, expr_ : ParsedExpr) : SELF_TYPE {{
      id <- id_;
      formals <- formals_;
      returnType <- returnType_;
      expr <- expr_;
      self;
   }};

   asMethod() : ParsedMethod { self };
};

class ParsedExpr {
   x() : Bool { false };
};

class ParsedErrorExpr inherits ParsedExpr {
   x() : Bool { false };
};

class ParsedBlockExpr inherits ParsedExpr {
   exprs : Collection;

   init(exprs_ : Collection) : SELF_TYPE {{
      exprs <- exprs_;
      self;
   }};
};

class ParsedIfExpr inherits ParsedExpr {
   expr : ParsedExpr;
   then_ : ParsedExpr;
   else_ : ParsedExpr;

   init(expr_ : ParsedExpr, then__ : ParsedExpr, else__ : ParsedExpr) : SELF_TYPE {{
      expr <- expr_;
      then_ <- then__;
      else_ <- else__;
      self;
   }};
};

class ParsedWhileExpr inherits ParsedExpr {
   expr : ParsedExpr;
   loop_ : ParsedExpr;

   init(expr_ : ParsedExpr, loop__ : ParsedExpr) : SELF_TYPE {{
      expr <- expr_;
      loop_ <- loop__;
      self;
   }};
};

class ParsedVar {
   id : String;
   type : String;
   expr : ParsedExpr;

   init(id_ : String, type_ : String, expr_ : ParsedExpr) : SELF_TYPE {{
      id <- id_;
      type <- type_;
      expr <- expr_;
      self;
   }};
};

class ParsedLetExpr inherits ParsedExpr {
   vars : Collection;
   expr : ParsedExpr;

   init(vars_ : Collection, expr_ : ParsedExpr) : SELF_TYPE {{
      vars <- vars_;
      expr <- expr_;
      self;
   }};
};

class ParsedCaseExpr inherits ParsedExpr {
   expr : ParsedExpr;
   branches : Collection;

   init(expr_ : ParsedExpr, branches_ : Collection) : SELF_TYPE {{
      expr <- expr_;
      branches <- branches_;
      self;
   }};
};

class ParsedAssignmentExpr inherits ParsedExpr {
   id : String;
   id() : String { id };

   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   init(id_ : String, expr_ : ParsedExpr) : SELF_TYPE {{
      id <- id_;
      expr <- expr_;
      self;
   }};
};

class ParsedSelfExpr inherits ParsedExpr {
   x() : Bool { false };
};

class ParsedIdExpr inherits ParsedExpr {
   id : String;
   id() : String { id };

   init(id_ : String) : SELF_TYPE {{
      id <- id_;
      self;
   }};
};

class ParsedNewExpr inherits ParsedExpr {
   type : String;

   init(type_ : String) : SELF_TYPE {{
      type <- type_;
      self;
   }};
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

   init(target_ : ParsedExpr, type_ : String, id_ : String, arguments_ : Collection) : SELF_TYPE {{
      target <- target_;
      type <- type_;
      id <- id_;
      arguments <- arguments_;
      self;
   }};
};

class ParsedUnaryExpr inherits ParsedExpr {
   op : String;
   op() : String { op };

   expr : ParsedExpr;
   expr() : ParsedExpr { expr };

   init(op_ : String, expr_ : ParsedExpr) : SELF_TYPE {{
      op <- op_;
      expr <- expr_;
      self;
   }};
};

class ParsedBinaryExpr inherits ParsedExpr {
   op : String;
   op() : String { op };

   left : ParsedExpr;
   left() : ParsedExpr { left };

   right : ParsedExpr;
   right() : ParsedExpr { right };

   init(op_ : String, left_ : ParsedExpr, right_ : ParsedExpr) : SELF_TYPE {{
      op <- op_;
      left <- left_;
      right <- right_;
      self;
   }};
};

class ParsedConstantBoolExpr inherits ParsedExpr {
   value : Bool;
   value() : Bool { value };

   init(value_ : Bool) : SELF_TYPE {{
      value <- value_;
      self;
   }};
};

class ParsedConstantIntExpr inherits ParsedExpr {
   value : Int;
   value() : Int { value };

   init(value_ : Int) : SELF_TYPE {{
      value <- value_;
      self;
   }};
};

class ParsedConstantStringExpr inherits ParsedExpr {
   value : String;
   value() : String { value };

   init(value_ : String) : SELF_TYPE {{
      value <- value_;
      self;
   }};
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

   error(s : String) : Bool {{
      if not error then
         {
            new IO.out_string("PARSE ERROR: line ")
                  .out_string(stringUtil.fromInt(tokenizer.line()))
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
   parseDispatchArguments(target : ParsedExpr, type : String, id : String) : ParsedExpr {
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

            new ParsedDispatchExpr.init(target, type, id, arguments);
         }
   };

   -- Parse after "."
   parseDispatch(target : ParsedExpr, type : String) : ParsedExpr {
      let id : String <- parseId(" for dispatch method") in
         {
            parsePunct("(", " for dispatch expression");
            parseDispatchArguments(target, type, id);
         }
   };

   parseIfExpr() : ParsedExpr {
      let expr : ParsedExpr <- parseExpr(" for 'if'") in
         {
            parseKeyword("then", " for 'if' expression");
            let then_ : ParsedExpr <- parseExpr(" for 'then'") in
               {
                  parseKeyword("else", " for 'if' expression");
                  let else_ : ParsedExpr <- parseExpr(" for 'else'") in
                     {
                        parseKeyword("fi", " for 'if' expression");
                        new ParsedIfExpr.init(expr, then_, else_);
                     };
               };
         }
   };

   parseWhileExpr() : ParsedExpr {
      let expr : ParsedExpr <- parseExpr(" for 'while'") in
         {
            parseKeyword("loop", " for 'while' expression");
            let loop_ : ParsedExpr <- parseExpr(" for 'loop'") in
               {
                  parseKeyword("pool", " for 'while' expression");
                  new ParsedWhileExpr.init(expr, loop_);
               };
         }
   };

   parseLetVar() : ParsedVar {
      let id : String <- parseId(" for 'let' variable") in
         {
            parsePunct(":", " for 'let' variable type");
            let type : String <- parseType(" for 'let' variable"),
                  expr : ParsedExpr in
               {
                  if tryParsePunct("<-") then
                     expr <- parseExpr(" for 'let' variable initialization")
                  else false fi;

                  new ParsedVar.init(id, type, expr);
               };
         }
   };

   parseLetExpr() : ParsedExpr {
      let vars : Collection <- new LinkedList.add(parseLetVar()) in
         {
            while tryParsePunct(",") loop
               vars.add(parseLetVar())
            pool;

            parseKeyword("in", " for 'let' expression");
            let expr : ParsedExpr <- parseExpr(" for 'let' expression") in
               new ParsedLetExpr.init(vars, expr);
         }
   };

   parseCaseBranch() : ParsedVar {
      let id : String <- parseId(" for 'case' branch") in
         {
            parsePunct(":", " for 'case' branch");
            let type : String <- parseType(" for 'case' branch") in
               {
                  parsePunct("=>", " for 'case' branch");
                  let expr : ParsedExpr <- parseExpr(" for 'case' branch") in
                     new ParsedVar.init(id, type, expr);
               };
         }
   };

   parseCaseExpr() : ParsedExpr {
      let expr : ParsedExpr <- parseExpr(" for 'case' expression") in
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

                  new ParsedCaseExpr.init(expr, branches);
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
                     new ParsedNewExpr.init(parseType(" for 'new'"))
                  else
                     if value = "isvoid" then
                        new ParsedUnaryExpr.init("isvoid", parseExprImpl(" for 'isvoid'", 6))
                     else
                        if value = "not" then
                           new ParsedUnaryExpr.init("not", parseExprImpl(" for 'not'", 2))
                        else
                           if value = "true" then
                              new ParsedConstantBoolExpr.init(true)
                           else
                              if value = "false" then
                                 new ParsedConstantBoolExpr.init(false)
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
      let exprs : Collection <- new LinkedList.add(parseExpr(" for block")) in
         {
            parsePunct(";", " after expression in block");
            while not if tryParsePunct("}") then
                  true
               else
                  peekToken().isEof()
            fi loop
               {
                  exprs.add(parseExpr(" for block"));
                  parsePunct(";", " after expression in block");
               }
            pool;

            new ParsedBlockExpr.init(exprs);
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
         new ParsedUnaryExpr.init("~", parseExprImpl(" for complement", 7))
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
            tokenId : TokenId <- token.asId() in
         if not isvoid tokenId then
            let id : String <- tokenId.value(),
                  tokenPunct : TokenPunct <- peekToken().asPunct() in
               if not isvoid tokenPunct then
                  if tokenPunct.value() = "(" then
                     {
                        skipToken();
                        parseDispatchArguments(new ParsedSelfExpr, "", id);
                     }
                  else
                     if tokenPunct.value() = "<-" then
                        {
                           skipToken();
                           new ParsedAssignmentExpr.init(id, parseExprImpl(" for assignment", 1));
                        }
                     else
                        new ParsedIdExpr.init(id)
                     fi
                  fi
               else
                  new ParsedIdExpr.init(id)
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
                              new ParsedConstantIntExpr.init(tokenInteger.value())
                           else
                              let tokenString : TokenString <- token.asString() in
                                 if not isvoid tokenString then
                                    new ParsedConstantStringExpr.init(tokenString.value())
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
                           expr <- parseDispatch(expr, "");
                        }
                     else
                        if tokenPunct.value() = "@" then
                           {
                              skipToken();
                              let type : String <- parseType(" for static dispatch type") in
                                 {
                                    parsePunct(".", " for dispatch method");
                                    expr <- parseDispatch(expr, type);
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

      let stack : LinkedList <- new LinkedList.addFirst(new TokenBinaryOp).addFirst(parsePostfixExpr(where)) in
         {
            --new IO.out_string("> parseExprImpl\n")
            let tokenBinaryOp : TokenBinaryOp <- peekToken().asBinaryOp() in
               while not isvoid tokenBinaryOp loop
                  {
                     --new IO.out_string(": parseExprImpl: op=").out_string(tokenBinaryOp.toString()).out_string("\n");
                     skipToken();
                     let expr : ParsedExpr <- parsePostfixExpr("") in
                        {
                           while tokenBinaryOp.prec() < case stack.get(1) of x : TokenBinaryOp => x.prec(); esac loop
                              let right : ParsedExpr <- case stack.removeFirst() of x : ParsedExpr => x; esac,
                                    tokenBinaryOp0 : TokenBinaryOp <- case stack.removeFirst() of x : TokenBinaryOp => x; esac,
                                    left : ParsedExpr <- case stack.removeFirst() of x : ParsedExpr => x; esac in
                                 stack.addFirst(new ParsedBinaryExpr.init(tokenBinaryOp0.value(), left, right))
                           pool;

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
                     left : ParsedExpr <- case stack.removeFirst() of x : ParsedExpr => x; esac in
                  stack.addFirst(new ParsedBinaryExpr.init(tokenBinaryOp0.value(), left, right))
            pool;

            --new IO.out_string("< parseExprImpl, peek=").out_string(peekToken().toString()).out_string("\n");
            case stack.removeFirst() of x : ParsedExpr => x; esac;
         }
   };

   parseExpr(where : String) : ParsedExpr {
      parseExprImpl(where, 0)
   };

   -- Parse after "("
   parseMethod(id : String) : ParsedMethod {
      let formals : Collection <- new LinkedList,
            returnType : String,
            expr : ParsedExpr in
         {
            let id : String <- tryParseId(),
                  continue : Bool <- not id = "" in
               while continue loop
                  {
                     if parsePunct(":", " for formal parameter type") then
                        let type : String <- parseType(" for formal parameter") in
                           formals.add(new ParsedFormal.init(id, type))
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

            new ParsedMethod.init(id, formals, returnType, expr);
         }
   };

   -- Parse after ":"
   parseAttribute(id : String) : ParsedAttribute {
      let type : String <- parseType(" for attribute"),
            expr : ParsedExpr in
         {
            if tryParsePunct("<-") then
               expr <- parseExpr(" for attribute initialization")
            else false fi;

            new ParsedAttribute.init(id, type, expr);
         }
   };

   parseFeature() : ParsedFeature {
      let id : String <- tryParseId() in
         if id = "" then
            let void : ParsedFeature in void
         else
            let feature : ParsedFeature,
                  token : Token <- readToken(),
                  tokenPunct : TokenPunct <- token.asPunct() in
               {
                  if not isvoid tokenPunct then
                     if tokenPunct.value() = "(" then
                        feature <- parseMethod(id)
                     else
                        if tokenPunct.value() = ":" then
                           feature <- parseAttribute(id)
                        else false fi
                     fi
                  else false fi;

                  if isvoid feature then
                     error("expected '(' or ':' for feature")
                  else
                     parsePunct(";", " after feature")
                  fi;

                  feature;
               }
         fi
   };

   parseClass() : ParsedClass {{
      parseKeyword("class", " in program");
      let id : String <- parseType(" for class definition"),
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

            new ParsedClass.init(id, inherits_, features);
         };
   }};

   parse() : ParsedProgram {
      let classes : Collection <- new LinkedList,
            class_ : ParsedClass <- parseClass() in
         {
            classes.add(class_);
            parsePunct(";", " after class definition");

            while not peekToken().isEof() loop
               let class_ : ParsedClass <- parseClass() in
                  {
                     classes.add(class_);
                     parsePunct(";", " after class definition");
                  }
            pool;

            new ParsedProgram.init(classes);
         }
   };
};
