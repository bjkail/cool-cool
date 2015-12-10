class Main inherits Test {
   test() : Object {{
      testEof();
      testError();
      testParenComment();
      testLineComment();

      testPunct();
      testBinaryOp();
      testKeyword();
      testType();
      testId();
      testInteger();
      testString();
   }};

   newTokenizer(s : String) : Tokenizer {
      new Tokenizer.init(new StringInputStream.init(s))
   };

   assertTokenEof(context : String, t : Tokenizer) : Object {
      let token : Token <- t.next() in
         if not token.isEof() then
            failContext(context, "expected=EOF, actual=".concat(token.toString()))
         else false fi
   };

   assertTokenError(context : String, value : String, t: Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenError : TokenError <- token.asError() in
         {
            if isvoid tokenError then
               failContext(context.concat(" void"), "expected=".concat(tokenToString(new TokenError.init(value)))
                     .concat(", actual=").concat(tokenToString(token)))
            else false fi;
            assertStringEquals(context, value, tokenError.value());
         }
   };

   tokenToString(token : Token) : String {
      token.type_name().concat(":").concat(token.toString())
   };

   assertTokenPunct(context : String, value : String, t : Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenPunct : TokenPunct <- token.asPunct() in
         {
            if isvoid tokenPunct then
               failContext(context.concat(" void"), "expected=".concat(tokenToString(new TokenPunct.init(value)))
                     .concat(", actual=").concat(tokenToString(token)))
            else false fi;
            assertStringEquals(context, value, tokenPunct.value());
         }
   };

   assertTokenBinaryOp(context : String, value : String, t : Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenBinaryOp : TokenBinaryOp <- token.asBinaryOp() in
         {
            if isvoid tokenBinaryOp then
               failContext(context.concat(" void"), "expected=".concat(tokenToString(new TokenBinaryOp.init(value)))
                     .concat(", actual=").concat(tokenToString(token)))
            else false fi;
            assertStringEquals(context, value, tokenBinaryOp.value());
         }
   };

   assertTokenKeyword(context : String, value : String, t : Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenKeyword : TokenKeyword <- token.asKeyword() in
         {
            if isvoid tokenKeyword then
               failContext(context.concat(" void"), "expected=".concat(tokenToString(new TokenKeyword.init(value)))
                     .concat(", actual=").concat(tokenToString(token)))
            else false fi;
            assertStringEquals(context, value, tokenKeyword.value());
         }
   };

   assertTokenType(context : String, value : String, t : Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenType : TokenType <- token.asType() in
         {
            if isvoid tokenType then
               failContext(context.concat(" void"), "expected=".concat(tokenToString(new TokenType.init(value)))
                     .concat(", actual=").concat(tokenToString(token)))
            else false fi;
            assertStringEquals(context, value, tokenType.value());
         }
   };

   assertTokenId(context : String, value : String, t : Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenId : TokenId <- token.asId() in
         {
            if isvoid tokenId then
               failContext(context.concat(" void"), "expected=".concat(tokenToString(new TokenId.init(value)))
                     .concat(", actual=").concat(tokenToString(token)))
            else false fi;
            assertStringEquals(context, value, tokenId.value());
         }
   };

   assertTokenInteger(context : String, value : Int, t : Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenInteger : TokenInteger <- token.asInteger() in
         {
            if isvoid tokenInteger then
               failContext(context.concat(" void"), "expected=".concat(tokenToString(new TokenInteger.init(value)))
                     .concat(", actual=").concat(tokenToString(token)))
            else false fi;
            assertIntEquals(context, value, tokenInteger.value());
         }
   };

   assertTokenString(context : String, value : String, t : Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenString : TokenString <- token.asString() in
         {
            if isvoid tokenString then
               failContext(context.concat(" void"), "expected=".concat(tokenToString(new TokenString.init(0, value)))
                     .concat(", actual=").concat(tokenToString(token)))
            else false fi;
            assertStringEquals(context, value, tokenString.value());
         }
   };


   testEof() : Object {
      if begin("eof") then
         assertTokenEof("eof", newTokenizer(""))
      else false fi
   };

   testError() : Object {
      if begin("error") then
         assertTokenError("", "line 1: invalid character: [%]", newTokenizer("%"))
      else false fi
   };

   testParenComment() : Object {
      if begin("parenComment") then
         {
            assertTokenPunct("empty", ".", newTokenizer("(**)."));
            assertTokenPunct("space", ".", newTokenizer("(* *)."));
            assertTokenPunct("newline", ".", newTokenizer("(*\n*)."));
            assertTokenPunct("asterisk", ".", newTokenizer("(***)."));
            assertTokenPunct("chars", ".", newTokenizer("(* x \n * \n x *)."));
            assertTokenError("eof", "line 1: unexpected EOF in enclosing comment", newTokenizer("(*"));
         }
      else false fi
   };

   testLineComment() : Object {
      if begin("lineComment") then
         {
            assertTokenEof("empty", newTokenizer("--"));
            assertTokenEof("space", newTokenizer("-- "));
            assertTokenPunct("newline", ".", newTokenizer("--\n."));
            assertTokenPunct("chars", ".", newTokenizer("-- x-- *) .\n."));
         }
      else false fi
   };

   testPunct() : Object {
      if begin("punct") then
         let t : Tokenizer <- newTokenizer("{}(( *):,@.;~<-=>") in
            {
               assertTokenPunct("", "{", t);
               assertTokenPunct("", "}", t);
               assertTokenPunct("", "(", t);
               assertTokenPunct("asterisk", "(", t);
               assertTokenBinaryOp("asterisk", "*", t);
               assertTokenPunct("", ")", t);
               assertTokenPunct("", ":", t);
               assertTokenPunct("", ",", t);
               assertTokenPunct("", "@", t);
               assertTokenPunct("", ".", t);
               assertTokenPunct("", ";", t);
               assertTokenPunct("", "~", t);
               assertTokenPunct("", "<-", t);
               assertTokenPunct("", "=>", t);
            }
      else false fi
   };

   testBinaryOp() : Object {
      if begin("binaryOp") then
         {
            let t : Tokenizer <- newTokenizer("+-*/<<==") in
               {
                  assertTokenBinaryOp("", "+", t);
                  assertTokenBinaryOp("", "-", t);
                  assertTokenBinaryOp("", "*", t);
                  assertTokenBinaryOp("", "/", t);
                  assertTokenBinaryOp("", "<", t);
                  assertTokenBinaryOp("", "<=", t);
                  assertTokenBinaryOp("", "=", t);
               };

            let t : Tokenizer <- newTokenizer("<--") in
               {
                  -- As opposed to TokenBinaryOp("<") TokenEof
                  assertTokenPunct("<--", "<-", t);
                  assertTokenBinaryOp("<--", "-", t);
               };
         }
      else false fi
   };

   testKeyword() : Object {
      if begin("keyword") then
         let t : Tokenizer <- newTokenizer("case class else esac false fi if in inherits isvoid let loop new not of pool then true while") in
            {
               assertTokenKeyword("", "case", t);
               assertTokenKeyword("", "class", t);
               assertTokenKeyword("", "else", t);
               assertTokenKeyword("", "esac", t);
               assertTokenKeyword("", "false", t);
               assertTokenKeyword("", "fi", t);
               assertTokenKeyword("", "if", t);
               assertTokenKeyword("", "in", t);
               assertTokenKeyword("", "inherits", t);
               assertTokenKeyword("", "isvoid", t);
               assertTokenKeyword("", "let", t);
               assertTokenKeyword("", "loop", t);
               assertTokenKeyword("", "new", t);
               assertTokenKeyword("", "not", t);
               assertTokenKeyword("", "of", t);
               assertTokenKeyword("", "pool", t);
               assertTokenKeyword("", "then", t);
               assertTokenKeyword("", "true", t);
               assertTokenKeyword("", "while", t);
            }
      else false fi
   };

   testType() : Object {
      if begin("type") then
         let t : Tokenizer <- newTokenizer("Object Bool Int String IO A BxX0_ SELF_TYPE") in
            {
               assertTokenType("", "Object", t);
               assertTokenType("", "Bool", t);
               assertTokenType("", "Int", t);
               assertTokenType("", "String", t);
               assertTokenType("", "IO", t);
               assertTokenType("", "A", t);
               assertTokenType("", "BxX0_", t);
               assertTokenType("", "SELF_TYPE", t);
            }
      else false fi
   };

   testId() : Object {
      if begin("keyword") then
         let t : Tokenizer <- newTokenizer("a bxX0_ self") in
            {
               assertTokenId("", "a", t);
               assertTokenId("", "bxX0_", t);
               assertTokenId("", "self", t);
            }
      else false fi
   };

   testInteger() : Object {
      if begin("integer") then
         let t : Tokenizer <- newTokenizer("0 2147483647 2147483648 12345678901234567890") in
            {
               assertTokenInteger("", 0, t);
               assertTokenInteger("", 2147483647, t);
               assertTokenError("", "line 1: integer overflow: 2147483648", t);
               assertTokenError("", "line 1: integer overflow: 12345678901234567890", t);
               assertTokenEof("", t);
            }
      else false fi
   };

   testString() : Object {
      if begin("string") then
         {
            let t : Tokenizer <- newTokenizer("\"\" \"a\" \"\\c\\b\\t\\n\\f\" \"\\\\\" \"\\\"\" \"\\\n\"") in
               {
                  assertTokenString("", "", t);
                  assertTokenString("", "a", t);
                  assertTokenString("", "c\b\t\n\f", t);
                  assertTokenString("", stringUtil.backslash(), t);
                  assertTokenString("", stringUtil.doubleQuote(), t);
                  assertTokenString("", "\n", t);
               };

            let s8 : String <- "01234567",
                  s64 : String <- s8.concat(s8).concat(s8).concat(s8).concat(s8).concat(s8).concat(s8).concat(s8),
                  s512 : String <- s64.concat(s64).concat(s64).concat(s64).concat(s64).concat(s64).concat(s64).concat(s64),
                  s1024 : String <- s512.concat(s512) in
               {
                  assertTokenString("", s1024, newTokenizer("\"".concat(s1024).concat("\"")));
                  assertTokenError("", "line 1: maximum string constant length exceeded",
                        newTokenizer("\"x".concat(s1024).concat("\"")));
               };

            assertTokenError("", "line 1: unexpected EOF in string", newTokenizer("\""));
            assertTokenError("", "line 1: unexpected newline in string", newTokenizer("\"\n\""));
         }
      else false fi
   };
};

class StringInputStream inherits InputStream {
   stringUtil : StringUtil <- new StringUtil;
   s : String;
   pos : Int;

   init(s_ : String) : SELF_TYPE {{
      s <- s_;
      self;
   }};

   read() : String {
      if pos = s.length() then
         ""
      else
         let c : String <- s.substr(pos, 1) in
            {
               -- Support UVA Cool Interpreter dialect.
               if "\\".length() = 2 then
                  if c = stringUtil.backslash() then
                     let c2 : String <- s.substr(pos + 1, 1) in
                        if if c2 = stringUtil.backslash() then
                              true
                           else
                              c2 = stringUtil.doubleQuote()
                        fi then
                           {
                              c <- c2;
                              pos <- pos + 1;
                           }
                        else
                           if c2 = "n" then
                              {
                                 c <- "\n";
                                 pos <- pos + 1;
                              }
                           else false fi
                        fi
                  else false fi
               else false fi;

               pos <- pos + 1;
               c;
            }
      fi
   };
};
