class Main inherits Test {
   test() : Object {{
      testEof();
      testError();
      testParenComment();
      testLineComment();

      testEofDirective();
      testEscapeDirective();
      testFileDirective();
      testFilesDirective();
      testOptionDirective();
      testUvaDirective();

      testPunct();
      testBinaryOp();
      testKeyword();
      testType();
      testId();
      testInteger();
      testString();
   }};

   newTokenizer(s : String) : Tokenizer {
      new Tokenizer.init(new TestStringInputStream.init(s))
   };

   newIOTokenizer(context : String, lines : Collection) : Tokenizer {
      let is : IOInputStream <- new IOInputStream.init(new TestIO.init(self, context, lines, new Collection)) in
         new Tokenizer.init(is).setListener(new TestTokenizerListener.init(is))
   };

   assertTokenEofMoreFiles(context : String, moreFiles : Bool, t : Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenEof : TokenEof <- token.asEof() in
         if isvoid tokenEof then
            failContext(context, "expected=EOF, actual=".concat(token.toString()))
         else
            assertBoolEquals(context.concat(" moreFiles"), moreFiles, tokenEof.moreFiles())
         fi
   };

   assertTokenEof(context : String, t : Tokenizer) : Object {
      assertTokenEofMoreFiles(context, false, t)
   };

   assertTokenError(context : String, value : String, t: Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenError : TokenError <- token.asError() in
         {
            if isvoid tokenError then
               failContext(context.concat(" error"), "expected=".concat(tokenToString(new TokenError.init(0, value)))
                     .concat(", actual=").concat(tokenToString(token)))
            else false fi;
            assertStringEquals(context, value, "line ".concat(stringUtil.fromInt(tokenError.line())).concat(": ").concat(tokenError.value()));
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

   assertTokenStringEscapes(context : String, value : String, escapes : Int, t : Tokenizer) : Object {
      let token : Token <- t.next(),
            tokenString : TokenString <- token.asString() in
         {
            if isvoid tokenString then
               failContext(context.concat(" void"), "expected=".concat(tokenToString(new TokenString.init(0, value, escapes)))
                     .concat(", actual=").concat(tokenToString(token)))
            else false fi;
            assertStringEquals(context, value, tokenString.value());
            assertIntEquals(context.concat(" escapes"), escapes, tokenString.escapes());
         }
   };

   assertTokenString(context : String, value : String, t : Tokenizer) : Object {
      assertTokenStringEscapes(context, value, 0, t)
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
            assertTokenBinaryOp("nested", "*", newTokenizer("(*(**)*)"));
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

   testEofDirective() : Object {
      if begin("eofDirective") then
         {
            let t : Tokenizer <- newIOTokenizer("", new LinkedList
                     .add("--cool:eof=1")
                     .add(".")
                     .add("")
                     .add(".")) in
               {
                  assertTokenPunct("", ".", t);
                  assertTokenEof("", t);
               };
         }
      else false fi
   };

   testEscapeDirective() : Object {
      if begin("escapeDirective") then
         {
            let t : Tokenizer <- newTokenizer("--cool:escape:\\b=@\n\"\\b\"") in
               {
                  assertTokenString("bs", "@", t);
                  assertTokenEof("bs", t);
               };

            let t : Tokenizer <- newTokenizer("--cool:escape:\\f=@\n@.@\"\\f\"") in
               {
                  assertTokenPunct("ff", ".", t);
                  assertTokenString("ff", "@", t);
                  assertTokenEof("ff", t);
               };

            let t : Tokenizer <- newTokenizer("--cool:escape:\\r=@\n@.@") in
               {
                  assertTokenPunct("cr", ".", t);
                  assertTokenEof("cr", t);
               };

            let t : Tokenizer <- newTokenizer("--cool:escape:\\t=@\n@.@\"\\t\"") in
               {
                  assertTokenPunct("tab", ".", t);
                  assertTokenString("tab", "@", t);
                  assertTokenEof("tab", t);
               };

            let t : Tokenizer <- newTokenizer("--cool:escape:\\v=@\n@.@") in
               {
                  assertTokenPunct("vt", ".", t);
                  assertTokenEof("vt", t);
               };

            let t : Tokenizer <- newTokenizer("--cool:escape:\\v=@\n\"") in
               assertTokenError("line", "line 1: unexpected EOF in string", t);
         }
      else false fi
   };

   testFileDirective() : Object {
      if begin("fileDirective") then
         {
            let t : Tokenizer <- newTokenizer(".\n."),
                  flm : TokenizerLineMap <- t.lineMap() in
               {
                  assertTokenPunct("default 1", ".", t);
                  assertIntEquals("default 1 total", 1, t.line());
                  assertStringEquals("default 1", "line 1", flm.lineToString(1));

                  assertTokenPunct("default 2", ".", t);
                  assertIntEquals("default 2 total", 2, t.line());
                  assertStringEquals("default 2", "line 1", flm.lineToString(1));
                  assertStringEquals("default 2", "line 2", flm.lineToString(2));
               };

            let t : Tokenizer <- newTokenizer("--cool:file=a\n.\n."),
                  flm : TokenizerLineMap <- t.lineMap() in
               {
                  assertTokenPunct("single a1", ".", t);
                  assertIntEquals("single a1 total", 1, t.line());
                  assertStringEquals("single a1", "a: line 1", flm.lineToString(1));

                  assertTokenPunct("single a2", ".", t);
                  assertIntEquals("single a2 total", 2, t.line());
                  assertStringEquals("single a2", "a: line 1", flm.lineToString(1));
                  assertStringEquals("single a2", "a: line 2", flm.lineToString(2));
               };

            let t : Tokenizer <- newTokenizer("--cool:file=a\n.\n.\n--cool:file=b\n.\n."),
                  flm : TokenizerLineMap <- t.lineMap() in
               {
                  assertTokenPunct("multi a1", ".", t);
                  assertIntEquals("multi a1 total", 1, t.line());
                  assertStringEquals("multi a1", "a: line 1", flm.lineToString(1));

                  assertTokenPunct("multi a2", ".", t);
                  assertIntEquals("multi a2 total", 2, t.line());
                  assertStringEquals("multi a2", "a: line 1", flm.lineToString(1));
                  assertStringEquals("multi a2", "a: line 2", flm.lineToString(2));

                  assertTokenPunct("multi b1", ".", t);
                  assertIntEquals("multi b1 total", 3, t.line());
                  assertStringEquals("multi b1", "a: line 1", flm.lineToString(1));
                  assertStringEquals("multi b1", "a: line 2", flm.lineToString(2));
                  assertStringEquals("multi b1", "b: line 1", flm.lineToString(3));

                  assertTokenPunct("multi b2", ".", t);
                  assertIntEquals("multi b2 total", 4, t.line());
                  assertStringEquals("multi b2", "a: line 1", flm.lineToString(1));
                  assertStringEquals("multi b2", "a: line 2", flm.lineToString(2));
                  assertStringEquals("multi b2", "b: line 1", flm.lineToString(3));
                  assertStringEquals("multi b2", "b: line 2", flm.lineToString(4));
               };
         }
      else false fi
   };

   testFilesDirective() : Object {
      if begin("filesDirective") then
         {
            let t : Tokenizer <- newIOTokenizer("", new LinkedList
                     .add("--cool:files=2")
                     .add("--cool:eof=1")
                     .add("1")
                     .add("")
                     .add("2")
                     .add("")
                     .add("3")) in
               {
                  assertTokenInteger("", 1, t);
                  assertTokenEofMoreFiles("", true, t);
                  assertTokenInteger("", 2, t);
                  assertTokenEof("", t);
               };
         }
      else false fi
   };

   testOptionDirective() : Object {
      if begin("optionDirective") then
         {
            let listener : TestOptionTokenizerListener <- new TestOptionTokenizerListener,
                  t : Tokenizer <- newTokenizer("--cool:option=a\n--cool:option=bcd").setListener(listener) in
               {
                  assertTokenEof("option", t);
                  let iter : Iterator <- listener.options().iterator() in
                     {
                        assertTrue("option a", iter.next());
                        assertStringEquals("option a", "a", case iter.get() of x : String => x; esac);
                        assertTrue("option bcd", iter.next());
                        assertStringEquals("option bcd", "bcd", case iter.get() of x : String => x; esac);
                        assertFalse("options", iter.next());
                     };
               };
         }
      else false fi
   };

   testUvaDirective() : Object {
      if begin("uvaDirective") then
         {
            let listener : TestUvaTokenizerListener <- new TestUvaTokenizerListener,
                  t : Tokenizer <- newTokenizer("--cool:uva\n\"\\c\"").setListener(listener) in
               {
                  assertTokenString("string", stringUtil.backslash().concat("c"), t);
                  assertTrue("uva", listener.uva());
               };
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
         {
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
               };

            let t : Tokenizer <- newTokenizer("CASE CLASS ELSE ESAC FALSE FI IF IN INHERITS ISVOID LET LOOP NEW NOT OF POOL THEN TRUE WHILE") in
               {
                  assertTokenKeyword("upper", "case", t);
                  assertTokenKeyword("upper", "class", t);
                  assertTokenKeyword("upper", "else", t);
                  assertTokenKeyword("upper", "esac", t);
                  assertTokenType("upper", "FALSE", t);
                  assertTokenKeyword("upper", "fi", t);
                  assertTokenKeyword("upper", "if", t);
                  assertTokenKeyword("upper", "in", t);
                  assertTokenKeyword("upper", "inherits", t);
                  assertTokenKeyword("upper", "isvoid", t);
                  assertTokenKeyword("upper", "let", t);
                  assertTokenKeyword("upper", "loop", t);
                  assertTokenKeyword("upper", "new", t);
                  assertTokenKeyword("upper", "not", t);
                  assertTokenKeyword("upper", "of", t);
                  assertTokenKeyword("upper", "pool", t);
                  assertTokenKeyword("upper", "then", t);
                  assertTokenType("upper", "TRUE", t);
                  assertTokenKeyword("upper", "while", t);
               };

            let t : Tokenizer <- newTokenizer("Case Class Else Esac False Fi If In Inherits Isvoid Let Loop New Not Of Pool Then True While") in
               {
                  assertTokenKeyword("title", "case", t);
                  assertTokenKeyword("title", "class", t);
                  assertTokenKeyword("title", "else", t);
                  assertTokenKeyword("title", "esac", t);
                  assertTokenType("title", "False", t);
                  assertTokenKeyword("title", "fi", t);
                  assertTokenKeyword("title", "if", t);
                  assertTokenKeyword("title", "in", t);
                  assertTokenKeyword("title", "inherits", t);
                  assertTokenKeyword("title", "isvoid", t);
                  assertTokenKeyword("title", "let", t);
                  assertTokenKeyword("title", "loop", t);
                  assertTokenKeyword("title", "new", t);
                  assertTokenKeyword("title", "not", t);
                  assertTokenKeyword("title", "of", t);
                  assertTokenKeyword("title", "pool", t);
                  assertTokenKeyword("title", "then", t);
                  assertTokenType("title", "True", t);
                  assertTokenKeyword("title", "while", t);
               };

            let t : Tokenizer <- newTokenizer("cASE cLASS eLSE eSAC fALSE fI iF iN iNHERITS iSVOID lET lOOP nEW nOT oF pOOL tHEN tRUE wHILE") in
               {
                  assertTokenKeyword("inverseTitle", "case", t);
                  assertTokenKeyword("inverseTitle", "class", t);
                  assertTokenKeyword("inverseTitle", "else", t);
                  assertTokenKeyword("inverseTitle", "esac", t);
                  assertTokenKeyword("inverseTitle", "false", t);
                  assertTokenKeyword("inverseTitle", "fi", t);
                  assertTokenKeyword("inverseTitle", "if", t);
                  assertTokenKeyword("inverseTitle", "in", t);
                  assertTokenKeyword("inverseTitle", "inherits", t);
                  assertTokenKeyword("inverseTitle", "isvoid", t);
                  assertTokenKeyword("inverseTitle", "let", t);
                  assertTokenKeyword("inverseTitle", "loop", t);
                  assertTokenKeyword("inverseTitle", "new", t);
                  assertTokenKeyword("inverseTitle", "not", t);
                  assertTokenKeyword("inverseTitle", "of", t);
                  assertTokenKeyword("inverseTitle", "pool", t);
                  assertTokenKeyword("inverseTitle", "then", t);
                  assertTokenKeyword("inverseTitle", "true", t);
                  assertTokenKeyword("inverseTitle", "while", t);
               };

            let t : Tokenizer <- newTokenizer("cASe cLASs eLSe eSAc fALSe fi if in iNHERITs iSVOId lEt lOOp nEw nOt of pOOl tHEn tRUe wHILe") in
               {
                  assertTokenKeyword("mixed", "case", t);
                  assertTokenKeyword("mixed", "class", t);
                  assertTokenKeyword("mixed", "else", t);
                  assertTokenKeyword("mixed", "esac", t);
                  assertTokenKeyword("mixed", "false", t);
                  assertTokenKeyword("mixed", "fi", t);
                  assertTokenKeyword("mixed", "if", t);
                  assertTokenKeyword("mixed", "in", t);
                  assertTokenKeyword("mixed", "inherits", t);
                  assertTokenKeyword("mixed", "isvoid", t);
                  assertTokenKeyword("mixed", "let", t);
                  assertTokenKeyword("mixed", "loop", t);
                  assertTokenKeyword("mixed", "new", t);
                  assertTokenKeyword("mixed", "not", t);
                  assertTokenKeyword("mixed", "of", t);
                  assertTokenKeyword("mixed", "pool", t);
                  assertTokenKeyword("mixed", "then", t);
                  assertTokenKeyword("mixed", "true", t);
                  assertTokenKeyword("mixed", "while", t);
               };

            let t : Tokenizer <- newTokenizer("CasE ClasS ElsE EsaC FalsE FI IF IN InheritS IsvoiD LeT LooP NeW NoT OF PooL TheN TruE WhilE") in
               {
                  assertTokenKeyword("mixed2", "case", t);
                  assertTokenKeyword("mixed2", "class", t);
                  assertTokenKeyword("mixed2", "else", t);
                  assertTokenKeyword("mixed2", "esac", t);
                  assertTokenType("mixed2", "FalsE", t);
                  assertTokenKeyword("mixed2", "fi", t);
                  assertTokenKeyword("mixed2", "if", t);
                  assertTokenKeyword("mixed2", "in", t);
                  assertTokenKeyword("mixed2", "inherits", t);
                  assertTokenKeyword("mixed2", "isvoid", t);
                  assertTokenKeyword("mixed2", "let", t);
                  assertTokenKeyword("mixed2", "loop", t);
                  assertTokenKeyword("mixed2", "new", t);
                  assertTokenKeyword("mixed2", "not", t);
                  assertTokenKeyword("mixed2", "of", t);
                  assertTokenKeyword("mixed2", "pool", t);
                  assertTokenKeyword("mixed2", "then", t);
                  assertTokenType("mixed2", "TruE", t);
                  assertTokenKeyword("mixed2", "while", t);
               };
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
            let t : Tokenizer <- newTokenizer("\"\"") in
               assertTokenString("empty", "", t);
            let t : Tokenizer <- newTokenizer("\"\"").setUva(true) in
               assertTokenString("uva empty", "", t);

            let t : Tokenizer <- newTokenizer("\"a\"") in
               assertTokenString("single", "a", t);
            let t : Tokenizer <- newTokenizer("\"a\"").setUva(true) in
               assertTokenString("uva single", "a", t);

            let t : Tokenizer <- newTokenizer("\"\\c\"") in
               assertTokenString("escape ignored", "c", t);
            let t : Tokenizer <- newTokenizer("\"\\c\"").setUva(true) in
               assertTokenString("uva escape ignored", stringUtil.backslash().concat("c"), t);

            let t : Tokenizer <- newTokenizer("\"\\b\"") in
               assertTokenStringEscapes("backspace", "\b", "\b".length() - 1, t);
            let t : Tokenizer <- newTokenizer("\"\\b\"").setUva(true) in
               assertTokenString("uva backspace", stringUtil.backslash().concat("b"), t);

            let t : Tokenizer <- newTokenizer("\"\\t\"") in
               assertTokenStringEscapes("tab", "\t", "\t".length() - 1, t);
            let t : Tokenizer <- newTokenizer("\"\\t\"").setUva(true) in
               assertTokenString("uva tab", stringUtil.backslash().concat("t"), t);

            let t : Tokenizer <- newTokenizer("\"\\n\"") in
               assertTokenStringEscapes("linefeed", "\n", "\n".length() - 1, t);
            let t : Tokenizer <- newTokenizer("\"\\n\"").setUva(true) in
               assertTokenString("uva linefeed", stringUtil.backslash().concat("n"), t);

            let t : Tokenizer <- newTokenizer("\"\\f\"") in
               assertTokenStringEscapes("formfeed", "\f", "\f".length() - 1, t);
            let t : Tokenizer <- newTokenizer("\"\\f\"").setUva(true) in
               assertTokenString("uva formfeed", stringUtil.backslash().concat("f"), t);

            let t : Tokenizer <- newTokenizer("\"\\\\\"") in
               assertTokenStringEscapes("backslash", "\\", "\\".length() - 1, t);
            let t : Tokenizer <- newTokenizer("\"\\\\\"").setUva(true) in
               assertTokenString("uva backslash", stringUtil.backslash().concat(stringUtil.backslash()), t);

            let t : Tokenizer <- newTokenizer("\"\\\"\"") in
               assertTokenString("double quote", stringUtil.doubleQuote(), t);
            let t : Tokenizer <- newTokenizer("\"\\\"\"").setUva(true) in
               assertTokenString("uva double quote", stringUtil.backslash().concat(stringUtil.doubleQuote()), t);

            let t : Tokenizer <- newTokenizer("\"\\\n\"") in
               assertTokenStringEscapes("escaped linefeed", "\n", "\n".length() - 1, t);
            let t : Tokenizer <- newTokenizer("\"\\\n\"").setUva(true) in
               assertTokenError("uva escaped linefeed", "line 1: unexpected newline in string", t);

            let t : Tokenizer <- newTokenizer("\"\\n\\n\"") in
               assertTokenStringEscapes("linefeed linefeed", "\n\n", "\n\n".length() - 2, t);
            let t : Tokenizer <- newTokenizer("\"\\n\\n\"").setUva(true) in
               assertTokenString("uva linefeed linefeed",
                     stringUtil.backslash().concat("n").concat(stringUtil.backslash()).concat("n"), t);

            let s8 : String <- "01234567",
                  s64 : String <- s8.concat(s8).concat(s8).concat(s8).concat(s8).concat(s8).concat(s8).concat(s8),
                  s512 : String <- s64.concat(s64).concat(s64).concat(s64).concat(s64).concat(s64).concat(s64).concat(s64),
                  s1024 : String <- s512.concat(s512) in
               {
                  assertTokenString("length", s1024, newTokenizer("\"".concat(s1024).concat("\"")));
                  assertTokenError("length", "line 1: maximum string constant length exceeded",
                        newTokenizer("\"x".concat(s1024).concat("\"")));
               };

            let s1 : String <- stringUtil.backslash(),
                  s8 : String <- s1.concat(s1).concat(s1).concat(s1).concat(s1).concat(s1).concat(s1).concat(s1),
                  s64 : String <- s8.concat(s8).concat(s8).concat(s8).concat(s8).concat(s8).concat(s8).concat(s8),
                  s512 : String <- s64.concat(s64).concat(s64).concat(s64).concat(s64).concat(s64).concat(s64).concat(s64),
                  s1024 : String <- s512.concat(s512),
                  s2048 : String <- s1024.concat(s1024),
                  s4096 : String <- s2048.concat(s2048) in
               {
                  -- 4096 TestStringInputStream backslashes = 2048 "input"
                  -- backslashes = 1024 string characters
                  assertTokenStringEscapes("escape length", s2048, 1024, newTokenizer("\"".concat(s4096).concat("\"")));
                  assertTokenError("escape length", "line 1: maximum string constant length exceeded",
                        newTokenizer("\"x".concat(s4096).concat("\"")));

                  -- 2048 TestStringInputStream backslashes = 1024 "input"
                  -- backslashes = 1024 UVA string characters
                  assertTokenString("uva escape length", s1024,
                        newTokenizer("\"".concat(s2048).concat("\"")).setUva(true));
                  assertTokenError("uva escape error length", "line 1: maximum string constant length exceeded",
                        newTokenizer("\"x".concat(s2048).concat("\"")).setUva(true));
               };

            assertTokenError("", "line 1: unexpected EOF in string", newTokenizer("\""));
            assertTokenError("", "line 1: unexpected newline in string", newTokenizer("\"\n\""));
         }
      else false fi
   };
};

class TestTokenizerListener inherits TokenizerListener {
   is : IOInputStream;

   init(is_ : IOInputStream) : SELF_TYPE {{
      is <- is_;
      self;
   }};

   setEmptyLineEofCount(n : Int) : Object {
      is.setEmptyLineEofCount(n)
   };

   eof() : Object {
      is.reset()
   };
};

class TestOptionTokenizerListener inherits TokenizerListener {
   options : Collection <- new LinkedList;
   options() : Collection { options };

   handleOption(s : String) : Object {
      options.add(s)
   };
};

class TestUvaTokenizerListener inherits TokenizerListener {
   uva : Bool;
   uva() : Bool { uva };
   setUva(uva_ : Bool) : Object { uva <- uva_ };
};
