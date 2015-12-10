class Token {
   toString() : String { "<invalid token>" };
   isEof() : Bool { false };
   asPunct() : TokenPunct { let void : TokenPunct in void };
   asBinaryOp() : TokenBinaryOp { let void : TokenBinaryOp in void };
   asKeyword() : TokenKeyword { let void : TokenKeyword in void };
   asType() : TokenType { let void : TokenType in void };
   asId() : TokenId { let void : TokenId in void };
   asInteger() : TokenInteger { let void : TokenInteger in void };
   asString() : TokenString { let void : TokenString in void };
};

class TokenEof inherits Token {
   isEof() : Bool { true };
   toString() : String { "EOF" };
};

class TokenError inherits Token {
   s : String <- "<invalid error>";

   init(s_ : String) : SELF_TYPE {{
      s <- s_;
      self;
   }};

   toString() : String { "ERROR: ".concat(s) };
   isEof() : Bool { true };
};

class TokenStringValued inherits Token {
   value : String;

   init(value_ : String) : SELF_TYPE {{
      value <- value_;
      self;
   }};

   toString() : String { value };
   value() : String { value };
};

class TokenPunct inherits TokenStringValued {
   asPunct() : TokenPunct { self };
};

class TokenBinaryOp inherits TokenStringValued {
   prec : Int;
   prec() : Int { prec };

   initOp(prec_ : Int) : SELF_TYPE {{
      prec <- prec_;
      self;
   }};

   asBinaryOp() : TokenBinaryOp { self };
};

class TokenKeyword inherits TokenStringValued {
   asKeyword() : TokenKeyword { self };
};

class TokenType inherits TokenStringValued {
   toString() : String { "TYPE[".concat(value).concat("]") };
   asType() : TokenType { self };
};

class TokenId inherits TokenStringValued {
   toString() : String { "ID[".concat(value).concat("]") };
   asId() : TokenId { self };
};

class TokenInteger inherits Token {
   value : Int;
   value() : Int { value };

   init(value_ : Int) : SELF_TYPE {{
      value <- value_;
      self;
   }};

   toString() : String { new StringUtil.fromInt(value) };
   asInteger() : TokenInteger { self };
};

class TokenString inherits TokenStringValued {
   toString() : String {
      let doubleQuote : String <- new StringUtil.doubleQuote() in
         doubleQuote.concat(value).concat(doubleQuote)
   };

   asString() : TokenString { self };
};

class Tokenizer {
   stringUtil : StringUtil <- new StringUtil;
   is : InputStream;
   next : String;
   line : Int <- 1;

   init(is_ : InputStream) : SELF_TYPE {{
      is <- is_;
      self;
   }};

   line() : Int { line };

   readChar() : String {
      if next = "" then
         let c : String <- is.read() in
            {
               if c = "\n" then
                  line <- line + 1
               else false fi;

               --new IO.out_string("readChar: [").out_string(c).out_string("]\n");
               c;
            }
      else
         let c : String <- next in
            {
               next <- "";
               c;
            }
      fi
   };

   unreadChar(c : String) : Object {
      next <- c
   };

   matchChar(match : String) : Bool {
      let c : String <- readChar() in
         if c = match then
            true
         else
            {
               unreadChar(c);
               false;
            }
         fi
   };

   skipLine() : Object {
      let continue : Bool <- true in
         while
            let c : String <- readChar() in
               if c = "" then
                  false
               else
                  not c = "\n"
               fi
         loop false pool
   };

   newTokenError(s : String) : TokenError {
      new TokenError.init("line ".concat(stringUtil.fromInt(line)).concat(": ").concat(s))
   };

   isWhitespace(c : String) : Bool {
      if c = " " then true else
      if c = "\n" then true else
      if c = "\f" then true else
      -- FIXME: There's no way to represent "\r"
      if c = "\t" then true else
      -- FIXME: There's no way to represent "\v"
         false
      fi fi fi fi
   };

   isUpper(c : String) : Bool {
      if c = "A" then true else
      if c = "B" then true else
      if c = "C" then true else
      if c = "D" then true else
      if c = "E" then true else
      if c = "F" then true else
      if c = "G" then true else
      if c = "H" then true else
      if c = "I" then true else
      if c = "J" then true else
      if c = "K" then true else
      if c = "L" then true else
      if c = "M" then true else
      if c = "N" then true else
      if c = "O" then true else
      if c = "P" then true else
      if c = "Q" then true else
      if c = "R" then true else
      if c = "S" then true else
      if c = "T" then true else
      if c = "U" then true else
      if c = "V" then true else
      if c = "W" then true else
      if c = "X" then true else
      if c = "Y" then true else
      if c = "Z" then true else
         false
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi fi fi fi
      fi
   };

   isLower(c : String) : Bool {
      if c = "a" then true else
      if c = "b" then true else
      if c = "c" then true else
      if c = "d" then true else
      if c = "e" then true else
      if c = "f" then true else
      if c = "g" then true else
      if c = "h" then true else
      if c = "i" then true else
      if c = "j" then true else
      if c = "k" then true else
      if c = "l" then true else
      if c = "m" then true else
      if c = "n" then true else
      if c = "o" then true else
      if c = "p" then true else
      if c = "q" then true else
      if c = "r" then true else
      if c = "s" then true else
      if c = "t" then true else
      if c = "u" then true else
      if c = "v" then true else
      if c = "w" then true else
      if c = "x" then true else
      if c = "y" then true else
      if c = "z" then true else
         false
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi fi fi fi
      fi
   };

   isIdentPart(c : String) : Bool {
      if isUpper(c) then
         true
      else
         if isLower(c) then
            true
         else
            if 0 <= stringUtil.toDigit(c) then
               true
            else
               c = "_"
            fi
         fi
      fi
   };

   readParenComment() : Token {
      let continue : Bool <- true,
            token : Token in
         {
            while continue loop
               let c : String <- readChar() in
                  if c = "" then
                     {
                        token <- newTokenError("unexpected EOF in enclosing comment");
                        continue <- false;
                     }
                  else
                     if c = "*" then
                        {
                           while c = "*" loop
                              c <- readChar()
                           pool;

                           if c = ")" then
                              continue <- false
                           else false fi;
                        }
                     else false fi
                  fi
            pool;

            token;
         }
   };

   readLineComment() : Token {{
      skipLine();
      let void : Token in void;
   }};

   readIdentName(c : String) : String {
      let s : String <- c in
         {
            c <- readChar();
            while isIdentPart(c) loop
               {
                  s <- s.concat(c);
                  c <- readChar();
               }
            pool;
            unreadChar(c);

            s;
         }
   };

   isKeyword(s : String) : Bool {
      if s = "case" then true else
      if s = "class" then true else
      if s = "else" then true else
      if s = "esac" then true else
      if s = "false" then true else
      if s = "fi" then true else
      if s = "if" then true else
      if s = "in" then true else
      if s = "inherits" then true else
      if s = "isvoid" then true else
      if s = "let" then true else
      if s = "loop" then true else
      if s = "new" then true else
      if s = "not" then true else
      if s = "of" then true else
      if s = "pool" then true else
      if s = "then" then true else
      if s = "true" then true else
      if s = "while" then true else
         false
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi fi fi
   };

   -- Read token for a character that cannot be part of a multi-character token
   -- (exclude "-" and "<").
   readSingleCharPunct(c : String) : Token {
      if c = "{" then new TokenPunct.init(c) else
      if c = "}" then new TokenPunct.init(c) else
      -- "(" can be part of "(*"
      if c = ")" then new TokenPunct.init(c) else
      if c = ":" then new TokenPunct.init(c) else
      if c = "," then new TokenPunct.init(c) else
      if c = "@" then new TokenPunct.init(c) else
      if c = "." then new TokenPunct.init(c) else
      if c = ";" then new TokenPunct.init(c) else
      if c = "+" then new TokenBinaryOp.init(c).initOp(4) else
      -- "-" can be part of "--"
      if c = "*" then new TokenBinaryOp.init(c).initOp(5) else
      if c = "/" then new TokenBinaryOp.init(c).initOp(5) else
      if c = "~" then new TokenPunct.init(c) else
      -- "<" can be part of "<-"
      -- "=" can be part of "=>"
         let void : Token in void
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi
   };

   readPunct(c : String) : Token {
      if c = "<" then
         let c : String <- readChar() in
            if c = "-" then
               new TokenPunct.init("<-")
            else
               if c = "=" then
                  new TokenBinaryOp.init("<=").initOp(3)
               else
                  {
                     unreadChar(c);
                     new TokenBinaryOp.init("<").initOp(3);
                  }
               fi
            fi

      else
         if c = "=" then
            if matchChar(">") then
               new TokenPunct.init("=>")
            else
               new TokenBinaryOp.init("=").initOp(3)
            fi

         else
            readSingleCharPunct(c)
         fi
      fi
   };

   readString(c : String) : Token {
      let s : String,
            token : Token in
         {
            c <- readChar();
            while not c = stringUtil.doubleQuote() loop
               {
                  if c = stringUtil.backslash() then
                     {
                        c <- readChar();
                        if c = "" then
                           {
                              token <- newTokenError("unexpected EOF in string");
                              c <- stringUtil.doubleQuote();
                           }
                        else if c = "b" then c <- "\b"
                        else if c = "t" then c <- "\t"
                        else if c = "n" then c <- "\n"
                        else if c = "f" then c <- "\f"
                        else
                           false
                        fi fi fi fi fi;
                     }
                  else false fi;

                  -- TODO: multiline string error.

                  s <- s.concat(c);
                  c <- readChar();
               }
            pool;

            if isvoid token then
               new TokenString.init(s)
            else
               token
            fi;
         }
   };

   readInteger(c : String) : Token {
      let n : Int <- stringUtil.toDigit(c),
            token : Token in
         {
            if 0 <= n then
               let continue : Bool <- true in
                  while continue loop
                     let c : String <- readChar(),
                           digit : Int <- stringUtil.toDigit(c) in
                        if digit < 0 then
                           {
                              unreadChar(c);
                              token <- new TokenInteger.init(n);
                              continue <- false;
                           }
                        else
                           let newN : Int <- (n * 10) + digit in
                              if newN < 0 then
                                 let s : String <- stringUtil.fromInt(n).concat(c) in
                                    {
                                       while continue loop
                                          let c : String <- readChar() in
                                             if stringUtil.toDigit(c) < 0 then
                                                {
                                                   unreadChar(c);
                                                   continue <- false;
                                                }
                                             else
                                                s <- s.concat(c)
                                             fi
                                       pool;

                                       token <- newTokenError("integer overflow: ".concat(s));
                                       continue <- false;
                                    }
                              else
                                 n <- newN
                              fi
                        fi
                  pool
            else false fi;

            token;
         }
   };

   readToken(c : String) : Token {
      if c = "" then
         new TokenEof
      else
         if isWhitespace(c) then
            let void : Token in void

         else
            if c = "(" then
               if matchChar("*") then
                  readParenComment()
               else
                  new TokenPunct.init("(")
               fi

            else
               if c = "-" then
                  if matchChar("-") then
                     readLineComment()
                  else
                     new TokenBinaryOp.init("-").initOp(4)
                  fi

               else
                  let token : Token <- readPunct(c) in
                     if not isvoid token then
                        token
                     else
                        let token : Token <- readInteger(c) in
                           if not isvoid token then
                              token
                           else
                              if c = stringUtil.doubleQuote() then
                                 readString(c)
                              else
                                 if isUpper(c) then
                                    new TokenType.init(readIdentName(c))
                                 else
                                    if isLower(c) then
                                       let name : String <- readIdentName(c) in
                                          if isKeyword(name) then
                                             new TokenKeyword.init(name)
                                          else
                                             new TokenId.init(name)
                                          fi
                                    else
                                       newTokenError("invalid character: [".concat(c).concat("]"))
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

   next() : Token {
      let token : Token in
         {
            while isvoid token loop
               let c : String <- readChar() in
                  token <- readToken(c)
            pool;

            --new IO.out_string("token=").out_string(token.toString()).out_string("\n");
            token;
         }
   };
};
