class Token {
   toString() : String { "<invalid token>" };
   asEof() : TokenEof { let void : TokenEof in void };
   asError() : TokenError { let void : TokenError in void };
   asPunct() : TokenPunct { let void : TokenPunct in void };
   asBinaryOp() : TokenBinaryOp { let void : TokenBinaryOp in void };
   asKeyword() : TokenKeyword { let void : TokenKeyword in void };
   asType() : TokenType { let void : TokenType in void };
   asId() : TokenId { let void : TokenId in void };
   asInteger() : TokenInteger { let void : TokenInteger in void };
   asString() : TokenString { let void : TokenString in void };
};

class TokenEof inherits Token {
   moreFiles : Bool;
   moreFiles() : Bool { moreFiles };

   init(moreFiles_ : Bool) : SELF_TYPE {{
      moreFiles <- moreFiles_;
      self;
   }};

   toString() : String { "EOF".concat(if moreFiles then "[moreFiles]" else "" fi) };
   asEof() : TokenEof { self };
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

class TokenError inherits Token {
   line : Int;
   line() : Int { line };

   value : String;
   value() : String { value };

   init(line_ : Int, value_ : String) : SELF_TYPE {{
      line <- line_;
      value <- value_;
      self;
   }};

   toString() : String { "ERROR: line ".concat(new StringUtil.fromInt(line)).concat(": ").concat(value) };
   asError() : TokenError { self };
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

class TokenString inherits Token {
   line : Int;
   line() : Int { line };

   value : String;
   value() : String { value };

   -- The number of literal two-character escape sequences used in this string
   -- (e.g., "\n" actually stored as "\\n").  In UVA mode, the number of "\t"
   -- and "\n" escape sequences (the sequences handled by IO.out_string).
   escapes : Int;
   escapes() : Int { escapes };

   init(line_ : Int, value_ : String, escapes_ : Int) : SELF_TYPE {{
      line <- line_;
      value <- value_;
      escapes <- escapes_;
      self;
   }};

   toString() : String {
      let doubleQuote : String <- new StringUtil.doubleQuote() in
         doubleQuote.concat(value).concat(doubleQuote)
   };

   asString() : TokenString { self };
};

class TokenizerListener {
   setEmptyLineEofCount(n : Int) : Object { false };
   eof() : Object { false };
   setUva(uva : Bool) : Object { false };
   handleOption(option : String) : Object { false };
};

class TokenizerLineMapFile {
   file : String;
   file() : String { file };

   firstTotalLine : Int;
   firstTotalLine() : Int { firstTotalLine };

   init(file_ : String, firstTotalLine_ : Int) : SELF_TYPE {{
      file <- file_;
      firstTotalLine <- firstTotalLine_;
      self;
   }};
};

class TokenizerLineMap {
   stringUtil : StringUtil;
   files : LinkedList <- new LinkedList.addFirst(new TokenizerLineMapFile.init("", 1));

   init(stringUtil_ : StringUtil) : SELF_TYPE {{
      stringUtil <- stringUtil_;
      self;
   }};

   beginFile(file : String, firstTotalLine : Int) : Object {
      files.addFirst(new TokenizerLineMapFile.init(file, firstTotalLine))
   };

   lineToString(totalLine : Int) : String {
      let continue : Bool <- true,
            iter : Iterator <- files.iterator(),
            file : String,
            line : Int,
            result : String in
         {
            while continue loop
               if iter.next() then
                  let mapFile : TokenizerLineMapFile <- case iter.get() of x : TokenizerLineMapFile => x; esac,
                        firstTotalLine : Int <- mapFile.firstTotalLine() in
                     if firstTotalLine <= totalLine then
                        {
                           file <- mapFile.file();
                           line <- totalLine - firstTotalLine + 1;
                           continue <- false;
                        }
                     else false fi
               else
                  continue <- false
               fi
            pool;

            if file = "" then "" else file.concat(": ") fi.concat("line ").concat(stringUtil.fromInt(line));
         }
   };
};

class Tokenizer {
   stringUtil : StringUtil <- new StringUtil;
   is : InputStream;
   next : String;
   line : Int <- 1;

   lineMap : TokenizerLineMap <- new TokenizerLineMap.init(stringUtil);
   lineMap() : TokenizerLineMap { lineMap };

   numFiles : Int;
   maxFiles : Int <- 1;

   bsChar : String; -- "\b"
   tabChar : String; -- "\t"
   vtChar : String; -- "\v"
   ffChar : String; -- "\f"
   crChar : String; -- "\r"

   lowerIdentName : String;

   listener : TokenizerListener;

   setListener(listener_ : TokenizerListener) : SELF_TYPE {{
      listener <- listener_;
      self;
   }};

   uva : Bool;
   setUva(uva_ : Bool) : SELF_TYPE {{
      uva <- uva_;
      self;
   }};

   init(is_ : InputStream) : SELF_TYPE {{
      is <- is_;
      self;
   }};

   line() : Int { line };

   readChar() : String {
      let c : String in
         {
            if next = "" then
               c <- is.read()
            else
               {
                  c <- next;
                  next <- "";
               }
            fi;

            if c = "\n" then
               line <- line + 1
            else false fi;

            c;
         }
   };

   unreadChar(c : String) : Object {{
      if c = "\n" then
         line <- line - 1
      else false fi;

      next <- c;
   }};

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

   readUntilEOL() : String {
      let s : String,
            continue : Bool <- true in
         {
            while
               let c : String <- readChar() in
                  if c = "" then
                     false
                  else
                     if c = "\n" then
                        {
                           unreadChar("\n");
                           false;
                        }
                     else
                        {
                           s <- s.concat(c);
                           true;
                        }
                     fi
                  fi
            loop false pool;

            s;
         }
   };

   newTokenErrorAt(line : Int, s : String) : TokenError {
      new TokenError.init(line, s)
   };

   newTokenError(s : String) : TokenError {
      newTokenErrorAt(line, s)
   };

   isWhitespace(c : String) : Bool {
      if c = " " then true else
      if c = "\n" then true else
      if c = ffChar then true else
      if c = crChar then true else
      if c = "\t" then true else
      if c = tabChar then true else
      if c = vtChar then true else
         false
      fi fi fi fi fi
      fi fi
   };

   upperCharToLower(c : String) : String {
      if c = "A" then "a" else
      if c = "B" then "b" else
      if c = "C" then "c" else
      if c = "D" then "d" else
      if c = "E" then "e" else
      if c = "F" then "f" else
      if c = "G" then "g" else
      if c = "H" then "h" else
      if c = "I" then "i" else
      if c = "J" then "j" else
      if c = "K" then "k" else
      if c = "L" then "l" else
      if c = "M" then "m" else
      if c = "N" then "n" else
      if c = "O" then "o" else
      if c = "P" then "p" else
      if c = "Q" then "q" else
      if c = "R" then "r" else
      if c = "S" then "s" else
      if c = "T" then "t" else
      if c = "U" then "u" else
      if c = "V" then "v" else
      if c = "W" then "w" else
      if c = "X" then "x" else
      if c = "Y" then "y" else
      if c = "Z" then "z" else
         ""
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

   readParenComment() : Token {
      let nested : Int <- 1,
            line_ : Int <- line,
            token : Token in
         {
            while 0 < nested loop
               let c : String <- readChar() in
                  if c = "" then
                     {
                        token <- newTokenErrorAt(line_, "unexpected EOF in enclosing comment");
                        nested <- 0;
                     }
                  else
                     if c = "*" then
                        {
                           while c = "*" loop
                              c <- readChar()
                           pool;

                           if c = ")" then
                              nested <- nested - 1
                           else false fi;
                        }
                     else
                        if c = "(" then
                           {
                              while c = "(" loop
                                 c <- readChar()
                              pool;

                              if c = "*" then
                                 nested <- nested + 1
                              else false fi;
                           }
                        else false fi
                     fi
                  fi
            pool;

            token;
         }
   };

   readEofDirective() : Object {
      let tokenInteger : TokenInteger <- readInteger("0").asInteger() in
         if not isvoid tokenInteger then
            if not isvoid listener then
               listener.setEmptyLineEofCount(tokenInteger.value())
            else false fi
         else false fi
   };

   readEscapeDirective() : Object {
      while matchChar(":") loop
         if matchChar(stringUtil.backslash()) then
            let c : String <- readChar() in
               if matchChar("=") then
                  let value : String <- readChar() in
                     if c = "b" then
                        bsChar <- value
                     else
                        if c = "t" then
                           tabChar <- value
                        else
                           if c = "v" then
                              vtChar <- value
                           else
                              if c = "f" then
                                 ffChar <- value
                              else
                                 if c = "r" then
                                    crChar <- value
                                 else false fi
                              fi
                           fi
                        fi
                     fi
               else false fi
         else false fi
      pool
   };

   readFileDirective() : Object {
      let file : String <- readUntilEOL() in
         lineMap.beginFile(file, line + 1)
   };

   readFilesDirective() : Object {
      let tokenInteger : TokenInteger <- readInteger("0").asInteger() in
         if not isvoid tokenInteger then
            maxFiles <- tokenInteger.value()
         else false fi
   };

   readOptionDirective() : Object {
      let option : String <- readUntilEOL() in
         if not isvoid listener then
            listener.handleOption(option)
         else false fi
   };

   readUvaDirective() : Object {{
      uva <- true;
      if not isvoid listener then
         listener.setUva(true)
      else false fi;
   }};

   readDirective() : Object {
      if matchChar("e") then
         if matchChar("o") then
            if matchChar("f") then
               if matchChar("=") then
                  readEofDirective()
               else false fi
            else false fi
         else
            if matchChar("s") then
               if matchChar("c") then
                  if matchChar("a") then
                     if matchChar("p") then
                        if matchChar("e") then
                           readEscapeDirective()
                        else false fi
                     else false fi
                  else false fi
               else false fi
            else false fi
         fi
      else
         if matchChar("f") then
            if matchChar("i") then
               if matchChar("l") then
                  if matchChar("e") then
                     if matchChar("=") then
                        readFileDirective()
                     else
                        if matchChar("s") then
                           if matchChar("=") then
                              readFilesDirective()
                           else false fi
                        else false fi
                     fi
                  else false fi
               else false fi
            else false fi
         else
            if matchChar("o") then
               if matchChar("p") then
                  if matchChar("t") then
                     if matchChar("i") then
                        if matchChar("o") then
                           if matchChar("n") then
                              if matchChar("=") then
                                 readOptionDirective()
                              else false fi
                           else false fi
                        else false fi
                     else false fi
                  else false fi
               else false fi
            else
               if matchChar("u") then
                  if matchChar("v") then
                     if matchChar("a") then
                        if matchChar("\n") then
                           {
                              unreadChar("\n");
                              readUvaDirective();
                           }
                        else false fi
                     else false fi
                  else false fi
               else false fi
            fi
         fi
      fi
   };

   readLineComment() : Token {{
      if matchChar("c") then
         if matchChar("o") then
            if matchChar("o") then
               if matchChar("l") then
                  if matchChar(":") then
                     {
                        line <- line - 1;
                        readDirective();
                     }
                  else false fi
               else false fi
            else false fi
         else false fi
      else false fi;

      skipLine();
      let void : Token in void;
   }};

   readIdentName(c : String, lower : String) : String {{
      lowerIdentName <- lower;

      let s : String <- c in
         {
            let continue : Bool <- true in
               while continue loop
                  let c : String <- readChar(),
                        lower : String <- upperCharToLower(c) in
                     if not lower = "" then
                        {
                           s <- s.concat(c);
                           lowerIdentName <- lowerIdentName.concat(lower);
                        }
                     else
                        if if isLower(c) then
                              true
                           else
                              if 0 <= stringUtil.toDigit(c) then
                                 true
                              else
                                 c = "_"
                              fi
                           fi
                        then
                           {
                              s <- s.concat(c);
                              lowerIdentName <- lowerIdentName.concat(c);
                           }
                        else
                           {
                              continue <- false;
                              unreadChar(c);
                           }
                        fi
                     fi
               pool;

            s;
         };
   }};

   isKeyword(s : String) : Bool {
      if s = "case" then true else
      if s = "class" then true else
      if s = "else" then true else
      if s = "esac" then true else
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
      if s = "while" then true else
         false
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi fi fi fi
      fi fi
   };

   isKeywordOrConstantBool(s : String) : Bool {
      if isKeyword(s) then
         true
      else
         if s = "false" then
            true
         else
            s = "true"
         fi
      fi
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

   readString() : Token {
      let s : String,
            escapes : Int,
            token : Token,
            line_ : Int <- line,
            c : String <- readChar() in
         {
            let continue : Bool <- true in
               while continue loop
                  if c = "" then
                     {
                        token <- newTokenErrorAt(line_, "unexpected EOF in string");
                        continue <- false;
                     }
                  else
                     if c = "\n" then
                        {
                           token <- newTokenErrorAt(line_, "unexpected newline in string");
                           continue <- false;
                        }
                     else
                        if c = stringUtil.doubleQuote() then
                           continue <- false
                        else
                           {
                              if c = stringUtil.backslash() then
                                 if uva then
                                    let c2 : String <- readChar() in
                                       if c2 = "\n" then
                                          if isvoid token then
                                             token <- newTokenErrorAt(line_, "unexpected newline in string")
                                          else false fi
                                       else
                                          {
                                             if if c2 = "n" then
                                                   true
                                                else
                                                   c2 = "t"
                                                fi
                                             then
                                                escapes <- escapes + 1
                                             else
                                                if c2 = stringUtil.backslash() then
                                                   let c3 : String <- readChar() in
                                                      {
                                                         if if c3 = "n" then
                                                               true
                                                            else
                                                               c3 = "t"
                                                            fi
                                                         then
                                                            escapes <- escapes + 1
                                                         else false fi;

                                                         unreadChar(c3);
                                                      }
                                                else false fi
                                             fi;

                                             c <- c.concat(c2);
                                          }
                                       fi
                                 else
                                    {
                                       c <- readChar();
                                       if c = "b" then
                                          c <- if bsChar = "" then "\b" else bsChar fi
                                       else
                                          if c = "t" then
                                             c <- if tabChar = "" then "\t" else tabChar fi
                                          else
                                             if c = "n" then
                                                c <- "\n"
                                             else
                                                if c = "f" then
                                                   c <- if ffChar = "" then "\f" else ffChar fi
                                                else
                                                   if c = stringUtil.backslash() then
                                                      -- Support UVA Cool dialect.
                                                      if "\\".length() = 2 then
                                                         c <- "\\"
                                                      else false fi
                                                   else false fi
                                                fi
                                             fi
                                          fi
                                       fi;

                                       if c.length() = 2 then
                                          escapes <- escapes + 1
                                       else false fi;
                                    }
                                 fi
                              else
                                 -- XXX: No way to represent NUL.
                                 false
                              fi;

                              s <- s.concat(c);
                              if not s.length() - escapes <= 1024 then
                                 if isvoid token then
                                    token <- newTokenErrorAt(line_, "maximum string constant length exceeded")
                                 else false fi
                              else false fi;

                              c <- readChar();
                           }
                        fi
                     fi
                  fi
               pool;

            if isvoid token then
               new TokenString.init(line_, s, escapes)
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
         {
            if not isvoid listener then
               listener.eof()
            else false fi;

            numFiles <- numFiles + 1;
            new TokenEof.init(numFiles < maxFiles);
         }
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
                                 readString()
                              else
                                 let lower : String <- upperCharToLower(c) in
                                    if not lower = "" then
                                       let name : String <- readIdentName(c, lower) in
                                          if isKeyword(lowerIdentName) then
                                             new TokenKeyword.init(lowerIdentName)
                                          else
                                             new TokenType.init(name)
                                          fi
                                    else
                                       if isLower(c) then
                                          let name : String <- readIdentName(c, c) in
                                             if isKeywordOrConstantBool(lowerIdentName) then
                                                new TokenKeyword.init(lowerIdentName)
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

            token;
         }
   };
};
