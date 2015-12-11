class InputStream {
   -- Returns the next byte as a single length string, or the empty string if
   -- there are no remaining bytes.
   read() : String { "" };
};

-- InputStream implemented using IO.in_string.
class IOInputStream inherits InputStream {
   -- The number of consecutive empty lines until a file is considered empty.
   emptyLineEofCount : Int <- 1024;
   setEmptyLineEofCount(count : Int) : Object { emptyLineEofCount <- count };

   io : IO <- new IO;
   -- True if at end of input.
   eof : Bool;
   -- The most recently read line.
   line : String;
   -- Position within the line.
   pos : Int <- 1;
   -- The number of buffered newlines.
   linefeeds : Int;

   readLinefeed() : String {{
      linefeeds <- linefeeds - 1;
      "\n";
   }};

   readFromLine() : String {
      let c : String <- line.substr(pos, 1) in
         {
            pos <- pos + 1;
            c;
         }
   };

   read() : String {
      if eof then
         ""
      else
         if not linefeeds = 0 then
            readLinefeed()
         else
            if pos < line.length() then
               readFromLine()
            else
               if pos = line.length() then
                  {
                     pos <- pos + 1;
                     "\n";
                  }
               else
                  {
                     pos <- 0;
                     let continue : Bool <- true in
                        while continue loop
                           {
                              line <- io.in_string();
                              if line = "" then
                                 {
                                    linefeeds <- linefeeds + 1;
                                    if not linefeeds < emptyLineEofCount then
                                       {
                                          eof <- true;
                                          continue <- false;
                                       }
                                    else false fi;
                                 }
                              else
                                 continue <- false
                              fi;
                           }
                        pool;

                     if eof then
                        ""
                     else
                        if not linefeeds = 0 then
                           readLinefeed()
                        else
                           readFromLine()
                        fi
                     fi;
                  }
               fi
            fi
         fi
      fi
   };
};
