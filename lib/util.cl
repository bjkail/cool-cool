class IntUtil {
   minValue : Int <- 0-2147483647-1;

   minValue() : Int {
      minValue
   };

   mod(a : Int, n : Int) : Int {
      a - (n * (a / n))
   };
};

class StringUtil {
   intUtil : IntUtil <- new IntUtil;

   -- Workaround for interpreter bug that causes blackslashed characters in
   -- strings to include literal backslashes.
   initEscapedChar(s : String) : String {
      if s.length() = 1 then
         s
      else
         s.substr(1, 1)
      fi
   };

   doubleQuote : String <- initEscapedChar("\"");
   doubleQuote() : String { doubleQuote };

   backslash : String <- initEscapedChar("\\");
   backslash() : String { backslash };

   fromBool(b : Bool) : String {
      if b then
         "true"
      else
         "false"
      fi
   };

   toDigit(s : String) : Int {
      if s = "0" then 0 else
      if s = "1" then 1 else
      if s = "2" then 2 else
      if s = "3" then 3 else
      if s = "4" then 4 else
      if s = "5" then 5 else
      if s = "6" then 6 else
      if s = "7" then 7 else
      if s = "8" then 8 else
      if s = "9" then 9 else
         0-1
      fi fi fi fi fi
      fi fi fi fi fi
   };

   fromDigit(i : Int) : String {
      if i = 0 then "0" else
      if i = 1 then "1" else
      if i = 2 then "2" else
      if i = 3 then "3" else
      if i = 4 then "4" else
      if i = 5 then "5" else
      if i = 6 then "6" else
      if i = 7 then "7" else
      if i = 8 then "8" else
      if i = 9 then "9" else
         ""
      fi fi fi fi fi
      fi fi fi fi fi
   };

   fromInt(i : Int) : String {
      if i = 0 then
         "0"
      else
         if i = intUtil.minValue() then
            "-2147483648"
         else
            let s : String,
                  neg : Bool <- i < 0 in
               {
                  if neg then
                     i <- 0 - i
                  else false fi;

                  while not i = 0 loop
                     {
                        s <- fromDigit(intUtil.mod(i, 10)).concat(s);
                        i <- i / 10;
                     }
                  pool;

                  if neg then
                     "-".concat(s)
                  else
                     s
                  fi;
               }
         fi
      fi
   };
};
