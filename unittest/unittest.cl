class Test {
   stringUtil : StringUtil <- new StringUtil;
   test : String;
   single : String;
   num : Int;

   main() : Object {{
      single <- new IO.in_string();
      test();
      new IO.out_string("TEST PASSED: ")
            .out_int(num)
            .out_string("\n");
   }};

   test() : Object { false };

   begin(test_ : String) : Bool {
      if if single = "" then
            true
         else
            test_ = single
      fi then
         {
            test <- test_;
            num <- num + 1;
            true;
         }
      else
         false
      fi
   };

   fail(s : String) : Object {{
      new IO.out_string("TEST FAILED: ")
            .out_string(test)
            .out_string(": ")
            .out_string(s)
            .out_string("\n");
      -- Force runtime error.
      case new TestAssertionFailed of x : Bool => x; esac;
   }};

   failContext(context : String, s : String) : Object {
      fail(if context = "" then "" else context.concat(": ") fi.concat(s))
   };

   assertVoid(context : String, actual : Object) : Object {
      if not isvoid actual then
         failContext(context, "expected=void, actual=".concat(actual.type_name()))
      else false fi
   };

   assertNotVoid(context : String, actual : Object) : Object {
      if isvoid actual then
         failContext(context, "expected=not void, actual=void")
      else false fi
   };

   assertFalse(context : String, actual : Bool) : Object {
      if actual then
         failContext(context, "expected=false, actual=true")
      else false fi
   };

   assertTrue(context : String, actual : Bool) : Object {
      if not actual then
         failContext(context, "expected=true, actual=false")
      else false fi
   };

   assertBoolEquals(context : String, expected : Bool, actual : Bool) : Object {
      if not actual = expected then
         failContext(context, "expected=".concat(stringUtil.fromBool(expected))
               .concat(", actual=").concat(stringUtil.fromBool(actual)))
      else false fi
   };

   assertStringEquals(context : String, expected : String, actual : String) : Object {
      if not actual = expected then
         failContext(context, "expected=".concat(expected)
               .concat(", actual=").concat(actual))
      else false fi
   };

   assertIntEquals(context : String, expected : Int, actual : Int) : Object {
      if not actual = expected then
         failContext(context, "expected=".concat(stringUtil.fromInt(expected))
               .concat(", actual=").concat(stringUtil.fromInt(actual)))
      else false fi
   };

   assertLessThan(context : String, actual : Int, value : Int) : Object {
      if not actual < value then
         failContext(context, "expected actual=".concat(stringUtil.fromInt(actual))
               .concat(" < value=").concat(stringUtil.fromInt(value)))
      else false fi
   };
};

-- Eyecatcher for runtime error.
class TestAssertionFailed {
   x : Bool;
};

class TestStringInputStream inherits InputStream {
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

-- Custom IO instance that uses a LinkedList
class TestIO inherits IO {
   test : Test;
   context : String;
   inIter : Iterator;
   outIter : Iterator;
   outIndex : Int;

   init(test_ : Test, context_ : String, in_ : Collection, out : Collection) : SELF_TYPE {{
      test <- test_;
      context <- context_;
      inIter <- in_.iterator();
      outIter <- out.iterator();
      self;
   }};

   in_string() : String {
      if inIter.next() then
         case inIter.get() of x : String => x; esac
      else
         ""
      fi
   };

   out_string(actual : String) : SELF_TYPE {{
      if not outIter.next() then
         test.failContext(context.concat(" out_string #").concat(new StringUtil.fromInt(outIndex)),
               "expected=void, actual=".concat(actual))
      else false fi;

      let expected : String <- case outIter.get() of x : String => x; esac in
         if not actual = expected then
            test.assertStringEquals(context.concat(" out_string #").concat(new StringUtil.fromInt(outIndex)), expected, actual)
         else false fi;

      outIndex <- outIndex + 1;
      self;
   }};

   assert() : Object {
      test.assertFalse(context.concat(" end"), outIter.next())
   };
};

class TestIOInputStream inherits IOInputStream {
   init(io_ : IO) : SELF_TYPE {{
      -- Use a custom IO instance.
      io <- io_;
      self;
   }};
};
