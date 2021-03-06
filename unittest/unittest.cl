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
               .concat(" (length=").concat(stringUtil.fromInt(expected.length())).concat(")")
               .concat(", actual=").concat(actual)
               .concat(" (length=").concat(stringUtil.fromInt(actual.length())).concat(")"))
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

   newTestIO(context : String, in_ : Collection, out : Collection) : TestIO {
         new TestIO.init(self, context, in_, out)
   };

   newEmptyTestIO(context : String) : TestIO {
      let empty : Collection <- new Collection in
         newTestIO(context, empty, empty)
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
class TestIO inherits ExtendedIO {
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

   in_int() : Int {
      if inIter.next() then
         case inIter.get() of x : Int => x; esac
      else
         0
      fi
   };

   out_string(actual : String) : SELF_TYPE {{
      if not outIter.next() then
         test.failContext(context.concat(" out_string #").concat(new StringUtil.fromInt(outIndex)),
               "expected=void, actual=".concat(actual))
      else false fi;

      let expected : String <-
               case outIter.get() of
                  x : String => x;
                  expected : Int =>
                     {
                        test.failContext(context.concat(" out #").concat(new StringUtil.fromInt(outIndex)),
                              "expected=out_int:".concat(new StringUtil.fromInt(expected))
                              .concat(", actual=out_string:").concat(actual));
                        "";
                     };
               esac in
         if not actual = expected then
            test.assertStringEquals(context.concat(" out_string #").concat(new StringUtil.fromInt(outIndex)), expected, actual)
         else false fi;

      outIndex <- outIndex + 1;
      self;
   }};

   out_int(actual : Int) : SELF_TYPE {{
      if not outIter.next() then
         test.failContext(context.concat(" out_string #").concat(new StringUtil.fromInt(outIndex)),
               "expected=void, actual=".concat(new StringUtil.fromInt(actual)))
      else false fi;

      let expected : Int <-
               case outIter.get() of
                  x : Int => x;
                  expected : String =>
                     {
                        test.failContext(context.concat(" out #").concat(new StringUtil.fromInt(outIndex)),
                              "expected=out_string:".concat(expected)
                              .concat(", actual=out_int:").concat(new StringUtil.fromInt(actual)));
                        0;
                     };
               esac in
         if not actual = expected then
            test.assertIntEquals(context.concat(" out_int #").concat(new StringUtil.fromInt(outIndex)), expected, actual)
         else false fi;

      outIndex <- outIndex + 1;
      self;
   }};

   assert() : Object {
      test.assertFalse(context.concat(" out_string end"), outIter.next())
   };
};

class TestFailErrorParser inherits Parser {
   test : Test;
   context : String;

   initTest(test_ : Test, context_ : String) : SELF_TYPE {{
      test <- test_;
      context <- context_;
      self;
   }};

   reportError(line : Int, s : String) : Object {
      test.failContext(context.concat(" parse"), "ERROR: line ".concat(new StringUtil.fromInt(line)).concat(": ").concat(s))
   };
};

class TestFailErrorAnalyzer inherits Analyzer {
   test : Test;
   context : String;

   initTest(test_ : Test, context_ : String) : SELF_TYPE {{
      test <- test_;
      context <- context_;
      self;
   }};

   reportError(line : Int, s : String) : Object {
      test.failContext(context.concat(" analyze"), "ERROR: line ".concat(new StringUtil.fromInt(line)).concat(": ").concat(s))
   };
};
