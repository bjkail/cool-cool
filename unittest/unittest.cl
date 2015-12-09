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
            num <- num + 1;
            true;
         }
      else
         false
      fi
   };

   fail(s : String) : Object {{
      new IO.out_string("TEST FAILED: ")
            .out_string(s)
            .out_string("\n");
      -- Force runtime error.
      case new TestAssertionFailed of x : Bool => x; esac;
   }};

   failContext(context : String, s : String) : Object {
      fail(if context = "" then "" else context.concat(": ") fi.concat(s))
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
