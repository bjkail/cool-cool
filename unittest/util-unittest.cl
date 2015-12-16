class Main inherits Test {
   test() : Object {{
      test_IntUtil_minValue();
      test_IntUtil_mod();

      test_StringUtil_doubleQuote();
      test_StringUtil_backslash();
      test_StringUtil_fromBool();
      test_StringUtil_toDigit();
      test_StringUtil_fromDigit();
      test_StringUtil_fromInt();
   }};

   test_IntUtil_minValue() : Object {
      if begin("IntUtil.minValue") then
         let u : IntUtil <- new IntUtil in
            assertLessThan("", u.minValue(), ~2147483647)
      else false fi
   };

   test_IntUtil_mod() : Object {
      if begin("IntUtil.mod") then
         let u : IntUtil <- new IntUtil in
            {
               assertIntEquals("mod(0, 1)", 0, u.mod(0, 1));
               assertIntEquals("mod(0, -1)", 0, u.mod(0, ~1));

               assertIntEquals("mod(8, 1)", 0, u.mod(8, 1));
               assertIntEquals("mod(8, 2)", 0, u.mod(8, 2));
               assertIntEquals("mod(8, 3)", 2, u.mod(8, 3));
               assertIntEquals("mod(8, 4)", 0, u.mod(8, 4));
               assertIntEquals("mod(8, 5)", 3, u.mod(8, 5));
               assertIntEquals("mod(8, 6)", 2, u.mod(8, 6));
               assertIntEquals("mod(8, 7)", 1, u.mod(8, 7));
               assertIntEquals("mod(8, 8)", 0, u.mod(8, 8));

               assertIntEquals("mod(8, -1)", 0, u.mod(8, ~1));
               assertIntEquals("mod(8, -2)", 0, u.mod(8, ~2));
               assertIntEquals("mod(8, -3)", 2, u.mod(8, ~3));
               assertIntEquals("mod(8, -4)", 0, u.mod(8, ~4));
               assertIntEquals("mod(8, -5)", 3, u.mod(8, ~5));
               assertIntEquals("mod(8, -6)", 2, u.mod(8, ~6));
               assertIntEquals("mod(8, -7)", 1, u.mod(8, ~7));
               assertIntEquals("mod(8, -8)", 0, u.mod(8, ~8));

               assertIntEquals("mod(-8, 1)", 0, u.mod(~8, 1));
               assertIntEquals("mod(-8, 2)", 0, u.mod(~8, 2));
               assertIntEquals("mod(-8, 3)", ~2, u.mod(~8, 3));
               assertIntEquals("mod(-8, 4)", 0, u.mod(~8, 4));
               assertIntEquals("mod(-8, 5)", ~3, u.mod(~8, 5));
               assertIntEquals("mod(-8, 6)", ~2, u.mod(~8, 6));
               assertIntEquals("mod(-8, 7)", ~1, u.mod(~8, 7));
               assertIntEquals("mod(-8, 8)", 0, u.mod(~8, 8));

               assertIntEquals("mod(-8, -1)", 0, u.mod(~8, ~1));
               assertIntEquals("mod(-8, -2)", 0, u.mod(~8, ~2));
               assertIntEquals("mod(-8, -3)", ~2, u.mod(~8, ~3));
               assertIntEquals("mod(-8, -4)", 0, u.mod(~8, ~4));
               assertIntEquals("mod(-8, -5)", ~3, u.mod(~8, ~5));
               assertIntEquals("mod(-8, -6)", ~2, u.mod(~8, ~6));
               assertIntEquals("mod(-8, -7)", ~1, u.mod(~8, ~7));
               assertIntEquals("mod(-8, -8)", 0, u.mod(~8, ~8));
            }
      else false fi
   };

   test_StringUtil_doubleQuote() : Object {
      if begin("StringUtil.doubleQuote") then
         let u : StringUtil <- new StringUtil in
            assertStringEquals("doubleQuote", "\"".substr("\"".length() - 1, 1), u.doubleQuote())
      else false fi
   };

   test_StringUtil_backslash() : Object {
      if begin("StringUtil.backslash") then
         let u : StringUtil <- new StringUtil in
            assertStringEquals("backslash", "\\".substr("\\".length() - 1, 1), u.backslash())
      else false fi
   };

   test_StringUtil_fromBool() : Object {
      if begin("StringUtil.fromBool") then
         let u : StringUtil <- new StringUtil in
            {
               assertStringEquals("fromBool(false)", "false", u.fromBool(false));
               assertStringEquals("fromBool(true)", "true", u.fromBool(true));
            }
      else false fi
   };

   test_StringUtil_toDigit() : Object {
      if begin("StringUtil.toDigit") then
         let u : StringUtil <- new StringUtil in
            {
               assertIntEquals("toDigit()", ~1, u.toDigit(""));
               assertIntEquals("toDigit(-)", ~1, u.toDigit("-"));
               assertIntEquals("toDigit(-2)", ~1, u.toDigit("-2"));
               assertIntEquals("toDigit(-1)", ~1, u.toDigit("-1"));
               assertIntEquals("toDigit(0)", 0, u.toDigit("0"));
               assertIntEquals("toDigit(1)", 1, u.toDigit("1"));
               assertIntEquals("toDigit(2)", 2, u.toDigit("2"));
               assertIntEquals("toDigit(3)", 3, u.toDigit("3"));
               assertIntEquals("toDigit(4)", 4, u.toDigit("4"));
               assertIntEquals("toDigit(5)", 5, u.toDigit("5"));
               assertIntEquals("toDigit(6)", 6, u.toDigit("6"));
               assertIntEquals("toDigit(7)", 7, u.toDigit("7"));
               assertIntEquals("toDigit(8)", 8, u.toDigit("8"));
               assertIntEquals("toDigit(9)", 9, u.toDigit("9"));
               assertIntEquals("toDigit(10)", ~1, u.toDigit("10"));
            }
      else false fi
   };

   test_StringUtil_fromDigit() : Object {
      if begin("StringUtil.fromDigit") then
         let u : StringUtil <- new StringUtil in
            {
               assertStringEquals("fromDigit(-2)", "", u.fromDigit(~2));
               assertStringEquals("fromDigit(-1)", "", u.fromDigit(~1));
               assertStringEquals("fromDigit(0)", "0", u.fromDigit(0));
               assertStringEquals("fromDigit(1)", "1", u.fromDigit(1));
               assertStringEquals("fromDigit(2)", "2", u.fromDigit(2));
               assertStringEquals("fromDigit(3)", "3", u.fromDigit(3));
               assertStringEquals("fromDigit(4)", "4", u.fromDigit(4));
               assertStringEquals("fromDigit(5)", "5", u.fromDigit(5));
               assertStringEquals("fromDigit(6)", "6", u.fromDigit(6));
               assertStringEquals("fromDigit(7)", "7", u.fromDigit(7));
               assertStringEquals("fromDigit(8)", "8", u.fromDigit(8));
               assertStringEquals("fromDigit(9)", "9", u.fromDigit(9));
               assertStringEquals("fromDigit(10)", "", u.fromDigit(10));
            }
      else false fi
   };

   test_StringUtil_fromInt() : Object {
      if begin("StringUtil.fromInt") then
         let u : StringUtil <- new StringUtil in
            {
               let n : Int <- 0 in
                  while n < 10 loop
                     {
                        assertStringEquals("fromInt(".concat(u.fromDigit(n)).concat(")"), u.fromDigit(n), u.fromInt(n));
                        n <- n + 1;
                     }
                  pool;

               assertStringEquals("fromInt(-1)", "-1", u.fromInt(~1));
               assertStringEquals("fromInt(-2147483648)", "-2147483648", u.fromInt(new IntUtil.minValue()));
               assertStringEquals("fromInt(2147483647)", "2147483647", u.fromInt(2147483647));
            }
      else false fi
   };
};
