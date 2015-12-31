class Main inherits Test {
   test() : Object {{
      testLabel();
      testComment();
      testReg();
      testInstr();
      testConstant();
   }};

   write(instr : Object) : String {
      let io : TestStringBufferIO <- new TestStringBufferIO in
         {
            new CoolasmWriter.init(io).write(new CoolasmProgram.init(new LinkedList.add(instr)));
            io.buffer();
         }
   };

   indent : String <- new CoolasmWriter.indent();

   assertInstrEquals(context : String, s : String, instr : CoolasmInstr) : Object {
      assertStringEquals(context, indent.concat(s).concat("\n"), write(instr))
   };

   testLabel() : Object {
      if begin("label") then
         {
            assertStringEquals("label", "a:\n", write(new CoolasmLabel.init("a")));
            assertStringEquals("label", "; c\na:\n", write(new CoolasmLabel.init("a").setComment("c")));
         }
      else false fi
   };

   r0 : CoolasmReg <- new CoolasmReg.init(0);
   r1 : CoolasmReg <- new CoolasmReg.init(1);
   r2 : CoolasmReg <- new CoolasmReg.init(2);
   r3 : CoolasmReg <- new CoolasmReg.init(3);

   testComment() : Object {
      if begin("comment") then
         {
            assertStringEquals("label", "        li r0 <- 1              ; c\n",
                  write(new CoolasmLiInstr.init(r0, 1).setComment("c")));
            assertStringEquals("label", "        add r0 <- r1 r2         ; c\n",
                  write(new CoolasmAddInstr.init(r0, r1, r2).setComment("c")));
            assertStringEquals("label", "        jmp 012345678901234567  ; c\n",
                  write(new CoolasmJmpInstr.init(new CoolasmLabel.init("012345678901234567")).setComment("c")));
            assertStringEquals("label", "        jmp 0123456789012345678 ; c\n",
                  write(new CoolasmJmpInstr.init(new CoolasmLabel.init("0123456789012345678")).setComment("c")));
            assertStringEquals("label", "        jmp 01234567890123456789 ; c\n",
                  write(new CoolasmJmpInstr.init(new CoolasmLabel.init("01234567890123456789")).setComment("c")));
            assertStringEquals("label", "        jmp 012345678901234567890 ; c\n",
                  write(new CoolasmJmpInstr.init(new CoolasmLabel.init("012345678901234567890")).setComment("c")));
         }
      else false fi
   };

   testReg() : Object {
      if begin("reg") then
         {
            assertInstrEquals("r0", "li r0 <- 1", new CoolasmLiInstr.init(r0, 1));
            assertInstrEquals("r1", "li r1 <- 1", new CoolasmLiInstr.init(r1, 1));
            assertInstrEquals("r2", "li r2 <- 1", new CoolasmLiInstr.init(r2, 1));
            assertInstrEquals("r3", "li r3 <- 1", new CoolasmLiInstr.init(r3, 1));
            assertInstrEquals("r4", "li r4 <- 1", new CoolasmLiInstr.init(new CoolasmReg.init(4), 1));
            assertInstrEquals("r5", "li r5 <- 1", new CoolasmLiInstr.init(new CoolasmReg.init(5), 1));
            assertInstrEquals("r6", "li r6 <- 1", new CoolasmLiInstr.init(new CoolasmReg.init(6), 1));
            assertInstrEquals("r7", "li r7 <- 1", new CoolasmLiInstr.init(new CoolasmReg.init(7), 1));
            assertInstrEquals("sp", "li sp <- 1", new CoolasmLiInstr.init(new CoolasmReg.init(8), 1));
            assertInstrEquals("fp", "li fp <- 1", new CoolasmLiInstr.init(new CoolasmReg.init(9), 1));
            assertInstrEquals("ra", "li ra <- 1", new CoolasmLiInstr.init(new CoolasmReg.init(10), 1));
         }
      else false fi
   };

   testInstr() : Object {
      if begin("instr") then
         {
            assertInstrEquals("li", "li r1 <- 1", new CoolasmLiInstr.init(r1, 1));
            assertInstrEquals("mov", "mov r1 <- r2", new CoolasmMovInstr.init(r1, r2));
            assertInstrEquals("add", "add r1 <- r2 r3", new CoolasmAddInstr.init(r1, r2, r3));
            assertInstrEquals("sub", "sub r1 <- r2 r3", new CoolasmSubInstr.init(r1, r2, r3));
            assertInstrEquals("mul", "mul r1 <- r2 r3", new CoolasmMulInstr.init(r1, r2, r3));
            assertInstrEquals("div", "div r1 <- r2 r3", new CoolasmDivInstr.init(r1, r2, r3));
            assertInstrEquals("jmp", "jmp a", new CoolasmJmpInstr.init(new CoolasmLabel.init("a")));
            assertInstrEquals("bz", "bz r1 a", new CoolasmBzInstr.init(r1, new CoolasmLabel.init("a")));
            assertInstrEquals("bnz", "bnz r1 a", new CoolasmBnzInstr.init(r1, new CoolasmLabel.init("a")));
            assertInstrEquals("beq", "beq r1 r2 a", new CoolasmBeqInstr.init(r1, r2, new CoolasmLabel.init("a")));
            assertInstrEquals("blt", "blt r1 r2 a", new CoolasmBltInstr.init(r1, r2, new CoolasmLabel.init("a")));
            assertInstrEquals("ble", "ble r1 r2 a", new CoolasmBleInstr.init(r1, r2, new CoolasmLabel.init("a")));
            assertInstrEquals("call label", "call a", new CoolasmCallLabelInstr.init(new CoolasmLabel.init("a")));
            assertInstrEquals("call reg", "call r1", new CoolasmCallRegInstr.init(r1));
            assertInstrEquals("return", "return", new CoolasmReturnInstr);
            assertInstrEquals("push", "push r1", new CoolasmPushInstr.init(r1));
            assertInstrEquals("pop", "pop r1", new CoolasmPopInstr.init(r1));
            assertInstrEquals("ld", "ld r1 <- r2[3]", new CoolasmLdInstr.init(r1, r2, 3));
            assertInstrEquals("st", "st r1[2] <- r3", new CoolasmStInstr.init(r1, 2, r3));
            assertInstrEquals("la", "la r1 <- a", new CoolasmLaInstr.init(r1, new CoolasmLabel.init("a")));
            assertInstrEquals("alloc", "alloc r1 r2", new CoolasmAllocInstr.init(r1, r2));
            assertInstrEquals("syscall", "syscall a", new CoolasmSyscallInstr.init("a"));
         }
      else false fi
   };

   testConstant() : Object {
      if begin("constant") then
         {
            assertInstrEquals("0", "constant 0", new CoolasmConstantIntegerInstr.init(0));
            assertInstrEquals("1", "constant 1", new CoolasmConstantIntegerInstr.init(1));
            assertInstrEquals("max", "constant 2147483647", new CoolasmConstantIntegerInstr.init(2147483647));
            assertInstrEquals("-1", "constant -1", new CoolasmConstantIntegerInstr.init(~1));
            assertInstrEquals("min", "constant -2147483648", new CoolasmConstantIntegerInstr.init(~2147483647 - 1));

            assertInstrEquals("empty",
                  "constant ".concat(stringUtil.doubleQuote()).concat(stringUtil.doubleQuote()),
                  new CoolasmConstantStringInstr.init(""));
            assertInstrEquals("single",
                  "constant ".concat(stringUtil.doubleQuote()).concat("a").concat(stringUtil.doubleQuote()),
                  new CoolasmConstantStringInstr.init("a"));
            assertInstrEquals("linefeed",
                  "constant ".concat(stringUtil.doubleQuote()).concat("\\n").concat(stringUtil.doubleQuote()),
                  new CoolasmConstantStringInstr.init("\n"));
            assertInstrEquals("tab",
                  "constant ".concat(stringUtil.doubleQuote()).concat("\\t").concat(stringUtil.doubleQuote()),
                  new CoolasmConstantStringInstr.init("\t"));
         }
      else false fi
   };
};

class TestStringBufferIO inherits IO {
   stringUtil : StringUtil <- new StringUtil;

   buffer : String;
   buffer() : String { buffer };

   out_string(s : String) : SELF_TYPE {{
      buffer <- buffer.concat(s);
      self;
   }};

   out_int(i : Int) : SELF_TYPE {{
      buffer <- buffer.concat(stringUtil.fromInt(i));
      self;
   }};
};
