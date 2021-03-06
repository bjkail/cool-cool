class Main inherits Test {
   test() : Object {{
      testStack();
      testInstr();
   }};

   interpretImpl(context : String, error : String, program : CoolasmProgram, io : TestIO) : CoolasmInterpreter {
      let program : CoolasmInterpreterProgram <- new CoolasmInterpreterAnalyzer.analyze(program),
            interpreter : CoolasmInterpreter <- new CoolasmInterpreter.init(io) in
         {
            assertBoolEquals(context.concat(" error"), error = "", interpreter.interpret(program));
            assertStringEquals(context.concat(" error"), error, interpreter.error());
            io.assert();
            interpreter;
         }
   };

   interpretError(context : String, error : String, program : CoolasmProgram) : CoolasmInterpreter {
      interpretImpl(context, error, program, newEmptyTestIO(context))
   };

   interpretIO(context : String, program : CoolasmProgram, io : TestIO) : CoolasmInterpreter {
      interpretImpl(context, "", program, io)
   };

   interpret(context : String, program : CoolasmProgram) : CoolasmInterpreter {
      interpretIO(context, program, newEmptyTestIO(context))
   };

   getInstrsProgram(instrs : LinkedList) : CoolasmProgram {
      new CoolasmProgram.init(instrs.addFirst(new CoolasmLabel.init("start"))
            .add(new CoolasmSyscallInstr.init("exit")))
   };

   interpretInstrs(context : String, instrs : LinkedList) : CoolasmInterpreter {
      interpret(context, getInstrsProgram(instrs))
   };

   interpretInstrsError(context : String, error : String, instrs : LinkedList) : CoolasmInterpreter {
      interpretError(context, error, getInstrsProgram(instrs))
   };

   interpretInstr(context : String, instr : CoolasmInstr) : CoolasmInterpreter {
      interpretInstrs(context, new LinkedList.add(instr))
   };

   getIntReg(interpreter : CoolasmInterpreter, reg : CoolasmReg) : Int {
      interpreter.getIntReg(reg.value())
   };

   r0 : CoolasmReg <- new CoolasmReg.init(0);
   r1 : CoolasmReg <- new CoolasmReg.init(1);
   r2 : CoolasmReg <- new CoolasmReg.init(2);
   r3 : CoolasmReg <- new CoolasmReg.init(3);
   r4 : CoolasmReg <- new CoolasmReg.init(4);
   r5 : CoolasmReg <- new CoolasmReg.init(5);
   r6 : CoolasmReg <- new CoolasmReg.init(6);
   r7 : CoolasmReg <- new CoolasmReg.init(7);
   sp : CoolasmReg <- new CoolasmReg.init(8);
   fp : CoolasmReg <- new CoolasmReg.init(9);
   ra : CoolasmReg <- new CoolasmReg.init(10);

   li(dst : CoolasmReg, value : Int) : CoolasmLiInstr { new CoolasmLiInstr.init(dst, value) };
   mov(dst : CoolasmReg, src : CoolasmReg) : CoolasmMovInstr { new CoolasmMovInstr.init(dst, src) };
   add(dst : CoolasmReg, reg1 : CoolasmReg, reg2 : CoolasmReg) : CoolasmAddInstr { new CoolasmAddInstr.init(dst, reg1, reg2) };
   sub(dst : CoolasmReg, reg1 : CoolasmReg, reg2 : CoolasmReg) : CoolasmSubInstr { new CoolasmSubInstr.init(dst, reg1, reg2) };
   mul(dst : CoolasmReg, reg1 : CoolasmReg, reg2 : CoolasmReg) : CoolasmMulInstr { new CoolasmMulInstr.init(dst, reg1, reg2) };
   div(dst : CoolasmReg, reg1 : CoolasmReg, reg2 : CoolasmReg) : CoolasmDivInstr { new CoolasmDivInstr.init(dst, reg1, reg2) };
   jmp(label : CoolasmLabel) : CoolasmJmpInstr { new CoolasmJmpInstr.init(label) };
   bz(reg : CoolasmReg, label : CoolasmLabel) : CoolasmBzInstr { new CoolasmBzInstr.init(reg, label) };
   bnz(reg : CoolasmReg, label : CoolasmLabel) : CoolasmBnzInstr { new CoolasmBnzInstr.init(reg, label) };
   beq(reg1 : CoolasmReg, reg2 : CoolasmReg, label : CoolasmLabel) : CoolasmBeqInstr { new CoolasmBeqInstr.init(reg1, reg2, label) };
   blt(reg1 : CoolasmReg, reg2 : CoolasmReg, label : CoolasmLabel) : CoolasmBltInstr { new CoolasmBltInstr.init(reg1, reg2, label) };
   ble(reg1 : CoolasmReg, reg2 : CoolasmReg, label : CoolasmLabel) : CoolasmBleInstr { new CoolasmBleInstr.init(reg1, reg2, label) };
   callLabel(label : CoolasmLabel) : CoolasmCallLabelInstr { new CoolasmCallLabelInstr.init(label) };
   callReg(reg : CoolasmReg) : CoolasmCallRegInstr { new CoolasmCallRegInstr.init(reg) };
   return : CoolasmReturnInstr <- new CoolasmReturnInstr;
   push(reg : CoolasmReg) : CoolasmPushInstr { new CoolasmPushInstr.init(reg) };
   pop(reg : CoolasmReg) : CoolasmPopInstr { new CoolasmPopInstr.init(reg) };
   ld(dst : CoolasmReg, src : CoolasmReg, srcoff : Int) : CoolasmLdInstr { new CoolasmLdInstr.init(dst, src, srcoff) };
   st(dst : CoolasmReg, dstoff : Int, src : CoolasmReg) : CoolasmStInstr { new CoolasmStInstr.init(dst, dstoff, src) };
   la(dst : CoolasmReg, label : CoolasmLabel) : CoolasmLaInstr { new CoolasmLaInstr.init(dst, label) };
   alloc(dst : CoolasmReg, size : CoolasmReg) : CoolasmAllocInstr { new CoolasmAllocInstr.init(dst, size) };
   constantInteger(value : Int) : CoolasmConstantIntegerInstr { new CoolasmConstantIntegerInstr.init(value) };
   constantString(value : String) : CoolasmConstantStringInstr { new CoolasmConstantStringInstr.init(value) };
   constantLabel(label : CoolasmLabel) : CoolasmConstantLabelInstr { new CoolasmConstantLabelInstr.init(label) };
   syscall(s : String) : CoolasmSyscallInstr { new CoolasmSyscallInstr.init(s) };

   programAddress : Int <- 1000;
   stackAddress : Int <- 2000000000;
   allocAddress : Int <- 20000;

   testStack() : Object {
      if begin("stack") then
         {
            let interpreter : CoolasmInterpreter <- interpretInstrs("", new LinkedList) in
               {
                  assertIntEquals("sp", stackAddress, getIntReg(interpreter, sp));
                  assertIntEquals("fp", stackAddress, getIntReg(interpreter, fp));
               };
         }
      else false fi
   };

   testInstr() : Object {
      if begin("instr") then
         {
            let interpreter : CoolasmInterpreter <- interpretInstr("li", li(r0, 1)) in
               assertIntEquals("li", 1, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("mov", new LinkedList
                     .add(li(r1, 1))
                     .add(mov(r0, r1))) in
               assertIntEquals("mov", 1, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("add", new LinkedList
                     .add(li(r1, 1))
                     .add(li(r2, 2))
                     .add(add(r0, r1, r2))) in
               assertIntEquals("add", 3, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("sub", new LinkedList
                     .add(li(r1, 1))
                     .add(li(r2, 2))
                     .add(sub(r0, r1, r2))) in
               assertIntEquals("sub", ~1, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("mul", new LinkedList
                     .add(li(r1, 2))
                     .add(li(r2, 3))
                     .add(mul(r0, r1, r2))) in
               assertIntEquals("mul", 6, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("div", new LinkedList
                     .add(li(r1, 6))
                     .add(li(r2, 3))
                     .add(div(r0, r1, r2))) in
               assertIntEquals("div", 2, getIntReg(interpreter, r0));

            interpretInstrsError("div zero", "divide by 0", new LinkedList
                     .add(li(r1, 0))
                     .add(li(r2, 0))
                     .add(div(r0, r1, r2)));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("jmp", new LinkedList
                     .add(li(r0, 0))
                     .add(jmp(label))
                     .add(li(r0, 1))
                     .add(label)) in
               assertIntEquals("jmp", 0, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("bz false", new LinkedList
                     .add(li(r0, 1))
                     .add(bz(r0, label))
                     .add(li(r0, 0))
                     .add(label)) in
               assertIntEquals("bz false", 0, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("bz true", new LinkedList
                     .add(li(r0, 0))
                     .add(bz(r0, label))
                     .add(li(r0, 1))
                     .add(label)) in
               assertIntEquals("bz true", 0, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("bnz false", new LinkedList
                     .add(li(r0, 0))
                     .add(bnz(r0, label))
                     .add(li(r0, 1))
                     .add(label)) in
               assertIntEquals("bnz false", 1, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("bnz true", new LinkedList
                     .add(li(r0, 1))
                     .add(bnz(r0, label))
                     .add(li(r0, 0))
                     .add(label)) in
               assertIntEquals("bnz true", 1, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("beq false", new LinkedList
                     .add(li(r0, 0))
                     .add(li(r1, 1))
                     .add(beq(r0, r1, label))
                     .add(li(r0, 1))
                     .add(label)) in
               assertIntEquals("beq false", 1, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("beq true", new LinkedList
                     .add(li(r0, 0))
                     .add(beq(r0, r0, label))
                     .add(li(r0, 1))
                     .add(label)) in
               assertIntEquals("beq true", 0, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("blt false", new LinkedList
                     .add(li(r0, 0))
                     .add(blt(r0, r0, label))
                     .add(li(r0, 1))
                     .add(label)) in
               assertIntEquals("blt false", 1, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("blt true", new LinkedList
                     .add(li(r0, 0))
                     .add(li(r1, 1))
                     .add(blt(r0, r1, label))
                     .add(li(r0, 1))
                     .add(label)) in
               assertIntEquals("blt true", 0, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("ble false", new LinkedList
                     .add(li(r0, 1))
                     .add(li(r1, 0))
                     .add(ble(r0, r1, label))
                     .add(li(r0, 0))
                     .add(label)) in
               assertIntEquals("ble false", 0, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("ble true", new LinkedList
                     .add(li(r0, 0))
                     .add(ble(r0, r0, label))
                     .add(li(r0, 1))
                     .add(label)) in
               assertIntEquals("ble true", 0, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("call label", new LinkedList
                     .add(li(r0, 0))
                     .add(callLabel(label))
                     .add(li(r0, 1))
                     .add(label)) in
               {
                  assertIntEquals("call label r0", 0, getIntReg(interpreter, r0));
                  assertIntEquals("call label ra", programAddress + 2, getIntReg(interpreter, ra));
               };

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("call reg", new LinkedList
                     .add(li(r0, programAddress + 3))
                     .add(callReg(r0))
                     .add(li(r0, 0))) in
               {
                  assertIntEquals("call reg r0", programAddress + 3, getIntReg(interpreter, r0));
                  assertIntEquals("call reg ra", programAddress + 2, getIntReg(interpreter, ra));
               };

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("call ra", new LinkedList
                     .add(callReg(ra))
                     .add(li(r0, 0))) in
               {
                  assertIntEquals("call reg r0", 0, getIntReg(interpreter, r0));
                  assertIntEquals("call reg ra", programAddress + 1, getIntReg(interpreter, ra));
               };

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("return", new LinkedList
                     .add(li(r0, 0))
                     .add(li(ra, programAddress + 4))
                     .add(return)
                     .add(li(r0, 1))) in
               assertIntEquals("return", 0, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("push", new LinkedList
                     .add(li(r0, 1))
                     .add(push(r0))) in
               {
                  assertIntEquals("push memory", 1, case interpreter.getMemory(stackAddress) of x : Int => x; esac);
                  assertIntEquals("push sp", stackAddress - 1, getIntReg(interpreter, sp));
               };

            let interpreter : CoolasmInterpreter <- interpretInstrs("pop", new LinkedList
                     .add(li(r0, 1))
                     .add(push(r0))
                     .add(pop(r1))) in
               {
                  assertIntEquals("pop r1", 1, getIntReg(interpreter, r1));
                  assertIntEquals("pop sp", stackAddress, getIntReg(interpreter, sp));
               };

            let interpreter : CoolasmInterpreter <- interpretInstrs("ld", new LinkedList
                     .add(li(r0, 2))
                     .add(push(r0))
                     .add(ld(r0, sp, 1))) in
               assertIntEquals("ld", 2, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("st", new LinkedList
                     .add(li(r0, 2))
                     .add(st(sp, 1, r0))) in
               assertIntEquals("st", 2, case interpreter.getMemory(stackAddress + 1) of x : Int => x; esac);

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("la", new LinkedList
                     .add(label)
                     .add(la(r0, label))) in
               assertIntEquals("la", programAddress, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("alloc", new LinkedList
                     .add(li(r0, 1))
                     .add(alloc(r0, r0))
                     .add(li(r1, 9))
                     .add(alloc(r1, r1))
                     .add(li(r2, 10))
                     .add(alloc(r2, r2))
                     .add(li(r3, 1))
                     .add(alloc(r3, r3))) in
               {
                  assertIntEquals("alloc r0", allocAddress, getIntReg(interpreter, r0));
                  assertIntEquals("alloc r1", allocAddress + 10, getIntReg(interpreter, r1));
                  assertIntEquals("alloc r2", allocAddress + 20, getIntReg(interpreter, r2));
                  assertIntEquals("alloc r3", allocAddress + 40, getIntReg(interpreter, r3));
               };

            interpretInstrsError("alloc zero", "alloc of 0", new LinkedList
                     .add(li(r0, 0))
                     .add(alloc(r0, r0)));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("constant integer", new LinkedList
                     .add(la(r0, label))
                     .add(ld(r0, r0, 0))
                     .add(syscall("exit"))
                     .add(label)
                     .add(constantInteger(1))) in
               assertIntEquals("constant integer", 1, getIntReg(interpreter, r0));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("constant string", new LinkedList
                     .add(la(r0, label))
                     .add(ld(r0, r0, 0))
                     .add(syscall("exit"))
                     .add(label)
                     .add(constantString("a"))) in
               assertStringEquals("constant string", "a", case interpreter.getReg(r0.value()) of x : String => x; esac);

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  label2 : CoolasmLabel <- new CoolasmLabel.init("label2"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("constant label", new LinkedList
                     .add(label)
                     .add(la(r0, label2))
                     .add(ld(r0, r0, 0))
                     .add(syscall("exit"))
                     .add(label2)
                     .add(constantLabel(label))) in
               assertIntEquals("constant label", programAddress, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretIO("syscall IO.in_string",
                     getInstrsProgram(new LinkedList.add(syscall("IO.in_string"))),
                     newTestIO("syscall IO.in_string", new LinkedList.add("a"), new Collection)),
                     addr : Int <- getIntReg(interpreter, r1) in
               {
                  assertIntEquals("syscall IO.in_string r1", allocAddress, addr);
                  assertStringEquals("syscall IO.in_string string", "a", interpreter.getStringMemory(addr));
               };

            let interpreter : CoolasmInterpreter <- interpretIO("syscall IO.in_int",
                     getInstrsProgram(new LinkedList.add(syscall("IO.in_int"))),
                     newTestIO("syscall IO.in_int", new LinkedList.add(1), new Collection)),
                     addr : Int <- getIntReg(interpreter, r1) in
               assertIntEquals("syscall IO.in_int r1", 1, getIntReg(interpreter, r1));

            interpretIO("syscall IO.out_int",
                     getInstrsProgram(new LinkedList
                           .add(li(r1, 1))
                           .add(syscall("IO.out_int"))),
                     newTestIO("syscall IO.out_int", new Collection, new LinkedList.add(1)));

            let label : CoolasmLabel <- new CoolasmLabel.init("label") in
               interpretIO("syscall IO.out_string",
                        getInstrsProgram(new LinkedList
                              .add(la(r1, label))
                              .add(syscall("IO.out_string"))
                              .add(syscall("exit"))
                              .add(label)
                              .add(constantString("a"))),
                        newTestIO("syscall IO.out_string", new Collection, new LinkedList.add("a")));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("syscall String.length", new LinkedList
                     .add(la(r1, label))
                     .add(syscall("String.length"))
                     .add(syscall("exit"))
                     .add(label)
                     .add(constantString("a"))) in
               assertIntEquals("syscall String.length", 1, getIntReg(interpreter, r1));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  label2 : CoolasmLabel <- new CoolasmLabel.init("label2"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("syscall String.concat", new LinkedList
                     .add(la(r1, label))
                     .add(la(r2, label2))
                     .add(syscall("String.concat"))
                     .add(syscall("exit"))
                     .add(label)
                     .add(constantString("a"))
                     .add(label2)
                     .add(constantString("bc"))),
                  addr : Int <- getIntReg(interpreter, r1) in
               {
                  assertIntEquals("syscall String.concat", allocAddress, addr);
                  assertStringEquals("syscall String.concat", "abc", interpreter.getStringMemory(addr));
               };

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("syscall String.substr begin low", new LinkedList
                     .add(la(r0, label))
                     .add(li(r1, ~1))
                     .add(li(r2, 0))
                     .add(syscall("String.substr"))
                     .add(syscall("exit"))
                     .add(label)
                     .add(constantString(""))),
                  addr : Int <- getIntReg(interpreter, r1) in
               assertIntEquals("syscall String.substr begin low", 0, getIntReg(interpreter, r1));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("syscall String.substr begin high", new LinkedList
                     .add(la(r0, label))
                     .add(li(r1, 1))
                     .add(li(r2, 0))
                     .add(syscall("String.substr"))
                     .add(syscall("exit"))
                     .add(label)
                     .add(constantString(""))),
                  addr : Int <- getIntReg(interpreter, r1) in
               assertIntEquals("syscall String.substr begin high", 0, getIntReg(interpreter, r1));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("syscall String.substr length low", new LinkedList
                     .add(la(r0, label))
                     .add(li(r1, 0))
                     .add(li(r2, ~1))
                     .add(syscall("String.substr"))
                     .add(syscall("exit"))
                     .add(label)
                     .add(constantString(""))),
                  addr : Int <- getIntReg(interpreter, r1) in
               assertIntEquals("syscall String.substr length low", 0, getIntReg(interpreter, r1));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("syscall String.substr length high", new LinkedList
                     .add(la(r0, label))
                     .add(li(r1, 0))
                     .add(li(r2, 1))
                     .add(syscall("String.substr"))
                     .add(syscall("exit"))
                     .add(label)
                     .add(constantString(""))),
                  addr : Int <- getIntReg(interpreter, r1) in
               assertIntEquals("syscall String.substr length high", 0, getIntReg(interpreter, r1));

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("syscall String.substr", new LinkedList
                     .add(la(r0, label))
                     .add(li(r1, 1))
                     .add(li(r2, 2))
                     .add(syscall("String.substr"))
                     .add(syscall("exit"))
                     .add(label)
                     .add(constantString("abcd"))),
                  addr : Int <- getIntReg(interpreter, r1) in
               {
                  assertIntEquals("syscall String.substr address", allocAddress, getIntReg(interpreter, r1));
                  assertStringEquals("syscall String.substr string", "bc", interpreter.getStringMemory(addr));
               };
         }
      else false fi
   };
};
