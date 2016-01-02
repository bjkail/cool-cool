class Main inherits Test {
   test() : Object {{
      testStack();
      testInstr();
   }};

   interpretError(context : String, error : String, program : CoolasmProgram) : CoolasmInterpreter {
      let program : CoolasmInterpreterProgram <- new CoolasmInterpreterAnalyzer.analyze(program),
            interpreter : CoolasmInterpreter <- new CoolasmInterpreter in
         {
            assertBoolEquals(context.concat(" error"), error = "", interpreter.interpret(program));
            assertStringEquals(context.concat(" error"), error, interpreter.error());
            interpreter;
         }
   };

   interpret(context : String, program : CoolasmProgram) : CoolasmInterpreter {
      interpretError(context, "", program)
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

   testStack() : Object {
      if begin("stack") then
         {
            let interpreter : CoolasmInterpreter <- interpretInstrs("", new LinkedList) in
               {
                  assertIntEquals("sp", 2000000000, getIntReg(interpreter, sp));
                  assertIntEquals("fp", 2000000000, getIntReg(interpreter, fp));
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
                  assertIntEquals("call label ra", 1002, getIntReg(interpreter, ra));
               };

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("call reg", new LinkedList
                     .add(li(r0, 1003))
                     .add(callReg(r0))
                     .add(li(r0, 0))) in
               {
                  assertIntEquals("call reg r0", 1003, getIntReg(interpreter, r0));
                  assertIntEquals("call reg ra", 1002, getIntReg(interpreter, ra));
               };

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("call ra", new LinkedList
                     .add(callReg(ra))
                     .add(li(r0, 0))) in
               {
                  assertIntEquals("call reg r0", 0, getIntReg(interpreter, r0));
                  assertIntEquals("call reg ra", 1001, getIntReg(interpreter, ra));
               };

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("return", new LinkedList
                     .add(li(r0, 0))
                     .add(li(ra, 1004))
                     .add(return)
                     .add(li(r0, 1))) in
               assertIntEquals("return", 0, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("push", new LinkedList
                     .add(li(r0, 1))
                     .add(push(r0))) in
               {
                  assertIntEquals("push memory", 1, case interpreter.getMemory(2000000000) of x : Int => x; esac);
                  assertIntEquals("push sp", 1999999999, getIntReg(interpreter, sp));
               };

            let interpreter : CoolasmInterpreter <- interpretInstrs("pop", new LinkedList
                     .add(li(r0, 1))
                     .add(push(r0))
                     .add(pop(r1))) in
               {
                  assertIntEquals("pop r1", 1, getIntReg(interpreter, r1));
                  assertIntEquals("pop sp", 2000000000, getIntReg(interpreter, sp));
               };

            let interpreter : CoolasmInterpreter <- interpretInstrs("ld", new LinkedList
                     .add(li(r0, 2))
                     .add(push(r0))
                     .add(ld(r0, sp, 1))) in
               assertIntEquals("ld", 2, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("st", new LinkedList
                     .add(li(r0, 2))
                     .add(st(sp, 1, r0))) in
               assertIntEquals("st", 2, case interpreter.getMemory(2000000001) of x : Int => x; esac);

            let label : CoolasmLabel <- new CoolasmLabel.init("label"),
                  interpreter : CoolasmInterpreter <- interpretInstrs("la", new LinkedList
                     .add(label)
                     .add(la(r0, label))) in
               assertIntEquals("la", 1000, getIntReg(interpreter, r0));

            let interpreter : CoolasmInterpreter <- interpretInstrs("alloc", new LinkedList
                     .add(li(r0, 3))
                     .add(alloc(r1, r0))
                     .add(alloc(r2, r1))) in
               {
                  assertIntEquals("alloc r1", 20000, getIntReg(interpreter, r1));
                  assertIntEquals("alloc r2", 20003, getIntReg(interpreter, r2));
               };

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
               assertIntEquals("constant label", 1000, getIntReg(interpreter, r0));
         }
      else false fi
   };
};
