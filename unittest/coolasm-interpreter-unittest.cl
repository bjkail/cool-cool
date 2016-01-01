class Main inherits Test {
   test() : Object {{
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
         }
      else false fi
   };
};
