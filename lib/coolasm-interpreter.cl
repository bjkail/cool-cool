class CoolasmInterpreterProgram {
   memory : IntMap;
   memory() : IntMap { memory };

   start : Int;
   start() : Int { start };

   init(memory_ : IntMap, start_ : Int) : SELF_TYPE {{
      memory <- memory_;
      start <- start_;
      self;
   }};
};

class CoolasmInterpreterInstr {
   interpret(interpreter : CoolasmInterpreter) : Object { new ObjectUtil.abortObject(self, "interpret: unimplemented") };
};

class CoolasmInterpreterLoadConstantInstr inherits CoolasmInterpreterInstr {
   reg : Int;
   value : Object;

   init(reg_ : Int, value_ : Object) : SELF_TYPE {{
      reg <- reg_;
      value <- value_;
      self;
   }};

   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.setReg(reg, value)
   };
};

class CoolasmInterpreterMovInstr inherits CoolasmInterpreterInstr {
   dst : Int;
   src : Int;

   init(dst_ : Int, src_ : Int) : SELF_TYPE {{
      dst <- dst_;
      src <- src_;
      self;
   }};

   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.setReg(dst, interpreter.getReg(src))
   };
};

class CoolasmInterpreterAbstractArithmeticInstr inherits CoolasmInterpreterInstr {
   dst : Int;
   src1 : Int;
   src2 : Int;

   init(dst_ : Int, src1_ : Int, src2_ : Int) : SELF_TYPE {{
      dst <- dst_;
      src1 <- src1_;
      src2 <- src2_;
      self;
   }};
};

class CoolasmInterpreterAddInstr inherits CoolasmInterpreterAbstractArithmeticInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.setReg(dst, interpreter.getIntReg(src1) + interpreter.getIntReg(src2))
   };
};

class CoolasmInterpreterSubInstr inherits CoolasmInterpreterAbstractArithmeticInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.setReg(dst, interpreter.getIntReg(src1) - interpreter.getIntReg(src2))
   };
};

class CoolasmInterpreterMulInstr inherits CoolasmInterpreterAbstractArithmeticInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.setReg(dst, interpreter.getIntReg(src1) * interpreter.getIntReg(src2))
   };
};

class CoolasmInterpreterDivInstr inherits CoolasmInterpreterAbstractArithmeticInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      let divisor : Int <- interpreter.getIntReg(src2) in
         if divisor = 0 then
            interpreter.exitError("divide by 0")
         else
            interpreter.setReg(dst, interpreter.getIntReg(src1) / divisor)
         fi
   };
};

class CoolasmInterpreterJmpInstr inherits CoolasmInterpreterInstr {
   pc : Int;

   init(pc_ : Int) : Object {{
      pc <- pc_;
      self;
   }};

   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.setPc(pc)
   };
};

class CoolasmInterpreterBzInstr inherits CoolasmInterpreterInstr {
   reg : Int;
   pc : Int;

   init(reg_ : Int, pc_ : Int) : Object {{
      reg <- reg_;
      pc <- pc_;
      self;
   }};

   interpret(interpreter : CoolasmInterpreter) : Object {
      if interpreter.getIntReg(reg) = 0 then
         interpreter.setPc(pc)
      else false fi
   };
};

class CoolasmInterpreterBnzInstr inherits CoolasmInterpreterInstr {
   reg : Int;
   pc : Int;

   init(reg_ : Int, pc_ : Int) : Object {{
      reg <- reg_;
      pc <- pc_;
      self;
   }};

   interpret(interpreter : CoolasmInterpreter) : Object {
      if not interpreter.getIntReg(reg) = 0 then
         interpreter.setPc(pc)
      else false fi
   };
};

class CoolasmInterpreterAbstractComparisonInstr inherits CoolasmInterpreterInstr {
   reg1 : Int;
   reg2 : Int;
   pc : Int;

   init(reg1_ : Int, reg2_ : Int, pc_ : Int) : Object {{
      pc <- pc_;
      reg1 <- reg1_;
      reg2 <- reg2_;
      self;
   }};
};

class CoolasmInterpreterBeqInstr inherits CoolasmInterpreterAbstractComparisonInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      if interpreter.getReg(reg1) = interpreter.getReg(reg2) then
         interpreter.setPc(pc)
      else false fi
   };
};

class CoolasmInterpreterCallLabelInstr inherits CoolasmInterpreterInstr {
   pc : Int;

   init(pc_ : Int) : Object {{
      pc <- pc_;
      self;
   }};

   interpret(interpreter : CoolasmInterpreter) : Object {{
      interpreter.setReg(10, interpreter.pc() + 1);
      interpreter.setPc(pc);
   }};
};

class CoolasmInterpreterAbstractRegInstr inherits CoolasmInterpreterInstr {
   reg : Int;

   init(reg_ : Int) : Object {{
      reg <- reg_;
      self;
   }};
};

class CoolasmInterpreterCallRegInstr inherits CoolasmInterpreterAbstractRegInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {{
      interpreter.setReg(10, interpreter.pc() + 1);
      interpreter.setPc(interpreter.getIntReg(reg) - 1);
   }};
};

class CoolasmInterpreterReturnInstr inherits CoolasmInterpreterInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {{
      interpreter.setPc(interpreter.getIntReg(10) - 1);
   }};
};

class CoolasmInterpreterPushInstr inherits CoolasmInterpreterAbstractRegInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      let sp : Int <- interpreter.getIntReg(8) in
         {
            interpreter.setMemory(sp, interpreter.getIntReg(reg));
            interpreter.setReg(8, sp - 1);
         }
   };
};

class CoolasmInterpreterPopInstr inherits CoolasmInterpreterAbstractRegInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      let sp : Int <- interpreter.getIntReg(8) + 1 in
         {
            interpreter.setReg(8, sp);
            interpreter.setReg(reg, interpreter.getMemory(sp));
         }
   };
};

class CoolasmInterpreterLdInstr inherits CoolasmInterpreterInstr {
   dst : Int;
   src : Int;
   srcoff : Int;

   init(dst_ : Int, src_ : Int, srcoff_ : Int) : SELF_TYPE {{
      dst <- dst_;
      src <- src_;
      srcoff <- srcoff_;
      self;
   }};

   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.setReg(dst, interpreter.getMemory(interpreter.getIntReg(src) + srcoff))
   };
};

class CoolasmInterpreterStInstr inherits CoolasmInterpreterInstr {
   dst : Int;
   dstoff : Int;
   src : Int;

   init(dst_ : Int, dstoff_ : Int, src_ : Int) : SELF_TYPE {{
      dst <- dst_;
      dstoff <- dstoff_;
      src <- src_;
      self;
   }};

   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.setMemory(interpreter.getIntReg(dst) + dstoff, interpreter.getReg(src))
   };
};

class CoolasmInterpreterAllocInstr inherits CoolasmInterpreterInstr {
   dst : Int;
   size : Int;

   init(dst_ : Int, size_ : Int) : SELF_TYPE {{
      dst <- dst_;
      size <- size_;
      self;
   }};

   interpret(interpreter : CoolasmInterpreter) : Object {
      let size : Int <- interpreter.getIntReg(size) in
         if size = 0 then
            interpreter.exitError("alloc of 0")
         else
            interpreter.setReg(dst, interpreter.alloc(size))
         fi
   };
};

class CoolasmInterpreterSyscallExitInstr inherits CoolasmInterpreterInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.exit()
   };
};

class CoolasmInterpreterSyscallIoInStringInstr inherits CoolasmInterpreterInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      let s : String <- interpreter.io().in_string(),
            addr : Int <- interpreter.alloc(1) in
         {
            interpreter.setMemory(addr, s);
            interpreter.setReg(1, addr);
         }
   };
};

class CoolasmInterpreterSyscallIoInIntInstr inherits CoolasmInterpreterInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.setReg(1, interpreter.io().in_int())
   };
};

class CoolasmInterpreterSyscallIoOutIntInstr inherits CoolasmInterpreterInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.io().out_int(interpreter.getIntReg(1))
   };
};

class CoolasmInterpreterLabel {
   pc : Int;
   pc() : Int { pc };

   init(pc_ : Int) : SELF_TYPE {{
      pc <- pc_;
      self;
   }};
};

class CoolasmInterpreterAnalyzer inherits CoolasmInstrVisitor {
   labels : StringMap <- new StringListMap;
   memory : IntMap <- new IntTreeMap;

   getLabel(label : CoolasmLabel) : CoolasmInterpreterLabel {
      case labels.getWithString(label.name()) of x : CoolasmInterpreterLabel => x; esac
   };

   analyze(program : CoolasmProgram) : CoolasmInterpreterProgram {{
      let iter : Iterator <- program.instrs().iterator(),
            pc : Int <- 1000 in
         while iter.next() loop
            case iter.get() of
               label : CoolasmLabel =>
                  let name : String <- label.name() in
                     labels.putWithString(name, new CoolasmInterpreterLabel.init(pc));

               instr : CoolasmInstr =>
                  pc <- pc + 1;
            esac
         pool;

      let iter : Iterator <- program.instrs().iterator(),
            pc : Int <- 1000 in
         while iter.next() loop
            case iter.get() of
               label : CoolasmLabel => false;

               instr : CoolasmInstr =>
                  {
                     memory.putWithInt(pc, instr.accept(self));
                     pc <- pc + 1;
                  };
            esac
         pool;

      let start : CoolasmInterpreterLabel <- case labels.getWithString("start") of x : CoolasmInterpreterLabel => x; esac in
         new CoolasmInterpreterProgram.init(memory, start.pc());
   }};

   visitLi(instr : CoolasmLiInstr) : Object {
      new CoolasmInterpreterLoadConstantInstr.init(instr.reg().value(), instr.value())
   };

   visitMov(instr : CoolasmMovInstr) : Object {
      new CoolasmInterpreterMovInstr.init(instr.dst().value(), instr.src().value())
   };

   visitAdd(instr : CoolasmAddInstr) : Object {
      new CoolasmInterpreterAddInstr.init(instr.dst().value(), instr.src1().value(), instr.src2().value())
   };

   visitSub(instr : CoolasmSubInstr) : Object {
      new CoolasmInterpreterSubInstr.init(instr.dst().value(), instr.src1().value(), instr.src2().value())
   };

   visitMul(instr : CoolasmMulInstr) : Object {
      new CoolasmInterpreterMulInstr.init(instr.dst().value(), instr.src1().value(), instr.src2().value())
   };

   visitDiv(instr : CoolasmDivInstr) : Object {
      new CoolasmInterpreterDivInstr.init(instr.dst().value(), instr.src1().value(), instr.src2().value())
   };

   visitJmp(instr : CoolasmJmpInstr) : Object {
      new CoolasmInterpreterJmpInstr.init(getLabel(instr.label()).pc() - 1)
   };

   visitBz(instr : CoolasmBzInstr) : Object {
      new CoolasmInterpreterBzInstr.init(instr.reg().value(), getLabel(instr.label()).pc() - 1)
   };

   visitBnz(instr : CoolasmBnzInstr) : Object {
      new CoolasmInterpreterBnzInstr.init(instr.reg().value(), getLabel(instr.label()).pc() - 1)
   };

   visitBeq(instr : CoolasmBeqInstr) : Object {
      new CoolasmInterpreterBeqInstr.init(instr.reg1().value(), instr.reg2().value(), getLabel(instr.label()).pc() - 1)
   };

   visitBlt(instr : CoolasmBltInstr) : Object {
      new CoolasmInterpreterBltInstr.init(instr.reg1().value(), instr.reg2().value(), getLabel(instr.label()).pc() - 1)
   };

   visitBle(instr : CoolasmBleInstr) : Object {
      new CoolasmInterpreterBleInstr.init(instr.reg1().value(), instr.reg2().value(), getLabel(instr.label()).pc() - 1)
   };

   visitCallLabel(instr : CoolasmCallLabelInstr) : Object {
      new CoolasmInterpreterCallLabelInstr.init(getLabel(instr.label()).pc() - 1)
   };

   visitCallReg(instr : CoolasmCallRegInstr) : Object {
      new CoolasmInterpreterCallRegInstr.init(instr.reg().value())
   };

   visitReturn(instr : CoolasmReturnInstr) : Object {
      new CoolasmInterpreterReturnInstr
   };

   visitPush(instr : CoolasmPushInstr) : Object {
      new CoolasmInterpreterPushInstr.init(instr.reg().value())
   };

   visitPop(instr : CoolasmPopInstr) : Object {
      new CoolasmInterpreterPopInstr.init(instr.reg().value())
   };

   visitLd(instr : CoolasmLdInstr) : Object {
      new CoolasmInterpreterLdInstr.init(instr.dst().value(), instr.src().value(), instr.srcoff())
   };

   visitSt(instr : CoolasmStInstr) : Object {
      new CoolasmInterpreterStInstr.init(instr.dst().value(), instr.dstoff(), instr.src().value())
   };

   visitLa(instr : CoolasmLaInstr) : Object {
      new CoolasmInterpreterLoadConstantInstr.init(instr.dst().value(), getLabel(instr.src()).pc())
   };

   visitAlloc(instr : CoolasmAllocInstr) : Object {
      new CoolasmInterpreterAllocInstr.init(instr.dst().value(), instr.size().value())
   };

   visitConstantInteger(instr : CoolasmConstantIntegerInstr) : Object {
      instr.value()
   };

   visitConstantString(instr : CoolasmConstantStringInstr) : Object {
      instr.value()
   };

   visitConstantLabel(instr : CoolasmConstantLabelInstr) : Object {
      getLabel(instr.label()).pc()
   };

   visitSyscall(instr : CoolasmSyscallInstr) : Object {
      let name : String <- instr.name() in
         if name = "IO.in_string" then
            new CoolasmInterpreterSyscallIoInStringInstr
         else
            if name = "IO.in_int" then
               new CoolasmInterpreterSyscallIoInIntInstr
            else
               if name = "IO.out_int" then
                  new CoolasmInterpreterSyscallIoOutIntInstr
               else
                  if name = "exit" then
                     new CoolasmInterpreterSyscallExitInstr
                  else new ObjectUtil.abortObject(self, "visitSyscall: ".concat(name)) fi
               fi
            fi
         fi
   };
};

class CoolasmInterpreter {
   io : IO;
   io() : IO { io };

   init(io_ : IO) : SELF_TYPE {{
      io <- io_;
      self;
   }};

   memory : IntMap <- new IntTreeMap;
   memory() : IntMap { memory };

   allocAddress : Int;

   regs : IntMap <- new IntTreeMap;

   pc : Int;
   pc() : Int { pc };
   setPc(pc_ : Int) : Object { pc <- pc_ };
   exit() : Object { pc <- ~1 };

   error : String;
   error() : String { error };
   exitError(s : String) : Object {{
      error <- s;
      exit();
   }};

   setReg(reg : Int, value : Object) : Object {
      regs.putWithInt(reg, value)
   };

   getReg(reg : Int) : Object {
      regs.getWithInt(reg)
   };

   getIntReg(reg : Int) : Int {
      case getReg(reg) of x : Int => x; esac
   };

   setMemory(addr : Int, value : Object) : Object {
      memory.putWithInt(addr, value)
   };

   getMemory(addr : Int) : Object {
      memory.getWithInt(addr)
   };

   getStringMemory(addr : Int) : String {
      case getMemory(addr) of x : String => x; esac
   };

   alloc(size : Int) : Int {
      let result : Int <- allocAddress in
         {
            size <- (size + 10) / 10 * 10;
            allocAddress <- allocAddress + size;
            result;
         }
   };

   getInstr(addr : Int) : CoolasmInterpreterInstr {
      case memory.getWithInt(addr) of x : CoolasmInterpreterInstr => x; esac
   };

   interpret(program : CoolasmInterpreterProgram) : Bool {{
      memory.putAll(program.memory());

      allocAddress <- 1000 + memory.size();
      if allocAddress < 20000 then
         allocAddress <- 20000
      else false fi;

      regs.putWithInt(8, 2000000000);
      regs.putWithInt(9, 2000000000);

      pc <- program.start();
      while 0 < pc loop
         let instr : CoolasmInterpreterInstr <- getInstr(pc) in
            {
               instr.interpret(self);
               pc <- pc + 1;
            }
      pool;

      error = "";
   }};
};
