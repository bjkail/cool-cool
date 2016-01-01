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
      interpreter.getReg(reg).setValue(value)
   };
};

class CoolasmInterpreterSyscallExitInstr inherits CoolasmInterpreterInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      interpreter.exit()
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

   getLabel(name : String) : CoolasmInterpreterLabel {
      case labels.getWithString(name) of x : CoolasmInterpreterLabel => x; esac
   };

   analyze(program : CoolasmProgram) : CoolasmInterpreterProgram {{
      let iter : Iterator <- program.instrs().iterator(),
            pc : Int <- 1000 in
         while iter.next() loop
            case iter.get() of
               label : CoolasmLabel =>
                  let name : String <- label.name(),
                        label : Object <- labels.getWithString(name) in
                     if isvoid label then
                        labels.putWithString(name, new CoolasmInterpreterLabel.init(pc))
                     else
                        case label of x : CoolasmInterpreterLabel => x.init(pc); esac
                     fi;

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

   visitSyscall(instr : CoolasmSyscallInstr) : Object {
      let name : String <- instr.name() in
         if name = "exit" then
            new CoolasmInterpreterSyscallExitInstr
         else new ObjectUtil.abortObject(self, "visitSyscall: ".concat(name)) fi
   };
};

class CoolasmInterpreterReg {
   value : Object;
   value() : Object { value };
   setValue(value_ : Object) : Object { value <- value_ };
};

class CoolasmInterpreter {
   memory : IntMap <- new IntTreeMap;
   memory() : IntMap { memory };

   regs : IntMap <- new IntTreeMap;
   regs() : IntMap { regs };

   pc : Int;
   setPc(pc_ : Int) : Object { pc <- pc_ };
   exit() : Object { pc <- ~1 };

   getReg(reg : Int) : CoolasmInterpreterReg {
      case regs.getWithInt(reg) of x : CoolasmInterpreterReg => x; esac
   };

   getMemory(addr : Int) : Object {
      memory.getWithInt(addr)
   };

   getInstr(addr : Int) : CoolasmInterpreterInstr {
      case memory.getWithInt(addr) of x : CoolasmInterpreterInstr => x; esac
   };

   interpret(program : CoolasmInterpreterProgram) : Object {{
      memory.putAll(program.memory());

      let i : Int in
         while i < 10 loop
            {
               regs.putWithInt(i, new CoolasmInterpreterReg);
               i <- i + 1;
            }
         pool;

      pc <- program.start();
      while 0 < pc loop
         let instr : CoolasmInterpreterInstr <- getInstr(pc) in
            {
               instr.interpret(self);
               pc <- pc + 1;
            }
      pool;
   }};
};
