class CoolasmProgram {
   instrs : Collection;
   instrs() : Collection { instrs };

   init(instrs_ : Collection) : SELF_TYPE {{
      instrs <- instrs_;
      self;
   }};
};

class CoolasmReg {
   value : Int;
   value() : Int { value };

   init(value_ : Int) : SELF_TYPE {{
      value <- value_;
      self;
   }};
};

class CoolasmLabel {
   comment : String;
   comment() : String { comment };

   setComment(comment_ : String) : SELF_TYPE {{
      comment <- comment_;
      self;
   }};

   name : String;
   name() : String { if name = "" then new ObjectUtil.abortString(self, "name: invalid") else name fi };

   init(name_ : String) : SELF_TYPE {{
      name <- name_;
      self;
   }};
};

class CoolasmInstrVisitor {
   visitLi(instr : CoolasmLiInstr) : Object { new ObjectUtil.abortObject(self, "visitLi: unimplemented") };
   visitMov(instr : CoolasmMovInstr) : Object { new ObjectUtil.abortObject(self, "visitMov: unimplemented") };
   visitAdd(instr : CoolasmAddInstr) : Object { new ObjectUtil.abortObject(self, "visitAdd: unimplemented") };
   visitSub(instr : CoolasmSubInstr) : Object { new ObjectUtil.abortObject(self, "visitSub: unimplemented") };
   visitMul(instr : CoolasmMulInstr) : Object { new ObjectUtil.abortObject(self, "visitMul: unimplemented") };
   visitDiv(instr : CoolasmDivInstr) : Object { new ObjectUtil.abortObject(self, "visitDiv: unimplemented") };
   visitJmp(instr : CoolasmJmpInstr) : Object { new ObjectUtil.abortObject(self, "visitJmp: unimplemented") };
   visitBz(instr : CoolasmBzInstr) : Object { new ObjectUtil.abortObject(self, "visitBz: unimplemented") };
   visitBnz(instr : CoolasmBnzInstr) : Object { new ObjectUtil.abortObject(self, "visitBnz: unimplemented") };
   visitBeq(instr : CoolasmBeqInstr) : Object { new ObjectUtil.abortObject(self, "visitBeq: unimplemented") };
   visitBlt(instr : CoolasmBltInstr) : Object { new ObjectUtil.abortObject(self, "visitBlt: unimplemented") };
   visitBle(instr : CoolasmBleInstr) : Object { new ObjectUtil.abortObject(self, "visitBle: unimplemented") };
   visitCallLabel(instr : CoolasmCallLabelInstr) : Object { new ObjectUtil.abortObject(self, "visitCallLabel: unimplemented") };
   visitCallReg(instr : CoolasmCallRegInstr) : Object { new ObjectUtil.abortObject(self, "visitCallReg: unimplemented") };
   visitReturn(instr : CoolasmReturnInstr) : Object { new ObjectUtil.abortObject(self, "visitReturn: unimplemented") };
   visitPush(instr : CoolasmPushInstr) : Object { new ObjectUtil.abortObject(self, "visitPush: unimplemented") };
   visitPop(instr : CoolasmPopInstr) : Object { new ObjectUtil.abortObject(self, "visitPop: unimplemented") };
   visitLd(instr : CoolasmLdInstr) : Object { new ObjectUtil.abortObject(self, "visitLd: unimplemented") };
   visitSt(instr : CoolasmStInstr) : Object { new ObjectUtil.abortObject(self, "visitSt: unimplemented") };
   visitLa(instr : CoolasmLaInstr) : Object { new ObjectUtil.abortObject(self, "visitLa: unimplemented") };
   visitAlloc(instr : CoolasmAllocInstr) : Object { new ObjectUtil.abortObject(self, "visitAlloc: unimplemented") };
   visitConstantInteger(instr : CoolasmConstantIntegerInstr) : Object { new ObjectUtil.abortObject(self, "visitConstantInteger: unimplemented") };
   visitConstantString(instr : CoolasmConstantStringInstr) : Object { new ObjectUtil.abortObject(self, "visitConstantString: unimplemented") };
   visitConstantLabel(instr : CoolasmConstantLabelInstr) : Object { new ObjectUtil.abortObject(self, "visitConstantString: unimplemented") };
   visitSyscall(instr : CoolasmSyscallInstr) : Object { new ObjectUtil.abortObject(self, "visitSyscall: unimplemented") };
};

class CoolasmInstr {
   comment : String;
   comment() : String { comment };

   setComment(comment_ : String) : SELF_TYPE {{
      comment <- comment_;
      self;
   }};

   accept(visitor : CoolasmInstrVisitor) : Object { new ObjectUtil.abortObject(self, "accept: unimplemented") };
};

class CoolasmLiInstr inherits CoolasmInstr {
   reg : CoolasmReg;
   reg() : CoolasmReg { reg };

   value : Int;
   value() : Int { value };

   init(reg_ : CoolasmReg, value_ : Int) : SELF_TYPE {{
      reg <- reg_;
      value <- value_;
      self;
   }};

   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitLi(self) };
};

class CoolasmMovInstr inherits CoolasmInstr {
   dst : CoolasmReg;
   dst() : CoolasmReg { dst };

   src : CoolasmReg;
   src() : CoolasmReg { src };

   init(dst_ : CoolasmReg, src_ : CoolasmReg) : SELF_TYPE {{
      dst <- dst_;
      src <- src_;
      self;
   }};

   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitMov(self) };
};

class CoolasmAbstractArithmeticInstr inherits CoolasmInstr {
   dst : CoolasmReg;
   dst() : CoolasmReg { dst };

   src1 : CoolasmReg;
   src1() : CoolasmReg { src1 };

   src2 : CoolasmReg;
   src2() : CoolasmReg { src2 };

   init(dst_ : CoolasmReg, src1_ : CoolasmReg, src2_ : CoolasmReg) : SELF_TYPE {{
      dst <- dst_;
      src1 <- src1_;
      src2 <- src2_;
      self;
   }};
};

class CoolasmAddInstr inherits CoolasmAbstractArithmeticInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitAdd(self) };
};

class CoolasmSubInstr inherits CoolasmAbstractArithmeticInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitSub(self) };
};

class CoolasmMulInstr inherits CoolasmAbstractArithmeticInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitMul(self) };
};

class CoolasmDivInstr inherits CoolasmAbstractArithmeticInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitDiv(self) };
};

class CoolasmAbstractRegInstr inherits CoolasmInstr {
   reg : CoolasmReg;
   reg() : CoolasmReg { reg };

   init(reg_ : CoolasmReg) : SELF_TYPE {{
      reg <- reg_;
      self;
   }};
};

class CoolasmAbstractLabelInstr inherits CoolasmInstr {
   label : CoolasmLabel;
   label() : CoolasmLabel { label };

   init(label_ : CoolasmLabel) : SELF_TYPE {{
      label <- label_;
      self;
   }};
};

class CoolasmAbstractRegLabelInstr inherits CoolasmInstr {
   reg : CoolasmReg;
   reg() : CoolasmReg { reg };

   label : CoolasmLabel;
   label() : CoolasmLabel { label };

   init(reg_ : CoolasmReg, label_ : CoolasmLabel) : SELF_TYPE {{
      reg <- reg_;
      label <- label_;
      self;
   }};
};

class CoolasmAbstractRegRegLabelInstr inherits CoolasmInstr {
   reg1 : CoolasmReg;
   reg1() : CoolasmReg { reg1 };

   reg2 : CoolasmReg;
   reg2() : CoolasmReg { reg2 };

   label : CoolasmLabel;
   label() : CoolasmLabel { label };

   init(reg1_ : CoolasmReg, reg2_ : CoolasmReg, label_ : CoolasmLabel) : SELF_TYPE {{
      reg1 <- reg1_;
      reg2 <- reg2_;
      label <- label_;
      self;
   }};
};

class CoolasmJmpInstr inherits CoolasmAbstractLabelInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitJmp(self) };
};

class CoolasmBzInstr inherits CoolasmAbstractRegLabelInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitBz(self) };
};

class CoolasmBnzInstr inherits CoolasmAbstractRegLabelInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitBnz(self) };
};

class CoolasmBeqInstr inherits CoolasmAbstractRegRegLabelInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitBeq(self) };
};

class CoolasmBltInstr inherits CoolasmAbstractRegRegLabelInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitBlt(self) };
};

class CoolasmBleInstr inherits CoolasmAbstractRegRegLabelInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitBle(self) };
};

class CoolasmCallLabelInstr inherits CoolasmAbstractLabelInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitCallLabel(self) };
};

class CoolasmCallRegInstr inherits CoolasmAbstractRegInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitCallReg(self) };
};

class CoolasmReturnInstr inherits CoolasmInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitReturn(self) };
};

class CoolasmPushInstr inherits CoolasmAbstractRegInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitPush(self) };
};

class CoolasmPopInstr inherits CoolasmAbstractRegInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitPop(self) };
};

class CoolasmLdInstr inherits CoolasmInstr {
   dst : CoolasmReg;
   dst() : CoolasmReg { dst };

   src : CoolasmReg;
   src() : CoolasmReg { src };

   srcoff : Int;
   srcoff() : Int { srcoff };

   init(dst_ : CoolasmReg, src_ : CoolasmReg, srcoff_ : Int) : SELF_TYPE {{
      dst <- dst_;
      src <- src_;
      srcoff <- srcoff_;
      self;
   }};

   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitLd(self) };
};

class CoolasmStInstr inherits CoolasmInstr {
   dst : CoolasmReg;
   dst() : CoolasmReg { dst };

   dstoff : Int;
   dstoff() : Int { dstoff };

   src : CoolasmReg;
   src() : CoolasmReg { src };

   init(dst_ : CoolasmReg, dstoff_ : Int, src_ : CoolasmReg) : SELF_TYPE {{
      dst <- dst_;
      dstoff <- dstoff_;
      src <- src_;
      self;
   }};

   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitSt(self) };
};

class CoolasmLaInstr inherits CoolasmInstr {
   dst : CoolasmReg;
   dst() : CoolasmReg { dst };

   src : CoolasmLabel;
   src() : CoolasmLabel { src };

   init(dst_ : CoolasmReg, src_ : CoolasmLabel) : SELF_TYPE {{
      dst <- dst_;
      src <- src_;
      self;
   }};

   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitLa(self) };
};

class CoolasmAllocInstr inherits CoolasmInstr {
   dst : CoolasmReg;
   dst() : CoolasmReg { dst };

   size : CoolasmReg;
   size() : CoolasmReg { size };

   init(dst_ : CoolasmReg, size_ : CoolasmReg) : SELF_TYPE {{
      dst <- dst_;
      size <- size_;
      self;
   }};

   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitAlloc(self) };
};

class CoolasmConstantIntegerInstr inherits CoolasmInstr {
   value : Int;
   value() : Int { value };

   init(value_ : Int) : SELF_TYPE {{
      value <- value_;
      self;
   }};

   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitConstantInteger(self) };
};

class CoolasmConstantStringInstr inherits CoolasmInstr {
   value : String;
   value() : String { value };

   init(value_ : String) : SELF_TYPE {{
      value <- value_;
      self;
   }};

   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitConstantString(self) };
};

class CoolasmConstantLabelInstr inherits CoolasmAbstractLabelInstr {
   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitConstantLabel(self) };
};

class CoolasmSyscallInstr inherits CoolasmInstr {
   name : String;
   name() : String { name };

   init(name_ : String) : SELF_TYPE {{
      name <- name_;
      self;
   }};

   accept(visitor : CoolasmInstrVisitor) : Object { visitor.visitSyscall(self) };
};
