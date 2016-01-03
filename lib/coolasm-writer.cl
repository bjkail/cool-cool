class CoolasmWriter inherits CoolasmInstrVisitor {
   stringUtil : StringUtil <- new StringUtil;
   backslash : String <- stringUtil.backslash();
   doubleQuote : String <- stringUtil.doubleQuote();
   spaces : String <- "                        ";

   io : IO;
   length : Int <- ~1;

   init(io_ : IO) : SELF_TYPE {{
      io <- io_;
      self;
   }};

   out_string(s : String) : SELF_TYPE {{
      if 0 <= length then
         length <- length + s.length()
      else false fi;
      io.out_string(s);
      self;
   }};

   out_int(i : Int) : SELF_TYPE {{
      if 0 <= length then
         length <- length + stringUtil.fromInt(i).length()
      else false fi;
      io.out_int(i);
      self;
   }};

   out_reg(reg : CoolasmReg) : SELF_TYPE {
      let value : Int <- reg.value() in
         if value <= 7 then
            out_string("r".concat(stringUtil.fromDigit(value)))
         else
            if value = 8 then
               out_string("sp")
            else
               if value = 9 then
                  out_string("fp")
               else
                  out_string("ra")
               fi
            fi
         fi
   };

   indent : String <- "        ";
   indent() : String { indent };

   write(program : CoolasmProgram) : Object {
      let iter : Iterator <- program.instrs().iterator(),
            any : Bool in
         while iter.next() loop
            case iter.get() of
               label : CoolasmLabel =>
                  {
                     let comment : String <- label.comment() in
                        if not comment = "" then
                           io.out_string("; ").out_string(comment).out_string("\n")
                        else false fi;

                     io.out_string(label.name()).out_string(":\n");
                  };

               instr : CoolasmInstr =>
                  {
                     io.out_string(indent);
                     writeInstr(instr);
                     io.out_string("\n");
                  };
            esac
         pool
   };

   writeInstr(instr : CoolasmInstr) : Object {
      let comment : String <- instr.comment() in
         if comment = "" then
            instr.accept(self)
         else
            {
               length <- 0;
               instr.accept(self);

               if length < spaces.length() then
                  io.out_string(spaces.substr(length, spaces.length() - length))
               else
                  io.out_string(" ")
               fi;
               length <- ~1;

               io.out_string("; ").out_string(comment);
            }
         fi
   };

   visitLi(instr : CoolasmLiInstr) : Object {
      out_string("li ").out_reg(instr.reg()).out_string(" <- ").out_int(instr.value())
   };

   visitMov(instr : CoolasmMovInstr) : Object {
      out_string("mov ").out_reg(instr.dst()).out_string(" <- ").out_reg(instr.src())
   };

   visitAdd(instr : CoolasmAddInstr) : Object {
      out_string("add ").out_reg(instr.dst()).out_string(" <- ").out_reg(instr.src1()).out_string(" ").out_reg(instr.src2())
   };

   visitSub(instr : CoolasmSubInstr) : Object {
      out_string("sub ").out_reg(instr.dst()).out_string(" <- ").out_reg(instr.src1()).out_string(" ").out_reg(instr.src2())
   };

   visitMul(instr : CoolasmMulInstr) : Object {
      out_string("mul ").out_reg(instr.dst()).out_string(" <- ").out_reg(instr.src1()).out_string(" ").out_reg(instr.src2())
   };

   visitDiv(instr : CoolasmDivInstr) : Object {
      out_string("div ").out_reg(instr.dst()).out_string(" <- ").out_reg(instr.src1()).out_string(" ").out_reg(instr.src2())
   };

   visitJmp(instr : CoolasmJmpInstr) : Object {
      out_string("jmp ").out_string(instr.label().name())
   };

   visitBz(instr : CoolasmBzInstr) : Object {
      out_string("bz ").out_reg(instr.reg()).out_string(" ").out_string(instr.label().name())
   };

   visitBnz(instr : CoolasmBnzInstr) : Object {
      out_string("bnz ").out_reg(instr.reg()).out_string(" ").out_string(instr.label().name())
   };

   visitBeq(instr : CoolasmBeqInstr) : Object {
      out_string("beq ").out_reg(instr.reg1()).out_string(" ").out_reg(instr.reg2()).out_string(" ").out_string(instr.label().name())
   };

   visitBlt(instr : CoolasmBltInstr) : Object {
      out_string("blt ").out_reg(instr.reg1()).out_string(" ").out_reg(instr.reg2()).out_string(" ").out_string(instr.label().name())
   };

   visitBle(instr : CoolasmBleInstr) : Object {
      out_string("ble ").out_reg(instr.reg1()).out_string(" ").out_reg(instr.reg2()).out_string(" ").out_string(instr.label().name())
   };

   visitCallReg(instr : CoolasmCallRegInstr) : Object {
      out_string("call ").out_reg(instr.reg())
   };

   visitCallLabel(instr : CoolasmCallLabelInstr) : Object {
      out_string("call ").out_string(instr.label().name())
   };

   visitReturn(instr : CoolasmReturnInstr) : Object {
      out_string("return")
   };

   visitPush(instr : CoolasmPushInstr) : Object {
      out_string("push ").out_reg(instr.reg())
   };

   visitPop(instr : CoolasmPopInstr) : Object {
      out_string("pop ").out_reg(instr.reg())
   };

   visitLd(instr : CoolasmLdInstr) : Object {
      out_string("ld ").out_reg(instr.dst())
            .out_string(" <- ").out_reg(instr.src())
            .out_string("[").out_int(instr.srcoff()).out_string("]")
   };

   visitSt(instr : CoolasmStInstr) : Object {
      out_string("st ").out_reg(instr.dst())
            .out_string("[").out_int(instr.dstoff()).out_string("]")
            .out_string(" <- ").out_reg(instr.src())
   };

   visitLa(instr : CoolasmLaInstr) : Object {
      out_string("la ").out_reg(instr.dst()).out_string(" <- ").out_string(instr.src().name())
   };

   visitAlloc(instr : CoolasmAllocInstr) : Object {
      out_string("alloc ").out_reg(instr.dst()).out_string(" ").out_reg(instr.size())
   };

   visitConstantInteger(instr : CoolasmConstantIntegerInstr) : Object {
      out_string("constant ").out_int(instr.value())
   };

   visitConstantString(instr : CoolasmConstantStringInstr) : Object {{
      out_string("constant ").out_string(doubleQuote);

      let s : String <- instr.value(),
            i : Int,
            begin : Int in
         {
            while i < s.length() loop
               let c : String <- s.substr(i, 1) in
                  if c = backslash then
                     if "\\".length() = 2 then
                        let c : String <- s.substr(i + 1, 1) in
                           if c = "n" then
                              {
                                 out_string(s.substr(begin, i - begin)).out_string("\\n");
                                 i <- begin <- i + 2;
                              }
                           else
                              if c = "t" then
                                 {
                                    out_string(s.substr(begin, i - begin)).out_string("\\t");
                                    i <- begin <- i + 2;
                                 }
                              else
                                 i <- i + 1
                              fi
                           fi
                     else
                        {
                           if c = "\n" then
                              {
                                 out_string(s.substr(begin, i - begin)).out_string("\\n");
                                 begin <- i + 1;
                              }
                           else
                              if c = "\t" then
                                 {
                                    out_string(s.substr(begin, i - begin)).out_string("\\t");
                                    begin <- i + 1;
                                 }
                              else
                                 if c = "\f" then
                                    {
                                       out_string(s.substr(begin, i - begin)).out_string("\\f");
                                       begin <- i + 1;
                                    }
                                 else
                                    if c = "\b" then
                                       {
                                          out_string(s.substr(begin, i - begin)).out_string("\\b");
                                          begin <- i + 1;
                                       }
                                    else false fi
                                 fi
                              fi
                           fi;

                           i <- i + 1;
                        }
                     fi
                  else
                     i <- i + 1
                  fi
            pool;

            out_string(s.substr(begin, i - begin));
         };

      out_string(doubleQuote);
   }};

   visitConstantLabel(instr : CoolasmConstantLabelInstr) : Object {
      out_string("constant ").out_string(instr.label().name())
   };

   visitSyscall(instr : CoolasmSyscallInstr) : Object {
      out_string("syscall ").out_string(instr.name())
   };
};
