class CoolasmInterpreterBltInstr inherits CoolasmInterpreterAbstractComparisonInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      if interpreter.getIntReg(reg1) < interpreter.getIntReg(reg2) then
         interpreter.setPc(pc)
      else false fi
   };
};

class CoolasmInterpreterBleInstr inherits CoolasmInterpreterAbstractComparisonInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      if interpreter.getIntReg(reg1) <= interpreter.getIntReg(reg2) then
         interpreter.setPc(pc)
      else false fi
   };
};
