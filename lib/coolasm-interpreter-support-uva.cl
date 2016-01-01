class CoolasmInterpreterBltInstr inherits CoolasmInterpreterAbstractComparisonInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      if interpreter.getReg(reg1) < interpreter.getReg(reg2) then
         interpreter.setPc(pc)
      else false fi
   };
};

class CoolasmInterpreterBleInstr inherits CoolasmInterpreterAbstractComparisonInstr {
   interpret(interpreter : CoolasmInterpreter) : Object {
      if interpreter.getReg(reg1) <= interpreter.getReg(reg2) then
         interpreter.setPc(pc)
      else false fi
   };
};
