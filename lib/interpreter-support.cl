class InterpreterLessExprState inherits InterpreterBinaryExprState {
   interpret(right : InterpreterValue) : InterpreterValue {
      let left : InterpreterIntValue <- case left of x : InterpreterIntValue => x; esac,
            right : InterpreterIntValue <- case right of x : InterpreterIntValue => x; esac in
         new InterpreterBoolValue.init(type, left.value() < right.value())
   };
};

class InterpreterLessEqualExprState inherits InterpreterBinaryExprState {
   interpret(right : InterpreterValue) : InterpreterValue {
      let left : InterpreterIntValue <- case left of x : InterpreterIntValue => x; esac,
            right : InterpreterIntValue <- case right of x : InterpreterIntValue => x; esac in
         new InterpreterBoolValue.init(type, left.value() <= right.value())
   };
};
