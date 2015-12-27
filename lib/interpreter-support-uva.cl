class InterpreterLessExprState inherits InterpreterBinaryExprState {
   hasLeft : Bool;

   addValue(value : InterpreterValue) : Object {
      if hasLeft then
         result <- new InterpreterBoolValue.init(type,
               if isvoid left then
                  false
               else
                  if isvoid value then
                     false
                  else
                     left.comparisonValue() < value.comparisonValue()
                  fi
               fi)
      else
         {
            left <- value;
            hasLeft <- true;
         }
      fi
   };
};

class InterpreterLessEqualExprState inherits InterpreterBinaryExprState {
   hasLeft : Bool;

   addValue(value : InterpreterValue) : Object {
      if hasLeft then
         result <- new InterpreterBoolValue.init(type,
               if isvoid left then
                  isvoid value
               else
                  if isvoid value then
                     false
                  else
                     left.comparisonValue() <= value.comparisonValue()
                  fi
               fi)
      else
         {
            left <- value;
            hasLeft <- true;
         }
      fi
   };
};
