class InterpreterProgram {
   uva : Bool;
   uva() : Bool { uva };

   expr : InterpreterExpr;
   expr() : InterpreterExpr { expr };

   init(uva_ : Bool, expr_ : InterpreterExpr) : SELF_TYPE {{
      uva <- uva_;
      expr <- expr_;
      self;
   }};
};

class InterpreterAttributeInit {
   index : Int;
   index() : Int { index };

   expr : InterpreterExpr;
   expr() : InterpreterExpr { expr };

   init(index_ : Int, expr_ : InterpreterExpr) : SELF_TYPE {{
      index <- index_;
      expr <- expr_;
      self;
   }};
};

class InterpreterType {
   name : String;
   name() : String { name };

   inheritsType : InterpreterType;
   inheritsType() : InterpreterType { inheritsType };
   setInheritsType(inheritsType_ : InterpreterType) : Object { inheritsType <- inheritsType_ };

   inheritsDepth : Int;
   inheritsDepth() : Int { inheritsDepth };

   defaultValue() : InterpreterValue { let void : InterpreterValue in void };

   attributeInits : LinkedList <- new LinkedList;
   attributeInits() : LinkedList { attributeInits };

   methods : IntMap <- new IntTreeMap;
   methods() : IntMap { methods };

   getMethod(method : InterpreterMethod) : InterpreterMethod {
      let result : Object <- methods.getWithInt(method.index()) in
         if isvoid result then
            {
               new ObjectUtil.abortObject(self, "getMethod: type=".concat(name).concat(", method=").concat(method.toString()));
               let void : InterpreterMethod in void;
            }
         else
            case result of x : InterpreterMethod => x; esac
         fi
   };

   init(name_ : String, inheritsDepth_ : Int) : SELF_TYPE {{
      name <- name_;
      inheritsDepth <- inheritsDepth_;
      self;
   }};

   conformsTo(type : InterpreterType) : Bool {
      let typeInheritsDepth : Int <- type.inheritsDepth(),
            inheritsType : InterpreterType <- self in
         {
            while typeInheritsDepth < inheritsType.inheritsDepth() loop
               inheritsType <- inheritsType.inheritsType()
            pool;

            inheritsType = type;
         }
   };
};

class InterpreterMethod {
   containingType : InterpreterType;
   containingType() : InterpreterType { containingType };

   id : String;
   id() : String { id };

   index : Int;
   index() : Int { index };
   setIndex(index_ : Int) : Object { index <- index_ };

   expr : InterpreterExpr;
   expr() : InterpreterExpr { expr };
   setExpr(expr_ : InterpreterExpr) : Object { expr <- expr_ };

   init(containingType_ : InterpreterType, id_ : String, index_ : Int) : SELF_TYPE {{
      containingType <- containingType_;
      id <- id_;
      index <- index_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {{
      state.interpretDispatch(interpreter, expr);
   }};

   toString() : String {
      containingType.name()
            .concat(".").concat(id())
            .concat(":").concat(new StringUtil.fromInt(index))
   };
};

class InterpreterBasicObjectAbortMethod inherits InterpreterMethod {
   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      interpreter.proceedError(~1, "abort")
   };
};

class InterpreterBasicObjectTypeNameMethod inherits InterpreterMethod {
   stringType : InterpreterType;

   initStringType(stringType_ : InterpreterType) : SELF_TYPE {{
      stringType <- stringType_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      interpreter.proceedValue(new InterpreterStringValue.init(stringType, state.target().type().name(), 0))
   };
};

class InterpreterBasicObjectCopyMethod inherits InterpreterMethod {
   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      interpreter.proceedValue(state.target().copyValue())
   };
};

class InterpreterBasicIOOutStringMethod inherits InterpreterMethod {
   backslash : String <- new StringUtil.backslash();

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      let arg : InterpreterStringValue <- case state.args().getWithInt(0) of x : InterpreterStringValue => x; esac,
            s : String <- arg.value() in
         {
            let escapes : Int <- arg.escapes() in
               if escapes = 0 then
                  interpreter.io().outString(s, s.length())
               else
                  let i : Int,
                        begin : Int,
                        escapes : Int in
                     {
                        while i < s.length() loop
                           if s.substr(i, 1) = backslash then
                              if s.substr(i + 1, 1) = backslash then
                                 {
                                    let length : Int <- i + 1 - begin in
                                       interpreter.io().outString(s.substr(begin, length), length - escapes);
                                    begin <- i + 2;
                                    escapes <- 0;
                                    i <- begin;
                                 }
                              else
                                 {
                                    i <- i + 2;
                                    escapes <- escapes + 1;
                                 }
                              fi
                           else
                              i <- i + 1
                           fi
                        pool;

                        if begin < s.length() then
                           let length : Int <- s.length() - begin in
                              interpreter.io().outString(s.substr(begin, length), length - escapes)
                        else false fi;
                     }
               fi;

            interpreter.proceedValue(state.target());
         }
   };
};

class InterpreterUvaBasicIOOutStringMethod inherits InterpreterMethod {
   backslash : String <- new StringUtil.backslash();

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      let arg : InterpreterStringValue <- case state.args().getWithInt(0) of x : InterpreterStringValue => x; esac in
         {
            interpreter.io().outString(arg.value(), arg.length());
            interpreter.proceedValue(state.target());
         }
   };
};

class InterpreterBasicIOOutIntMethod inherits InterpreterMethod {
   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      let arg : InterpreterIntValue <- case state.args().getWithInt(0) of x : InterpreterIntValue => x; esac in
         {
            interpreter.io().out_int(arg.value());
            interpreter.proceedValue(state.target());
         }
   };
};

class InterpreterBasicIOInStringMethod inherits InterpreterMethod {
   escapes : Bool <- "\\".length() = 2;
   backslash : String <- new StringUtil.backslash();

   stringType : InterpreterType;

   initStringType(stringType_ : InterpreterType) : SELF_TYPE {{
      stringType <- stringType_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      if interpreter.hasInput() then
         let s : String <- interpreter.io().in_string() in
            if escapes then
               let result : String,
                     i : Int,
                     begin : Int,
                     escapes : Int in
                  {
                     while i < s.length() loop
                        {
                           if s.substr(i, 1) = backslash then
                              {
                                 result <- result.concat(s.substr(begin, i + 1 - begin));
                                 begin <- i;
                                 escapes <- escapes + 1;
                              }
                           else false fi;

                           i <- i + 1;
                        }
                     pool;

                     result <- result.concat(s.substr(begin, i - begin));
                     interpreter.proceedValue(new InterpreterStringValue.init(stringType, result, escapes));
                  }
            else
               interpreter.proceedValue(new InterpreterStringValue.init(stringType, s, 0))
            fi
      else
         interpreter.proceedError(0, "IO.in_string requires --stdin")
      fi
   };
};

class InterpreterUvaBasicIOInStringMethod inherits InterpreterMethod {
   backslash : String <- new StringUtil.backslash();

   stringType : InterpreterType;

   initStringType(stringType_ : InterpreterType) : SELF_TYPE {{
      stringType <- stringType_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      if interpreter.hasInput() then
         let s : String <- interpreter.io().in_string(),
               escapes : Int in
            {
               let i : Int in
                  while i < s.length() loop
                     {
                        if if s.substr(i, 1) = backslash then
                              if i + 1 < s.length() then
                                 let c : String <- s.substr(i + 1, 1) in
                                    if c = "n" then
                                       true
                                    else
                                       c = "t"
                                    fi
                              else false fi
                           else false fi
                        then
                           escapes <- escapes + 1
                        else false fi;

                        i <- i + 1;
                     }
                  pool;

               interpreter.proceedValue(new InterpreterStringValue.init(stringType, s, escapes));
            }
      else
         interpreter.proceedError(0, "IO.in_string requires --stdin")
      fi
   };
};

class InterpreterBasicIOInIntMethod inherits InterpreterMethod {
   intType : InterpreterType;

   initIntType(intType_ : InterpreterType) : SELF_TYPE {{
      intType <- intType_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      if interpreter.hasInput() then
         interpreter.proceedValue(new InterpreterIntValue.init(intType, interpreter.io().in_int()))
      else
         interpreter.proceedError(0, "IO.in_int requires --stdin")
      fi
   };
};

class InterpreterBasicStringLengthMethod inherits InterpreterMethod {
   intType : InterpreterType;

   initIntType(intType_ : InterpreterType) : SELF_TYPE {{
      intType <- intType_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      let value : InterpreterStringValue <- case state.target() of x : InterpreterStringValue => x; esac in
         interpreter.proceedValue(new InterpreterIntValue.init(intType, value.length()))
   };
};

class InterpreterUvaBasicStringLengthMethod inherits InterpreterMethod {
   intType : InterpreterType;

   initIntType(intType_ : InterpreterType) : SELF_TYPE {{
      intType <- intType_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      let value : InterpreterStringValue <- case state.target() of x : InterpreterStringValue => x; esac in
         interpreter.proceedValue(new InterpreterIntValue.init(intType, value.value().length()))
   };
};

class InterpreterBasicStringConcatMethod inherits InterpreterMethod {
   stringType : InterpreterType;

   initIntType(stringType_ : InterpreterType) : SELF_TYPE {{
      stringType <- stringType_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      let value : InterpreterStringValue <- case state.target() of x : InterpreterStringValue => x; esac,
            arg : InterpreterStringValue <- case state.args().getWithInt(0) of x : InterpreterStringValue => x; esac in
         if arg.isEmpty() then
            interpreter.proceedValue(value)
         else
            if value.isEmpty() then
               interpreter.proceedValue(arg)
            else
               interpreter.proceedValue(new InterpreterStringValue.init(
                     stringType,
                     value.value().concat(arg.value()),
                     value.escapes() + arg.escapes()))
            fi
         fi
   };
};

class InterpreterBasicStringSubstrMethod inherits InterpreterMethod {
   backslash : String <- new StringUtil.backslash();
   stringType : InterpreterType;
   emptyValue : InterpreterValue;

   initBasic(stringType_ : InterpreterType, emptyValue_ : InterpreterValue) : SELF_TYPE {{
      stringType <- stringType_;
      emptyValue <- emptyValue_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      let value : InterpreterStringValue <- case state.target() of x : InterpreterStringValue => x; esac,
            valueLength : Int <- value.length(),
            argBegin : Int <- case state.args().getWithInt(0) of x : InterpreterIntValue => x.value(); esac,
            argLength : Int <- case state.args().getWithInt(1) of x : InterpreterIntValue => x.value(); esac in
         if if argBegin < 0 then
               true
            else
               if argLength < 0 then
                  true
               else
                  valueLength < argBegin + argLength
               fi
            fi
         then
            let stringUtil : StringUtil <- new StringUtil in
               interpreter.proceedError(0, "substr(".concat(stringUtil.fromInt(argBegin))
                     .concat(", ").concat(stringUtil.fromInt(argLength))
                     .concat(") is out of range for string of length ").concat(stringUtil.fromInt(valueLength)))
         else
            if argLength = 0 then
               interpreter.proceedValue(emptyValue)
            else
               if if argBegin = 0 then
                     argLength = valueLength
                  else false fi
               then
                  interpreter.proceedValue(value)
               else
                  let s : String <- value.value(),
                        escapes : Int <- value.escapes() in
                     if escapes = 0 then
                        interpreter.proceedValue(new InterpreterStringValue.init(stringType, s.substr(argBegin, argLength), 0))
                     else
                        let result : String,
                              begin : Int <- argBegin,
                              escapes : Int,
                              i : Int in
                           {
                              let j : Int in
                                 while j < argBegin loop
                                    {
                                       if s.substr(i, 1) = backslash then
                                          {
                                             begin <- begin + 1;
                                             i <- i + 2;
                                          }
                                       else
                                          i <- i + 1
                                       fi;

                                       j <- j + 1;
                                    }
                              pool;

                              let j : Int in
                                 while j < argLength loop
                                    {
                                       if s.substr(i, 1) = backslash then
                                          {
                                             escapes <- escapes + 1;
                                             i <- i + 2;
                                          }
                                       else
                                          i <- i + 1
                                       fi;

                                       j <- j + 1;
                                    }
                                 pool;

                              interpreter.proceedValue(new InterpreterStringValue.init(
                                    stringType,
                                    s.substr(begin, argLength + escapes),
                                    escapes));
                           }
                     fi
               fi
            fi
         fi
   };
};

class InterpreterUvaBasicStringSubstrMethod inherits InterpreterMethod {
   backslash : String <- new StringUtil.backslash();
   stringType : InterpreterType;
   emptyValue : InterpreterValue;

   initBasic(stringType_ : InterpreterType, emptyValue_ : InterpreterValue) : SELF_TYPE {{
      stringType <- stringType_;
      emptyValue <- emptyValue_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      let value : InterpreterStringValue <- case state.target() of x : InterpreterStringValue => x; esac,
            s : String <- value.value(),
            valueLength : Int <- s.length(),
            argBegin : Int <- case state.args().getWithInt(0) of x : InterpreterIntValue => x.value(); esac,
            argLength : Int <- case state.args().getWithInt(1) of x : InterpreterIntValue => x.value(); esac in
         if if argBegin < 0 then
               true
            else
               if argLength < 0 then
                  true
               else
                  valueLength < argBegin + argLength
               fi
            fi
         then
            let stringUtil : StringUtil <- new StringUtil in
               interpreter.proceedError(0, "substr(".concat(stringUtil.fromInt(argBegin))
                     .concat(", ").concat(stringUtil.fromInt(argLength))
                     .concat(") is out of range for string of length ").concat(stringUtil.fromInt(valueLength)))
         else
            if argLength = 0 then
               interpreter.proceedValue(emptyValue)
            else
               if if argBegin = 0 then
                     argLength = valueLength
                  else false fi
               then
                  interpreter.proceedValue(value)
               else
                  let result : String <- s.substr(argBegin, argLength),
                        escapes : Int in
                     {
                        if not value.escapes() = 0 then
                           let i : Int in
                              while i < argLength loop
                                 {
                                    if if result.substr(i, 1) = backslash then
                                          not i + 1 = argLength
                                       else false fi
                                    then
                                       let c : String <- result.substr(i + 1, 1) in
                                          if if c = "n" then
                                                true
                                             else
                                                c = "t"
                                             fi
                                          then
                                             escapes <- escapes + 1
                                          else false fi
                                    else false fi;

                                    i <- i + 1;
                                 }
                              pool
                        else false fi;

                        interpreter.proceedValue(new InterpreterStringValue.init(stringType, result, escapes));
                     }
               fi
            fi
         fi
   };
};

class InterpreterAnalyzerAttribute {
   index : Int;
   index() : Int { index };

   analyzedAttr : AnalyzedAttribute;
   analyzedAttr() : AnalyzedAttribute { analyzedAttr };
   id() : String { analyzedAttr.id() };

   init(index_ : Int, analyzedAttr_ : AnalyzedAttribute) : SELF_TYPE {{
      index <- index_;
      analyzedAttr <- analyzedAttr_;
      self;
   }};
};

class InterpreterAnalyzerMethod {
   analyzedMethod : AnalyzedMethod;
   analyzedMethod() : AnalyzedMethod { analyzedMethod };

   id : String;
   id() : String { id };

   method : InterpreterMethod;
   method() : InterpreterMethod { method };

   index() : Int { method.index() };
   setIndex(index_ : Int) : Object { method.setIndex(index_) };

   initBasic(containingType : InterpreterType, method_ : InterpreterMethod) : SELF_TYPE {{
      id <- method_.id();
      method <- method_;
      self;
   }};

   init(containingType : InterpreterType, index_ : Int, analyzedMethod_ : AnalyzedMethod) : SELF_TYPE {{
      analyzedMethod <- analyzedMethod_;
      id <- analyzedMethod_.id();
      method <- new InterpreterMethod.init(containingType, analyzedMethod_.id(), index_);
      self;
   }};
};

class InterpreterAnalyzerType {
   analyzedType : AnalyzedType;
   analyzedType() : AnalyzedType { analyzedType };

   type : InterpreterType;
   type() : InterpreterType { type };
   name() : String { type.name() };

   inheritsType : InterpreterAnalyzerType;
   inheritsType() : InterpreterAnalyzerType { inheritsType };

   setInheritsType(inheritsType_ : InterpreterAnalyzerType) : Object {{
      inheritsType <- inheritsType_;
      nextAttributeIndex <- inheritsType_.nextAttributeIndex();
      attributeInits <- inheritsType_.attributeInits();
      nextMethodIndex <- inheritsType_.nextMethodIndex();

      let inheritsType : InterpreterType <- inheritsType_.type() in
         {
            type.setInheritsType(inheritsType);
            type.methods().putAll(inheritsType.methods());
         };
   }};

   analyzed : Bool;

   analyze() : Bool {
      if analyzed then
         false
      else
         analyzed <- true
      fi
   };

   analyzedAttributes : Bool;

   analyzeAttributes() : Bool {
      if analyzedAttributes then
         false
      else
         analyzedAttributes <- true
      fi
   };

   attributes : StringMap <- new StringListMap;
   attributes() : StringMap { attributes };

   definedAttributes : Collection <- new LinkedList;
   definedAttributes() : Collection { definedAttributes };

   attributeInits : Bool;
   attributeInits() : Bool { attributeInits };

   nextAttributeIndex : Int;
   nextAttributeIndex() : Int { nextAttributeIndex };

   addAttribute(analyzedAttr : AnalyzedAttribute) : Object {
      let index : Int <-
               let index : Int <- nextAttributeIndex in
                  {
                     nextAttributeIndex <- index + 1;
                     index;
                  },
            attr : InterpreterAnalyzerAttribute <- new InterpreterAnalyzerAttribute.init(index, analyzedAttr) in
         {
            attributes.putWithString(attr.id(), attr);
            definedAttributes.add(attr);

            if not isvoid analyzedAttr.expr() then
               attributeInits <- true
            else false fi;
         }
   };

   getAttribute(id : String) : InterpreterAnalyzerAttribute {
      let attr : Object <- attributes.getWithString(id) in
         if isvoid attr then
            if isvoid inheritsType then
               let void : InterpreterAnalyzerAttribute in void
            else
               inheritsType.getAttribute(id)
            fi
         else
            case attr of x : InterpreterAnalyzerAttribute => x; esac
         fi
   };

   methods : StringMap <- new StringListMap;
   methods() : StringMap { methods };

   addMethodImpl(method : InterpreterAnalyzerMethod) : Object {{
      methods.putWithString(method.id(), method);
      type.methods().putWithInt(method.index(), method.method());
   }};

   nextMethodIndex : Int;
   nextMethodIndex() : Int { nextMethodIndex };

   allocateNextMethodIndex() : Int {
      let index : Int <- nextMethodIndex in
         {
            nextMethodIndex <- index + 1;
            index;
         }
   };

   addBasicMethod(id : String, method : InterpreterMethod) : Object {
      let index : Int <- allocateNextMethodIndex() in
         addMethodImpl(new InterpreterAnalyzerMethod.initBasic(type, method.init(type, id, index)))
   };

   addMethod(analyzedMethod : AnalyzedMethod) : Object {
      let id : String <- analyzedMethod.id(),
            index : Int <-
               let old : InterpreterAnalyzerMethod <- inheritsType.getMethod(analyzedMethod.id()) in
                  if isvoid old then
                     allocateNextMethodIndex()
                  else
                     old.index()
                  fi in
         addMethodImpl(new InterpreterAnalyzerMethod.init(type, index, analyzedMethod))
   };

   getMethod(id : String) : InterpreterAnalyzerMethod {
      let method : Object <- methods.getWithString(id) in
         if isvoid method then
            if isvoid inheritsType then
               let void : InterpreterAnalyzerMethod in void
            else
               inheritsType.getMethod(id)
            fi
         else
            case method of x : InterpreterAnalyzerMethod => x; esac
         fi
   };

   initBasic(name : String, inheritsDepth : Int) : SELF_TYPE {{
      type <- new InterpreterType.init(name, inheritsDepth);
      analyzed <- true;
      analyzedAttributes <- true;
      self;
   }};

   init(analyzedType_ : AnalyzedType) : SELF_TYPE {{
      analyzedType <- analyzedType_;
      type <- new InterpreterType.init(analyzedType_.name(), analyzedType_.inheritsDepth());
      self;
   }};
};

class InterpreterAnalyzer inherits AnalyzedExprVisitor {
   uva : Bool;
   uva() : Bool { uva };

   setUva(uva_ : Bool) : SELF_TYPE {{
      uva <- uva_;
      self;
   }};

   boolType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("Bool", 1);
   defaultBoolValue : InterpreterBoolValue <- new InterpreterBoolValue.init(boolType.type(), false);

   intType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("Int", 1);
   defaultIntValue : InterpreterIntValue <- new InterpreterIntValue.init(intType.type(), 0);

   stringType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("String", 1);
   defaultStringValue : InterpreterStringValue <- new InterpreterStringValue.init(stringType.type(), "", 0);

   types : StringMap <- new StringListMap;

   getType(type : AnalyzedType) : InterpreterAnalyzerType {
      case types.getWithString(type.name()) of x : InterpreterAnalyzerType => x; esac
   };

   getDefaultValue(type : AnalyzedType) : InterpreterValue {
      let name : String <- type.name() in
         if name = "Bool" then
            defaultBoolValue
         else
            if name = "Int" then
               defaultIntValue
            else
               if name = "String" then
                  defaultStringValue
               else
                  let void : InterpreterValue in void
               fi
            fi
         fi
   };

   analyzeType(type : InterpreterAnalyzerType) : Object {
      if type.analyze() then
         let analyzedType : AnalyzedType <- type.analyzedType() in
            {
               let inheritsType : InterpreterAnalyzerType <- getType(analyzedType.inheritsType()) in
                  {
                     analyzeType(inheritsType);
                     type.setInheritsType(inheritsType);
                  };

               let analyzedFeatureIter : Iterator <- analyzedType.definedFeatures().iterator() in
                  while analyzedFeatureIter.next() loop
                     let analyzedFeature : AnalyzedFeature <- case analyzedFeatureIter.get() of x : AnalyzedFeature => x; esac,
                           analyzedAttribute : AnalyzedAttribute <- analyzedFeature.asAttribute() in
                        if isvoid analyzedAttribute then
                           type.addMethod(analyzedFeature.asMethod())
                        else
                           type.addAttribute(analyzedAttribute)
                        fi
                  pool;
            }
      else false fi
   };

   analyzeExpr(expr : AnalyzedExpr) : InterpreterExpr {
      case expr.accept(self) of x : InterpreterExpr => x; esac
   };

   analyzeExprs(exprs : Collection) : Collection {
      let result : Collection <- new LinkedList in
         {
            let iter : Iterator <- exprs.iterator() in
               while iter.next() loop
                  result.add(case iter.get() of x : AnalyzedExpr => x.accept(self); esac)
               pool;

            result;
         }
   };

   analyzeAttributes(type : InterpreterAnalyzerType) : Object {
      if type.analyzeAttributes() then
         let inheritsType : InterpreterAnalyzerType <- type.inheritsType() in
            {
               analyzeAttributes(inheritsType);

               let attrInits : LinkedList <- type.type().attributeInits() in
                  {
                     attrInits.addAll(inheritsType.type().attributeInits());

                     let iter : Iterator <- type.definedAttributes().iterator() in
                        while iter.next() loop
                           let attr : InterpreterAnalyzerAttribute <- case iter.get() of x : InterpreterAnalyzerAttribute => x; esac,
                                 expr : AnalyzedExpr <- attr.analyzedAttr().expr() in
                              if not isvoid expr then
                                 attrInits.add(new InterpreterAttributeInit.init(attr.index(), analyzeExpr(expr)))
                              else false fi
                        pool;
                  };
            }
      else false fi
   };

   analyze(program : AnalyzedProgram) : InterpreterProgram {{
      let objectType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("Object", 0),
            ioType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("IO", 1) in
         {
            types.putWithString(objectType.name(), objectType);
            objectType.addBasicMethod("abort", new InterpreterBasicObjectAbortMethod);
            objectType.addBasicMethod("type_name", new InterpreterBasicObjectTypeNameMethod.initStringType(stringType.type()));
            objectType.addBasicMethod("copy", new InterpreterBasicObjectCopyMethod);

            types.putWithString(ioType.name(), ioType);
            ioType.setInheritsType(objectType);
            ioType.addBasicMethod("out_string",
                  if uva then
                     new InterpreterUvaBasicIOOutStringMethod
                  else
                     new InterpreterBasicIOOutStringMethod
                  fi);
            ioType.addBasicMethod("out_int", new InterpreterBasicIOOutIntMethod);
            ioType.addBasicMethod("in_string",
                  if uva then
                     new InterpreterUvaBasicIOInStringMethod.initStringType(stringType.type())
                  else
                     new InterpreterBasicIOInStringMethod.initStringType(stringType.type())
                  fi);
            ioType.addBasicMethod("in_int", new InterpreterBasicIOInIntMethod.initIntType(intType.type()));

            types.putWithString(intType.name(), intType);
            intType.setInheritsType(objectType);

            types.putWithString(stringType.name(), stringType);
            stringType.setInheritsType(objectType);
            stringType.addBasicMethod("length",
                  if uva then
                     new InterpreterUvaBasicStringLengthMethod.initIntType(intType.type())
                  else
                     new InterpreterBasicStringLengthMethod.initIntType(intType.type())
                  fi);
            stringType.addBasicMethod("concat", new InterpreterBasicStringConcatMethod.initIntType(stringType.type()));
            stringType.addBasicMethod("substr",
                  if uva then
                     new InterpreterUvaBasicStringSubstrMethod.initBasic(stringType.type(), defaultStringValue)
                  else
                     new InterpreterBasicStringSubstrMethod.initBasic(stringType.type(), defaultStringValue)
                  fi);

            types.putWithString(boolType.name(), boolType);
            boolType.setInheritsType(objectType);
         };

      let typeList : Collection <- new LinkedList in
         {
            -- Create InterpreterAnalyzerType
            let analyzedTypeIter : Iterator <- program.definedTypes().iterator() in
               while analyzedTypeIter.next() loop
                  let analyzedType : AnalyzedType <- case analyzedTypeIter.get() of x : AnalyzedType => x; esac,
                        type : InterpreterAnalyzerType <- new InterpreterAnalyzerType.init(analyzedType) in
                     {
                        types.putWithString(type.name(), type);
                        typeList.add(type);
                     }
               pool;

            -- Set inheritsType, and create attributes and methods.
            let typeIter : Iterator <- typeList.iterator() in
               while typeIter.next() loop
                  let type : InterpreterAnalyzerType <- case typeIter.get() of x : InterpreterAnalyzerType => x; esac in
                     analyzeType(type)
               pool;

            -- Analyze attributes/methods.
            let typeIter : Iterator <- typeList.iterator() in
               while typeIter.next() loop
                  let type : InterpreterAnalyzerType <- case typeIter.get() of x : InterpreterAnalyzerType => x; esac in
                     {
                        analyzeAttributes(type);

                        let methodIter : StringMapIterator <- type.methods().iterator() in
                           while methodIter.next() loop
                              let method : InterpreterAnalyzerMethod <- case methodIter.value() of x : InterpreterAnalyzerMethod => x; esac in
                                 method.method().setExpr(analyzeExpr(method.analyzedMethod().expr()))
                           pool;
                     }
               pool;

            let mainMethod : AnalyzedMethod <- program.mainMethod(),
                  mainType : AnalyzedType <- mainMethod.containingType(),
                  type : InterpreterAnalyzerType <- getType(mainType),
                  -- Manually build and analyze a "new Main.main()" expression
                  -- to use as the program entry point.
                  newExpr : AnalyzedExpr <- new AnalyzedNewExpr.init(0, mainType),
                  dispatchExpr : AnalyzedExpr <- new AnalyzedDispatchExpr.init(0, mainMethod.returnType(), newExpr, mainMethod, true, new Collection) in
               new InterpreterProgram.init(uva, analyzeExpr(dispatchExpr));
         };
   }};

   visitBlock(expr : AnalyzedBlockExpr) : Object {
      new InterpreterBlockExpr.init(analyzeExprs(expr.exprs()))
   };

   visitIf(expr : AnalyzedIfExpr) : Object {
      new InterpreterIfExpr.init(analyzeExpr(expr.expr()), analyzeExpr(expr.then_()), analyzeExpr(expr.else_()))
   };

   visitWhile(expr : AnalyzedWhileExpr) : Object {
      new InterpreterWhileExpr.init(analyzeExpr(expr.expr()), analyzeExpr(expr.loop_()))
   };

   visitLet(expr : AnalyzedLetExpr) : Object {
      let exprs : Collection <- new LinkedList,
            vars : Collection <- expr.vars(),
            firstVarIndex : Int in
         {
            let iter : Iterator <- vars.iterator() in
               {
                  iter.next();
                  firstVarIndex <- case iter.get() of x : AnalyzedLetVar => x.object().index(); esac;
               };

            let iter : Iterator <- vars.iterator() in
               while iter.next() loop
                  let var : AnalyzedLetVar <- case iter.get() of x : AnalyzedLetVar => x; esac,
                        expr : AnalyzedExpr <- var.expr() in
                     if not isvoid expr then
                        exprs.add(new InterpreterVarAssignmentExpr.init(var.object().index(), analyzeExpr(expr)))
                     else false fi
               pool;

            exprs.add(analyzeExpr(expr.expr()));
            new InterpreterLetExpr.init(firstVarIndex, vars.size(), exprs);
         }
   };

   visitCase(expr : AnalyzedCaseExpr) : Object {
      let branches : LinkedList <- new LinkedList in
         {
            let iter : Iterator <- expr.branches().iterator() in
               while iter.next() loop
                  let branch : AnalyzedCaseBranch <- case iter.get() of x : AnalyzedCaseBranch => x; esac in
                     branches.add(new InterpreterCaseBranch.init(getType(branch.checkType()).type(), analyzeExpr(branch.expr())))
               pool;

            branches.sort(new InterpreterCaseBranchComparator);
            new InterpreterCaseExpr.init(expr.line(), analyzeExpr(expr.expr()), expr.varIndex(), branches);
         }
   };

   visitArgumentAssignment(object : AnalyzedArgumentObject, expr : AnalyzedExpr) : Object {
      new InterpreterArgumentAssignmentExpr.init(object.index(), analyzeExpr(expr))
   };

   visitVarAssignment(object : AnalyzedVarObject, expr : AnalyzedExpr) : Object {
      new InterpreterVarAssignmentExpr.init(object.index(), analyzeExpr(expr))
   };

   visitAttributeAssignment(object : AnalyzedAttributeObject, expr : AnalyzedExpr) : Object {
      let attribute : AnalyzedAttribute <- object.attribute() in
         new InterpreterAttributeAssignmentExpr.init(
               getType(attribute.containingType()).getAttribute(attribute.id()).index(),
               analyzeExpr(expr))
   };

   visitSelf(object : AnalyzedSelfObject) : Object {
      new InterpreterSelfExpr
   };

   visitArgument(object : AnalyzedArgumentObject) : Object {
      new InterpreterArgumentExpr.init(object.index())
   };

   visitVar(object : AnalyzedVarObject) : Object {
      new InterpreterVarExpr.init(object.index(), getDefaultValue(object.type()))
   };

   visitAttribute(object : AnalyzedAttributeObject) : Object {
      let attribute : AnalyzedAttribute <- object.attribute() in
         new InterpreterAttributeExpr.init(
               getType(attribute.containingType()).getAttribute(attribute.id()).index(),
               getDefaultValue(attribute.type()))
   };

   visitNew(expr : AnalyzedNewExpr) : Object {
      let type : AnalyzedType <- expr.type() in
         if type.isSelfType() then
            new InterpreterNewSelfTypeExpr.init(expr.line())
         else
            let type : InterpreterAnalyzerType <- getType(type) in
               if type = boolType then
                  new InterpreterValueExpr.init(defaultBoolValue)
               else
                  if type = intType then
                     new InterpreterValueExpr.init(defaultIntValue)
                  else
                     if type = stringType then
                        new InterpreterValueExpr.init(defaultStringValue)
                     else
                        if type.attributeInits() then
                           new InterpreterNewExpr.init(expr.line(), type.type())
                        else
                           new InterpreterSimpleNewExpr.init(expr.line(), type.type())
                        fi
                     fi
                  fi
               fi
         fi
   };

   visitDispatch(expr : AnalyzedDispatchExpr) : Object {
      let method : AnalyzedMethod <- expr.method(),
            dispatchExpr : InterpreterDispatchExpr <-
               if expr.static() then
                  new InterpreterStaticDispatchExpr
               else
                  new InterpreterDispatchExpr
               fi in
         dispatchExpr.init(
               expr.line(),
               analyzeExprs(expr.arguments()),
               analyzeExpr(expr.expr()),
               getType(method.containingType()).getMethod(method.id()).method())
   };

   visitUnary(expr : AnalyzedUnaryExpr) : Object {
      let op : String <- expr.op() in
         if op = "isvoid" then
            new InterpreterIsVoidExpr.init(boolType.type(), analyzeExpr(expr.expr()))
         else
            if op = "~" then
               new InterpreterComplementExpr.init(intType.type(), analyzeExpr(expr.expr()))
            else
               if op = "not" then
                  new InterpreterNotExpr.init(boolType.type(), analyzeExpr(expr.expr()))
               else new ObjectUtil.abortObject(self, "visitUnary: unimplemented ".concat(op)) fi
            fi
         fi
   };

   visitBinary(expr : AnalyzedBinaryExpr) : Object {
      let left : InterpreterExpr <- analyzeExpr(expr.left()),
            right : InterpreterExpr <- analyzeExpr(expr.right()),
            op : String <- expr.op() in
         if op = "+" then
            new InterpreterAddExpr.init(intType.type(), left, right)
         else
            if op = "-" then
               new InterpreterSubtractExpr.init(intType.type(), left, right)
            else
               if op = "*" then
                  new InterpreterMultiplyExpr.init(intType.type(), left, right)
               else
                  if op = "/" then
                     new InterpreterDivideExpr.init(expr.line(), intType.type(), left, right)
                  else
                     if op = "<" then
                        new InterpreterLessExpr.init(boolType.type(), left, right)
                     else
                        if op = "<=" then
                           new InterpreterLessEqualExpr.init(boolType.type(), left, right)
                        else
                           if op = "=" then
                              new InterpreterEqualExpr.init(boolType.type(), left, right)
                           else new ObjectUtil.abortObject(self, "visitBinary: unimplemented".concat(op)) fi
                        fi
                     fi
                  fi
               fi
            fi
         fi
   };

   visitConstantBool(expr : AnalyzedConstantBoolExpr) : Object {
      new InterpreterValueExpr.init(new InterpreterBoolValue.init(boolType.type(), expr.value()))
   };

   visitConstantInt(expr : AnalyzedConstantIntExpr) : Object {
      new InterpreterValueExpr.init(new InterpreterIntValue.init(intType.type(), expr.value()))
   };

   visitConstantString(expr : AnalyzedConstantStringExpr) : Object {
      new InterpreterValueExpr.init(new InterpreterStringValue.init(stringType.type(), expr.value(), expr.escapes()))
   };
};

class InterpreterValue {
   type : InterpreterType;
   type() : InterpreterType { type };

   comparisonValue() : Object { self };
   copyValue() : InterpreterValue { self };
   toString() : String { self.type_name() };
};

class InterpreterErrorValue inherits InterpreterValue {
   value : String;
   value() : String { value };

   stack : String;
   stack() : String { stack };

   init(value_ : String, stack_ : String) : SELF_TYPE {{
      value <- value_;
      stack <- stack_;
      self;
   }};

   toString() : String { "error: ".concat(value) };
};

class InterpreterObjectValue inherits InterpreterValue {
   attributes : IntMap <- new IntTreeMap;
   attributes() : IntMap { attributes };

   init(type_ : InterpreterType) : SELF_TYPE {{
      type <- type_;
      self;
   }};

   copyValue() : InterpreterValue {
      let copy : InterpreterObjectValue <- new InterpreterObjectValue.init(type) in
         {
            copy.attributes().putAll(attributes);
            copy;
         }
   };

   toString() : String { "object ".concat(type.name()) };
};

class InterpreterBoolValue inherits InterpreterValue {
   value : Bool;
   value() : Bool { value };

   init(type_ : InterpreterType, value_ : Bool) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};

   comparisonValue() : Object { value };

   toString() : String { if value then "true" else "false" fi };
};

class InterpreterIntValue inherits InterpreterValue {
   value : Int;
   value() : Int { value };

   init(type_ : InterpreterType, value_ : Int) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};

   comparisonValue() : Object { value };

   toString() : String { new StringUtil.fromInt(value) };
};

class InterpreterStringValue inherits InterpreterValue {
   value : String;
   value() : String { value };

   escapes : Int;
   escapes() : Int { escapes };

   init(type_ : InterpreterType, value_ : String, escapes_ : Int) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      escapes <- escapes_;
      self;
   }};

   length() : Int {
      value.length() - escapes
   };

   isEmpty() : Bool {
      value.length() = 0
   };

   comparisonValue() : Object { value };

   toString() : String { "string[".concat(value).concat("]") };
};

class InterpreterExprState {
   prev : InterpreterExprState;
   prev() : InterpreterExprState { prev };
   setPrev(prev_ : InterpreterExprState) : Object { prev <- prev_ };

   -- Must call proceedValue or pushState
   proceed(interpreter : Interpreter) : Bool { new ObjectUtil.abortBool(self, "proceed: unimplemented") };
   addValue(value : InterpreterValue) : Object { new ObjectUtil.abortObject(self, "addValue: unimplemented") };

   toString() : String { type_name() };
};

class InterpreterExpr {
   -- Must call interpretValue or pushState
   interpret(interpreter : Interpreter) : Bool { new ObjectUtil.abortBool(self, "interpret: unimplemented") };

   toString() : String { type_name() };
};

class InterpreterBlockExpr inherits InterpreterExpr {
   exprs : Collection;

   init(exprs_ : Collection) : SELF_TYPE {{
      exprs <- exprs_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(new InterpreterBlockExprState.init(exprs));
   }};
};

class InterpreterBlockExprState inherits InterpreterExprState {
   exprIter : Iterator;
   value : InterpreterValue;

   init(exprs : Collection) : SELF_TYPE {{
      exprIter <- exprs.iterator();
      self;
   }};

   proceed(interpreter : Interpreter) : Bool {
      if exprIter.next() then
         case exprIter.get() of x : InterpreterExpr => x.interpret(interpreter); esac
      else
         interpreter.proceedValue(value)
      fi
   };

   addValue(value_ : InterpreterValue) : Object {
      value <- value_
   };
};

class InterpreterIfExpr inherits InterpreterExpr {
   expr : InterpreterExpr;
   then_ : InterpreterExpr;
   else_ : InterpreterExpr;

   init(expr_ : InterpreterExpr, then__ : InterpreterExpr, else__ : InterpreterExpr) : SELF_TYPE {{
      expr <- expr_;
      then_ <- then__;
      else_ <- else__;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(new InterpreterIfExprState.init(then_, else_));
      expr.interpret(interpreter);
   }};
};

class InterpreterIfExprState inherits InterpreterExprState {
   then_ : InterpreterExpr;
   else_ : InterpreterExpr;
   expr : InterpreterExpr;

   init(then__ : InterpreterExpr, else__ : InterpreterExpr) : SELF_TYPE {{
      then_ <- then__;
      else_ <- else__;
      self;
   }};

   addValue(value : InterpreterValue) : Object {
      if case value of x : InterpreterBoolValue => x.value(); esac then
         expr <- then_
      else
         expr <- else_
      fi
   };

   proceed(interpreter : Interpreter) : Bool {
      interpreter.proceedExpr(expr)
   };
};

class InterpreterWhileExpr inherits InterpreterExpr {
   expr : InterpreterExpr;
   loop_ : InterpreterExpr;

   init(expr_ : InterpreterExpr, loop__ : InterpreterExpr) : SELF_TYPE {{
      expr <- expr_;
      loop_ <- loop__;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(new InterpreterWhileExprState.init(expr, loop_));
      expr.interpret(interpreter);
   }};
};

class InterpreterWhileExprState inherits InterpreterExprState {
   expr : InterpreterExpr;
   loop_ : InterpreterExpr;
   test : Bool <- true;
   continue : Bool;

   init(expr_ : InterpreterExpr, loop__ : InterpreterExpr) : SELF_TYPE {{
      expr <- expr_;
      loop_ <- loop__;
      self;
   }};

   addValue(value : InterpreterValue) : Object {
      if test then
         continue <- case value of x : InterpreterBoolValue => x.value(); esac
      else false fi
   };

   proceed(interpreter : Interpreter) : Bool {
      if continue then
         if test then
            {
               test <- false;
               loop_.interpret(interpreter);
            }
         else
            {
               test <- true;
               expr.interpret(interpreter);
            }
         fi
      else
         interpreter.proceedValue(let void : InterpreterValue in void)
      fi
   };
};

class InterpreterLetExpr inherits InterpreterExpr {
   firstVarIndex : Int;
   lastVarIndex : Int;
   exprs : Collection;

   init(firstVarIndex_ : Int, numVars : Int, exprs_ : Collection) : SELF_TYPE {{
      firstVarIndex <- firstVarIndex_;
      lastVarIndex <- firstVarIndex + numVars - 1;
      exprs <- exprs_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {{
      interpreter.initVars();
      interpreter.pushState(new InterpreterLetExprState.init(firstVarIndex, lastVarIndex, exprs));
   }};
};

class InterpreterLetExprState inherits InterpreterExprState {
   firstVarIndex : Int;
   lastVarIndex : Int;
   exprIter : Iterator;
   value : InterpreterValue;

   init(firstVarIndex_ : Int, lastVarIndex_ : Int, exprs : Collection) : SELF_TYPE {{
      firstVarIndex <- firstVarIndex_;
      lastVarIndex <- lastVarIndex_;
      exprIter <- exprs.iterator();
      self;
   }};

   addValue(value_ : InterpreterValue) : Object {
      value <- value_
   };

   proceed(interpreter : Interpreter) : Bool {
      if exprIter.next() then
         case exprIter.get() of x : InterpreterExpr => x.interpret(interpreter); esac
      else
         {
            if not firstVarIndex = 0 then
               let i : Int <- lastVarIndex,
                     vars : IntMap <- interpreter.vars() in
                  while firstVarIndex <= i loop
                     {
                        vars.removeWithInt(i);
                        i <- i - 1;
                     }
                  pool
            else false fi;

            interpreter.proceedValue(value);
         }
      fi
   };
};

class InterpreterAssignmentExpr inherits InterpreterExpr {
   index : Int;
   expr : InterpreterExpr;

   init(index_ : Int, expr_ : InterpreterExpr) : SELF_TYPE {{
      index <- index_;
      expr <- expr_;
      self;
   }};

   newState() : InterpreterAssignmentExprState {{
      new ObjectUtil.abortObject(self, "newState: unimplemented");
      let void : InterpreterAssignmentExprState in void;
   }};

   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(newState().init(index));
      expr.interpret(interpreter);
   }};
};

class InterpreterAssignmentExprState inherits InterpreterExprState {
   index : Int;
   value : InterpreterValue;

   init(index_ : Int) : SELF_TYPE {{
      index <- index_;
      self;
   }};

   addValue(value_ : InterpreterValue) : Object {
      value <- value_
   };
};

class InterpreterCaseBranch {
   checkType : InterpreterType;
   checkType() : InterpreterType { checkType };

   expr : InterpreterExpr;
   expr() : InterpreterExpr { expr };

   init(checkType_ : InterpreterType, expr_ : InterpreterExpr) : SELF_TYPE {{
      checkType <- checkType_;
      expr <- expr_;
      self;
   }};
};

class InterpreterCaseBranchComparator inherits Comparator {
   compare(o1 : Object, o2 : Object) : Int {
      let inheritsDepth1 : Int <- case o1 of x : InterpreterCaseBranch => x.checkType().inheritsDepth(); esac,
            inheritsDepth2 : Int <- case o2 of x : InterpreterCaseBranch => x.checkType().inheritsDepth(); esac in
         if inheritsDepth1 = inheritsDepth2 then
            0
         else
            -- Sort highest entries first.
            if inheritsDepth1 < inheritsDepth2 then
               1
            else
               ~1
            fi
         fi
   };
};

class InterpreterCaseExpr inherits InterpreterExpr {
   line : Int;
   expr : InterpreterExpr;
   varIndex : Int;
   branches : Collection;

   init(line_ : Int, expr_ : InterpreterExpr, varIndex_ : Int, branches_ : Collection) : SELF_TYPE {{
      line <- line_;
      expr <- expr_;
      varIndex <- varIndex_;
      branches <- branches_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(new InterpreterCaseExprState.init(line, varIndex, branches));
      expr.interpret(interpreter);
   }};
};

class InterpreterCaseExprState inherits InterpreterExprState {
   line : Int;
   varIndex : Int;
   branches : Collection;
   value : InterpreterValue;
   expr : InterpreterExpr;

   init(line_ : Int, varIndex_ : Int, branches_ : Collection) : SELF_TYPE {{
      line <- line_;
      varIndex <- varIndex_;
      branches <- branches_;
      self;
   }};

   addValue(value_ : InterpreterValue) : Object {
      value <- value_
   };

   proceed(interpreter : Interpreter) : Bool {
      if isvoid value then
         interpreter.proceedError(line, "case on void")
      else
         let type : InterpreterType <- value.type(),
               expr : InterpreterExpr in
            {
               let iter : Iterator <- branches.iterator(),
                     continue : Bool <- true in
                  {
                     iter.next();
                     while continue loop
                        let branch : InterpreterCaseBranch <- case iter.get() of x : InterpreterCaseBranch => x; esac in
                           if type.conformsTo(branch.checkType()) then
                              {
                                 expr <- branch.expr();
                                 continue <- false;
                              }
                           else
                              continue <- iter.next()
                           fi
                     pool;
                  };

               if isvoid expr then
                  interpreter.proceedError(line, "case branch not matched for type '"
                        .concat(type.name()).concat("'"))
               else
                  {
                     interpreter.initVars().putWithInt(varIndex, value);
                     interpreter.proceedExpr(expr);
                  }
               fi;
            }
      fi
   };
};

class InterpreterArgumentAssignmentExpr inherits InterpreterAssignmentExpr {
   newState() : InterpreterAssignmentExprState {
      new InterpreterArgumentAssignmentExprState
   };
};

class InterpreterArgumentAssignmentExprState inherits InterpreterAssignmentExprState {
   proceed(interpreter : Interpreter) : Bool {{
      interpreter.arguments().putWithInt(index, value);
      interpreter.proceedValue(value);
   }};
};

class InterpreterVarAssignmentExpr inherits InterpreterAssignmentExpr {
   newState() : InterpreterAssignmentExprState {
      new InterpreterVarAssignmentExprState
   };
};

class InterpreterVarAssignmentExprState inherits InterpreterAssignmentExprState {
   proceed(interpreter : Interpreter) : Bool {{
      interpreter.vars().putWithInt(index, value);
      interpreter.proceedValue(value);
   }};
};

class InterpreterAttributeAssignmentExpr inherits InterpreterAssignmentExpr {
   newState() : InterpreterAssignmentExprState {
      new InterpreterAttributeAssignmentExprState
   };
};

class InterpreterAttributeAssignmentExprState inherits InterpreterAssignmentExprState {
   proceed(interpreter : Interpreter) : Bool {{
      interpreter.selfObject().attributes().putWithInt(index, value);
      interpreter.proceedValue(value);
   }};
};

class InterpreterSelfExpr inherits InterpreterExpr {
   interpret(interpreter : Interpreter) : Bool {
      interpreter.interpretValue(interpreter.selfObject())
   };

   toString() : String { "self" };
};

class InterpreterArgumentExpr inherits InterpreterExpr {
   index : Int;

   init(index_ : Int) : SELF_TYPE {{
      index <- index_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      let value : Object <- interpreter.arguments().getWithInt(index) in
         if isvoid value then
            interpreter.interpretValue(let void : InterpreterValue in void)
         else
            interpreter.interpretValue(case value of x : InterpreterValue => x; esac)
         fi
   };
};

class InterpreterObjectExpr inherits InterpreterExpr {
   index : Int;
   defaultValue : InterpreterValue;

   init(index_ : Int, defaultValue_ : InterpreterValue) : SELF_TYPE {{
      index <- index_;
      defaultValue <- defaultValue_;
      self;
   }};

   interpretObject(interpreter : Interpreter) : Object {
      new ObjectUtil.abortObject(self, "interpretObject: unimplemented")
   };

   interpret(interpreter : Interpreter) : Bool {
      let value : Object <- interpretObject(interpreter) in
         if isvoid value then
            interpreter.interpretValue(defaultValue)
         else
            interpreter.interpretValue(case value of x : InterpreterValue => x; esac)
         fi
   };
};

class InterpreterVarExpr inherits InterpreterObjectExpr {
   interpretObject(interpreter : Interpreter) : Object {
      interpreter.vars().getWithInt(index)
   };
};

class InterpreterAttributeExpr inherits InterpreterObjectExpr {
   interpretObject(interpreter : Interpreter) : Object {
      interpreter.selfObject().attributes().getWithInt(index)
   };
};

class InterpreterNewExpr inherits InterpreterExpr {
   line : Int;
   type : InterpreterType;

   init(line_ : Int, type_ : InterpreterType) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      interpreter.pushState(new InterpreterNewExprState.init(line, type, interpreter))
   };

   toString() : String { "new ".concat(type.name()) };
};

class InterpreterNewExprState inherits InterpreterExprState {
   line : Int;
   line() : Int { line };

   type : InterpreterType;
   attrInitIter : Iterator;

   savedSelfObject : InterpreterObjectValue;
   selfObject : InterpreterObjectValue;
   selfObjectAttributes : IntMap;
   attrInit : InterpreterAttributeInit;

   init(line_ : Int, type_ : InterpreterType, interpreter : Interpreter) : SELF_TYPE {{
      line <- line_;
      type <- type_;

      selfObject <- new InterpreterObjectValue.init(type_);
      selfObjectAttributes <- selfObject.attributes();

      self;
   }};

   proceed(interpreter : Interpreter) : Bool {
      if isvoid attrInitIter then
         {
            if interpreter.debugDispatch() then
               interpreter.debugOut("initializing ".concat(type.name()))
            else false fi;

            savedSelfObject <- interpreter.selfObject();
            if interpreter.pushNewActivationRecord(selfObject) then
               {
                  attrInitIter <- type.attributeInits().iterator();
                  proceedInit(interpreter);
               }
            else
               interpreter.proceedError(line, "stack overflow")
            fi;
         }
      else
         proceedInit(interpreter)
      fi
   };

   proceedInit(interpreter : Interpreter) : Bool {
      if attrInitIter.next() then
         {
            attrInit <- case attrInitIter.get() of x : InterpreterAttributeInit => x; esac;
            attrInit.expr().interpret(interpreter);
         }
      else
         {
            if interpreter.debugDispatch() then
               interpreter.debugOut("initialized")
            else false fi;

            interpreter.popNewActivationRecord(savedSelfObject);
            interpreter.proceedValue(selfObject);
         }
      fi
   };

   addValue(value : InterpreterValue) : Object {
      selfObjectAttributes.putWithInt(attrInit.index(), value)
   };

   stackEntry() : String {
      "new ".concat(selfObject.type().name())
   };

   toString() : String { "new ".concat(type.name()) };
};

class InterpreterNewSelfTypeExpr inherits InterpreterExpr {
   line : Int;

   init(line_ : Int) : SELF_TYPE {{
      line <- line_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      interpreter.pushState(new InterpreterNewExprState.init(line, interpreter.selfObject().type(), interpreter))
   };
};

class InterpreterSimpleNewExpr inherits InterpreterExpr {
   line : Int;
   type : InterpreterType;

   init(line_ : Int, type_ : InterpreterType) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      if interpreter.canPushActivationRecord() then
         interpreter.interpretValue(new InterpreterObjectValue.init(type))
      else
         interpreter.proceedError(line, "stack overflow")
      fi
   };

   toString() : String { "new.simple ".concat(type.name()) };
};

class InterpreterDispatchExpr inherits InterpreterExpr {
   line : Int;
   arguments : Collection;
   target : InterpreterExpr;
   method : InterpreterMethod;

   init(line_ : Int, arguments_ : Collection, target_ : InterpreterExpr, method_ : InterpreterMethod) : SELF_TYPE {{
      line <- line_;
      arguments <- arguments_;
      target <- target_;
      method <- method_;
      self;
   }};

   createState() : InterpreterDispatchExprState { new InterpreterDispatchExprState };

   interpret(interpreter : Interpreter) : Bool {
      interpreter.pushState(createState().init(line, arguments, target, method))
   };

   toString() : String { "dispatch ".concat(method.toString()) };
};

class InterpreterDispatchExprState inherits InterpreterExprState {
   line : Int;
   line() : Int { line };

   numArgs : Int;
   argExprIter : Iterator;
   targetExpr : InterpreterExpr;
   method : InterpreterMethod;

   args : IntMap <- new IntTreeMap;
   args() : IntMap { args };

   hasTarget : Bool;

   target : InterpreterValue;
   target() : InterpreterValue { target };

   hasResult : Bool;
   result : InterpreterValue;

   savedSelfObject : InterpreterObjectValue;
   savedArgs : IntMap;
   savedVars : IntMap;

   init(line_ : Int, argExprs : Collection, targetExpr_ : InterpreterExpr, method_ : InterpreterMethod) : SELF_TYPE {{
      line <- line_;
      numArgs <- argExprs.size();
      argExprIter <- argExprs.iterator();
      targetExpr <- targetExpr_;
      method <- method_;
      self;
   }};

   addValue(value : InterpreterValue) : Object {
      if args.size() < numArgs then
         args.putWithInt(args.size(), value)
      else
         if not hasTarget then
            {
               target <- value;
               hasTarget <- true;
            }
         else
            {
               result <- value;
               hasResult <- true;
            }
         fi
      fi
   };

   lookupMethod() : InterpreterMethod {
      target.type().getMethod(method)
   };

   proceed(interpreter : Interpreter) : Bool {
      if argExprIter.next() then
         {
            if interpreter.debug() then
               interpreter.debugOut("  arg ".concat(new StringUtil.fromInt(args.size() + 1)))
            else false fi;

            case argExprIter.get() of x : InterpreterExpr => x.interpret(interpreter); esac;
         }
      else
         if not hasTarget then
            {
               if interpreter.debug() then
                  interpreter.debugOut("  target ".concat(targetExpr.toString()))
               else false fi;

               targetExpr.interpret(interpreter);
            }
         else
            if not hasResult then
               if isvoid target then
                  interpreter.proceedError(line, "dispatch on void for method '".concat(method.id())
                        .concat("' in type '").concat(method.containingType().name()).concat("'"))
               else
                  if interpreter.canPushActivationRecord() then
                     let method : InterpreterMethod <- lookupMethod() in
                        {
                           if interpreter.debugDispatch() then
                              interpreter.debugOut("call ".concat(method.toString())
                                    .concat(" on ").concat(target.type().name()))
                           else false fi;

                           method.interpret(interpreter, self);
                        }
                  else
                     interpreter.proceedError(line, "stack overflow")
                  fi
               fi
            else
               {
                  if interpreter.debugDispatch() then
                     interpreter.debugOut("return ".concat(if isvoid result then "void" else result.type_name() fi))
                  else false fi;

                  interpreter.popDispatchContext(savedSelfObject, savedArgs, savedVars);
                  interpreter.proceedValue(result);
               }
            fi
         fi
      fi
   };

   interpretDispatch(interpreter : Interpreter, expr : InterpreterExpr) : Bool {{
      savedSelfObject <- interpreter.selfObject();
      savedArgs <- interpreter.arguments();
      savedVars <- interpreter.vars();
      interpreter.pushDispatchContext(case target of x : InterpreterObjectValue => x; esac, args);

      expr.interpret(interpreter);
   }};

   stackEntry() : String {
      if not isvoid target then
         method.containingType().name().concat(".").concat(method.id())
      else "" fi
   };

   toString() : String { "dispatch ".concat(method.toString()) };
};

class InterpreterStaticDispatchExpr inherits InterpreterDispatchExpr {
   createState() : InterpreterDispatchExprState { new InterpreterStaticDispatchExprState };
};

class InterpreterStaticDispatchExprState inherits InterpreterDispatchExprState {
   lookupMethod() : InterpreterMethod { method };
};

class InterpreterUnaryExpr inherits InterpreterExpr {
   type : InterpreterType;
   expr : InterpreterExpr;

   init(type_ : InterpreterType, expr_ : InterpreterExpr) : SELF_TYPE {{
      type <- type_;
      expr <- expr_;
      self;
   }};
};

class InterpreterUnaryExprState inherits InterpreterExprState {
   type : InterpreterType;
   value : InterpreterValue;

   init(type_ : InterpreterType) : SELF_TYPE {{
      type <- type_;
      self;
   }};

   addValue(value_ : InterpreterValue) : Object {
      value <- value_
   };
};

class InterpreterIsVoidExpr inherits InterpreterUnaryExpr {
   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(new InterpreterIsVoidExprState.init(type));
      expr.interpret(interpreter);
   }};
};

class InterpreterIsVoidExprState inherits InterpreterUnaryExprState {
   proceed(interpreter : Interpreter) : Bool {
      interpreter.proceedValue(new InterpreterBoolValue.init(type, isvoid value))
   };
};

class InterpreterComplementExpr inherits InterpreterUnaryExpr {
   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(new InterpreterComplementExprState.init(type));
      expr.interpret(interpreter);
   }};
};

class InterpreterComplementExprState inherits InterpreterUnaryExprState {
   proceed(interpreter : Interpreter) : Bool {
      interpreter.proceedValue(new InterpreterIntValue.init(type, case value of x : InterpreterIntValue => ~x.value(); esac))
   };
};

class InterpreterNotExpr inherits InterpreterUnaryExpr {
   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(new InterpreterNotExprState.init(type));
      expr.interpret(interpreter);
   }};
};

class InterpreterNotExprState inherits InterpreterUnaryExprState {
   proceed(interpreter : Interpreter) : Bool {
      interpreter.proceedValue(new InterpreterBoolValue.init(type, case value of x : InterpreterBoolValue => not x.value(); esac))
   };
};

class InterpreterBinaryExpr inherits InterpreterExpr {
   type : InterpreterType;
   left : InterpreterExpr;
   right : InterpreterExpr;

   init(type_ : InterpreterType, left_ : InterpreterExpr, right_ : InterpreterExpr) : SELF_TYPE {{
      type <- type_;
      left <- left_;
      right <- right_;
      self;
   }};

   newState() : InterpreterBinaryExprState {{
      new ObjectUtil.abortObject(self, "createState: unimplemented");
      let void : InterpreterBinaryExprState in void;
   }};

   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(newState().init(type, right));
      left.interpret(interpreter);
   }};
};

class InterpreterBinaryExprState inherits InterpreterExprState {
   type : InterpreterType;
   left : InterpreterValue;
   right : InterpreterExpr;
   result : InterpreterValue;

   init(type_ : InterpreterType, right_ : InterpreterExpr) : SELF_TYPE {{
      type <- type_;
      right <- right_;
      self;
   }};

   interpret(right : InterpreterValue) : InterpreterValue {{
      new ObjectUtil.abortObject(self, "interpret: unimplemented");
      let void : InterpreterValue in void;
   }};

   addValue(value : InterpreterValue) : Object {
      if isvoid left then
         left <- value
      else
         result <- interpret(value)
      fi
   };

   proceed(interpreter : Interpreter) : Bool {
      if isvoid result then
         right.interpret(interpreter)
      else
         interpreter.proceedValue(result)
      fi
   };
};

class InterpreterAddExpr inherits InterpreterBinaryExpr {
   newState() : InterpreterBinaryExprState {
      new InterpreterAddExprState
   };
};

class InterpreterAddExprState inherits InterpreterBinaryExprState {
   interpret(right : InterpreterValue) : InterpreterValue {
      let left : InterpreterIntValue <- case left of x : InterpreterIntValue => x; esac,
            right : InterpreterIntValue <- case right of x : InterpreterIntValue => x; esac in
         new InterpreterIntValue.init(type, left.value() + right.value())
   };
};

class InterpreterSubtractExpr inherits InterpreterBinaryExpr {
   newState() : InterpreterBinaryExprState {
      new InterpreterSubtractExprState
   };
};

class InterpreterSubtractExprState inherits InterpreterBinaryExprState {
   interpret(right : InterpreterValue) : InterpreterValue {
      let left : InterpreterIntValue <- case left of x : InterpreterIntValue => x; esac,
            right : InterpreterIntValue <- case right of x : InterpreterIntValue => x; esac in
         new InterpreterIntValue.init(type, left.value() - right.value())
   };
};

class InterpreterMultiplyExpr inherits InterpreterBinaryExpr {
   newState() : InterpreterBinaryExprState {
      new InterpreterMultiplyExprState
   };
};

class InterpreterMultiplyExprState inherits InterpreterBinaryExprState {
   interpret(right : InterpreterValue) : InterpreterValue {
      let left : InterpreterIntValue <- case left of x : InterpreterIntValue => x; esac,
            right : InterpreterIntValue <- case right of x : InterpreterIntValue => x; esac in
         new InterpreterIntValue.init(type, left.value() * right.value())
   };
};

class InterpreterDivideExpr inherits InterpreterExpr {
   line : Int;
   type : InterpreterType;
   left : InterpreterExpr;
   right : InterpreterExpr;

   init(line_ : Int, type_ : InterpreterType, left_ : InterpreterExpr, right_ : InterpreterExpr) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      left <- left_;
      right <- right_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(new InterpreterDivideExprState.init(line, type, right));
      left.interpret(interpreter);
   }};
};

class InterpreterDivideExprState inherits InterpreterExprState {
   line : Int;
   type : InterpreterType;
   left : InterpreterValue;
   rightExpr : InterpreterExpr;
   right : InterpreterValue;

   init(line_ : Int, type_ : InterpreterType, rightExpr_ : InterpreterExpr) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      rightExpr <- rightExpr_;
      self;
   }};

   addValue(value : InterpreterValue) : Object {
      if isvoid left then
         left <- value
      else
         right <- value
      fi
   };

   proceed(interpreter : Interpreter) : Bool {
      if isvoid right then
         rightExpr.interpret(interpreter)
      else
         let left : Int <- case left of x : InterpreterIntValue => x.value(); esac,
               right : Int <- case right of x : InterpreterIntValue => x.value(); esac in
            if right = 0 then
               interpreter.proceedError(line, "divide by 0")
            else
               interpreter.proceedValue(new InterpreterIntValue.init(type, left / right))
            fi
      fi
   };
};

class InterpreterLessExpr inherits InterpreterBinaryExpr {
   newState() : InterpreterBinaryExprState {
      new InterpreterLessExprState
   };
};

-- class InterpreterLessExprState is in interpreter-support*.cl

class InterpreterLessEqualExpr inherits InterpreterBinaryExpr {
   newState() : InterpreterBinaryExprState {
      new InterpreterLessEqualExprState
   };
};

-- class InterpreterLessEqualExprState is in interpreter-support*.cl

class InterpreterEqualExpr inherits InterpreterBinaryExpr {
   newState() : InterpreterBinaryExprState {
      new InterpreterEqualExprState
   };
};

class InterpreterEqualExprState inherits InterpreterBinaryExprState {
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
                     left.comparisonValue() = value.comparisonValue()
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

class InterpreterValueExpr inherits InterpreterExpr {
   value : InterpreterValue;

   init(value_ : InterpreterValue) : SELF_TYPE {{
      value <- value_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      interpreter.interpretValue(value)
   };

   toString() : String { value.toString() };
};

class InterpreterExitValueExprState inherits InterpreterExprState {
   interpreter : Interpreter;

   init(interpreter_ : Interpreter) : SELF_TYPE {{
      interpreter <- interpreter_;
      self;
   }};

   addValue(value : InterpreterValue) : Object {
      interpreter.addExitValue(value)
   };

   proceed(interpreter : Interpreter) : Bool {
      false
   };

   toString() : String { "exit" };
};

class Interpreter {
   debug : Bool;
   debug() : Bool { debug };

   debugDispatch : Bool <- debug;
   debugDispatch() : Bool { debugDispatch };

   initDebug(debug_ : StringMap) : SELF_TYPE {{
      debug <- not isvoid debug_.getWithString("interpreter");
      debugDispatch <- not isvoid debug_.getWithString("interpreter:dispatch");
      self;
   }};

   lineMap : TokenizerLineMap;

   io : ExtendedIO;
   io() : ExtendedIO { io };

   hasInput : Bool;
   hasInput() : Bool { hasInput };

   init(lineMap_ : TokenizerLineMap, io_ : ExtendedIO, hasInput_ : Bool) : SELF_TYPE {{
      lineMap <- lineMap_;
      io <- io_;
      hasInput <- hasInput_;
      self;
   }};

   uva : Bool;

   selfObject : InterpreterObjectValue;
   selfObject() : InterpreterObjectValue { selfObject };

   arguments : IntMap;
   arguments() : IntMap { arguments };

   vars : IntMap;
   vars() : IntMap { vars };

   initVars() : IntMap {
      if isvoid vars then
         vars <- new IntTreeMap
      else
         vars
      fi
   };

   numActivationRecords : Int;

   canPushActivationRecord() : Bool {
      if uva then
         (numActivationRecords + 1) < 1000
      else
         true
      fi
   };

   pushNewActivationRecord(selfObject_ : InterpreterObjectValue) : Bool {{
      selfObject <- selfObject_;

      numActivationRecords <- numActivationRecords + 1;
      if uva then
         numActivationRecords < 1000
      else
         true
      fi;
   }};

   popNewActivationRecord(savedSelfObject : InterpreterObjectValue) : Object {{
      selfObject <- savedSelfObject;
      numActivationRecords <- numActivationRecords - 1;
   }};

   pushDispatchContext(selfObject_ : InterpreterObjectValue, arguments_ : IntMap) : Object {{
      selfObject <- selfObject_;
      arguments <- arguments_;
      vars <- let void : IntMap in void;
      numActivationRecords <- numActivationRecords + 1;
   }};

   popDispatchContext(savedSelfObject : InterpreterObjectValue, savedArguments : IntMap, savedVars : IntMap) : Object {{
      selfObject <- savedSelfObject;
      arguments <- savedArguments;
      vars <- savedVars;
      numActivationRecords <- numActivationRecords - 1;
   }};

   value : InterpreterValue;
   exprState : InterpreterExprState <- new InterpreterExitValueExprState.init(self);
   exitValue : InterpreterValue;
   continue : Bool <- true;

   valueString(value : InterpreterValue) : String {
      if isvoid value then
         "void"
      else
         value.toString()
      fi
   };

   debugOutIndex : Int;

   debugOut(s : String) : Object {{
      io.out_string("DEBUG: interpreter: ").out_int(debugOutIndex)
            .out_string(" [").out_string(exprState.toString())
            .out_int(numActivationRecords).out_string(" ")
            .out_string("] ").out_string(s)
            .out_string("\n");
      debugOutIndex <- debugOutIndex + 1;
   }};

   interpretValue(value_ : InterpreterValue) : Bool {{
      if debug then
         debugOut("interpretValue: ".concat(valueString(value_)))
      else false fi;

      value <- value_;
      true;
   }};

   proceedValue(value_ : InterpreterValue) : Bool {{
      if debug then
         debugOut("proceedValue: ".concat(valueString(value_)))
      else false fi;

      exprState <- exprState.prev();
      value <- value_;
      true;
   }};

   proceedExpr(expr : InterpreterExpr) : Bool {{
      if debug then
         debugOut("proceedExpr ".concat(expr.toString()))
      else false fi;

      exprState <- exprState.prev();
      expr.interpret(self);
   }};

   proceedError(line : Int, value_ : String) : Bool {
      let stack : String in
         {
            if debug then
               debugOut("proceedError: [".concat(value_).concat("]"))
            else false fi;

            if uva then
               if 0 <= line then
                  stack <- new StringUtil.fromInt(line)
               else false fi
            else
               let exprState : InterpreterExprState <- exprState in
                  while not isvoid exprState loop
                     {
                        case exprState of
                           x : InterpreterNewExprState =>
                              {
                                 stack <- stack.concat(formatStackEntry(x.stackEntry(), line));
                                 line <- x.line();
                              };
                           x : InterpreterDispatchExprState =>
                              {
                                 if 0 < line then
                                    let stackEntry : String <- x.stackEntry() in
                                       if not stackEntry = "" then
                                          stack <- stack.concat(formatStackEntry(stackEntry, line))
                                       else false fi
                                 else false fi;
                                 line <- x.line();
                              };
                           x : Object => false;
                        esac;

                        exprState <- exprState.prev();
                     }
                  pool
            fi;

            exprState <- new InterpreterExitValueExprState.init(self);
            value <- new InterpreterErrorValue.init(value_, stack);
            true;
         }
   };

   formatStackEntry(stackEntry : String, line : Int) : String {
      "\tat ".concat(stackEntry).concat(" (").concat(lineMap.lineToString(line)).concat(")\n")
   };

   pushState(exprState_ : InterpreterExprState) : Bool {{
      if debug then
         debugOut("pushState: ".concat(exprState_.type_name()))
      else false fi;

      exprState_.setPrev(exprState);
      exprState <- exprState_;
      false;
   }};

   addExitValue(value : InterpreterValue) : Object {{
      if debug then
         debugOut("addExitValue: ".concat(valueString(value)))
      else false fi;

      exitValue <- value;
      continue <- false;
   }};

   pushExitState() : Bool {{
      if debug then
         debugOut("pushExitState")
      else false fi;

      exprState <- let void : InterpreterExprState in void;
      false;
   }};

   proceed() : Bool {{
      if debug then
         debugOut("proceed")
      else false fi;

      exprState.proceed(self);
   }};

   interpret(program : InterpreterProgram) : InterpreterValue {{
      uva <- program.uva();
      program.expr().interpret(self);

      while continue loop
         {
            while proceed() loop
               {
                  exprState.addValue(value);
                  value <- let void : InterpreterValue in void;
               }
            pool;
         }
      pool;

      if debug then
         debugOut("exit")
      else false fi;

      exitValue;
   }};
};
