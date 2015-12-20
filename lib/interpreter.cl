class InterpreterProgram {
   lineMap : TokenizerLineMap;
   expr : InterpreterExpr;

   init(lineMap_ : TokenizerLineMap, expr_ : InterpreterExpr) : SELF_TYPE {{
      lineMap <- lineMap_;
      expr <- expr_;
      self;
   }};

   interpret(io : IO) : InterpreterValue {
      let interpreter : Interpreter <- new Interpreter.init(lineMap, io) in
         {
            expr.interpret(interpreter);
            interpreter.interpret();
         }
   };
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

   defaultValue() : InterpreterValue { let void : InterpreterValue in void };

   attributeInits : LinkedList <- new LinkedList;
   attributeInits() : LinkedList { attributeInits };

   methods : IntMap <- new IntTreeMap;
   methods() : IntMap { methods };

   getMethod(index : Int) : InterpreterMethod {
      let method : Object <- methods.getWithInt(index) in
         if isvoid method then
            {
               new ObjectUtil.abortObject(self, "getMethod: type=".concat(name).concat(", index=").concat(new StringUtil.fromInt(index)));
               let void : InterpreterMethod in void;
            }
         else
            case method of x : InterpreterMethod => x; esac
         fi
   };

   init(name_ : String) : SELF_TYPE {{
      name <- name_;
      self;
   }};
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
      if interpreter.debug() then
         interpreter.debugOut("  method=".concat(toString())
               .concat(", expr=").concat(expr.toString()))
      else false fi;

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
      interpreter.proceedError(0, "abort")
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
      let target : InterpreterValue <- state.target() in
         case target of
            target : InterpreterObjectValue =>
               let copy : InterpreterObjectValue <- new InterpreterObjectValue.init(target.type()) in
                  {
                     copy.attributes().putAll(target.attributes());
                     interpreter.proceedValue(target);
                  };
            target : InterpreterValue => interpreter.proceedValue(target);
         esac
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
                  interpreter.io().out_string(s)
               else
                  let i : Int,
                        begin : Int in
                     {
                        while i < s.length() loop
                           if s.substr(i, 1) = backslash then
                              if s.substr(i + 1, 1) = backslash then
                                 {
                                    interpreter.io().out_string(s.substr(begin, i + 1 - begin));
                                    begin <- i + 2;
                                    i <- begin;
                                 }
                              else
                                 i <- i + 2
                              fi
                           else
                              i <- i + 1
                           fi
                        pool;

                        if begin < s.length() then
                           interpreter.io().out_string(s.substr(begin, s.length() - begin))
                        else false fi;
                     }
               fi;

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
   stringType : InterpreterType;

   initStringType(stringType_ : InterpreterType) : SELF_TYPE {{
      stringType <- stringType_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      interpreter.proceedValue(new InterpreterStringValue.init(stringType, interpreter.io().in_string(), 0))
   };
};

class InterpreterBasicIOInIntMethod inherits InterpreterMethod {
   intType : InterpreterType;

   initIntType(intType_ : InterpreterType) : SELF_TYPE {{
      intType <- intType_;
      self;
   }};

   interpret(interpreter : Interpreter, state : InterpreterDispatchExprState) : Bool {
      interpreter.proceedValue(new InterpreterIntValue.init(intType, interpreter.io().in_int()))
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
      nextMethodIndex <- inheritsType_.nextMethodIndex();

      let inheritsType : InterpreterType <- inheritsType_.type() in
         {
            type.setInheritsType(inheritsType);
            type.methods().putAll(inheritsType.methods());
         };
   }};

   definedFeatures : Bool;
   definedFeatures() : Bool { definedFeatures };
   setDefinedFeatures(definedFeatures_ : Bool) : Object { definedFeatures <- definedFeatures_ };

   analyzedAttributes : Bool;
   analyzedAttributes() : Bool { analyzedAttributes };
   setAnalyzedAttributes(analyzedAttributes_ : Bool) : Object { analyzedAttributes <- analyzedAttributes_ };

   attributes : StringMap <- new StringListMap;
   attributes() : StringMap { attributes };

   definedAttributes : Collection <- new LinkedList;
   definedAttributes() : Collection { definedAttributes };

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

   initBasic(name : String) : SELF_TYPE {{
      type <- new InterpreterType.init(name);
      definedFeatures <- true;
      analyzedAttributes <- true;
      self;
   }};

   init(analyzedType_ : AnalyzedType) : SELF_TYPE {{
      analyzedType <- analyzedType_;
      type <- new InterpreterType.init(analyzedType_.name());
      self;
   }};
};

class InterpreterAnalyzer inherits AnalyzedExprVisitor {
   lineMap : TokenizerLineMap;

   init(lineMap_ : TokenizerLineMap) : SELF_TYPE {{
      lineMap <- lineMap_;
      self;
   }};

   boolType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("Bool");
   defaultBoolValue : InterpreterBoolValue <- new InterpreterBoolValue.init(boolType.type(), false);

   intType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("Int");
   defaultIntValue : InterpreterIntValue <- new InterpreterIntValue.init(intType.type(), 0);

   stringType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("String");
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

   defineFeatures(type : InterpreterAnalyzerType) : Object {
      if not type.definedFeatures() then
         {
            type.setDefinedFeatures(true);

            let analyzedType : AnalyzedType <- type.analyzedType() in
               {
                  let inheritsType : InterpreterAnalyzerType <- getType(analyzedType.inheritsType()) in
                     {
                        defineFeatures(inheritsType);
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
               };
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
      if not type.analyzedAttributes() then
         {
            type.setAnalyzedAttributes(true);

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
               };
         }
      else false fi
   };

   analyze(program : AnalyzedProgram) : InterpreterProgram {{
      let objectType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("Object"),
            ioType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("IO") in
         {
            types.putWithString(objectType.name(), objectType);
            objectType.addBasicMethod("abort", new InterpreterBasicObjectAbortMethod);
            objectType.addBasicMethod("type_name", new InterpreterBasicObjectTypeNameMethod.initStringType(stringType.type()));
            objectType.addBasicMethod("copy", new InterpreterBasicObjectCopyMethod);

            types.putWithString(ioType.name(), ioType);
            ioType.setInheritsType(objectType);
            ioType.addBasicMethod("out_string", new InterpreterBasicIOOutStringMethod);
            ioType.addBasicMethod("out_int", new InterpreterBasicIOOutIntMethod);
            ioType.addBasicMethod("in_string", new InterpreterBasicIOInStringMethod.initStringType(stringType.type()));
            ioType.addBasicMethod("in_int", new InterpreterBasicIOInIntMethod.initIntType(intType.type()));

            types.putWithString(intType.name(), intType);
            intType.setInheritsType(objectType);

            types.putWithString(stringType.name(), stringType);
            stringType.setInheritsType(objectType);
            stringType.addBasicMethod("length", new InterpreterBasicStringLengthMethod.initIntType(intType.type()));
            stringType.addBasicMethod("concat", new InterpreterBasicStringConcatMethod.initIntType(stringType.type()));
            stringType.addBasicMethod("substr", new InterpreterBasicStringSubstrMethod.initBasic(stringType.type(), defaultStringValue));

            types.putWithString(boolType.name(), boolType);
            boolType.setInheritsType(objectType);
         };

      let typeList : Collection <- new LinkedList in
         {
            -- Create InterpreterAnalyzerType
            let analyzedTypeIter : StringMapIterator <- program.types().iterator() in
               while analyzedTypeIter.next() loop
                  let analyzedType : AnalyzedType <- case analyzedTypeIter.value() of x : AnalyzedType => x; esac,
                        type : InterpreterAnalyzerType <- new InterpreterAnalyzerType.init(analyzedType) in
                     -- Don't redefine basic types.
                     if isvoid types.putNewWithString(analyzedType.name(), type) then
                        typeList.add(type)
                     else false fi
               pool;

            -- Create attributes/methods.
            let typeIter : Iterator <- typeList.iterator() in
               while typeIter.next() loop
                  let type : InterpreterAnalyzerType <- case typeIter.get() of x : InterpreterAnalyzerType => x; esac in
                     defineFeatures(type)
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
               new InterpreterProgram.init(lineMap, analyzeExpr(dispatchExpr));
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

   visitLet(expr : AnalyzedLetExpr) : Object { new ObjectUtil.abortObject(self, "visitLet: unimplemented") };
   visitCase(expr : AnalyzedCaseExpr) : Object { new ObjectUtil.abortObject(self, "visitCase: unimplemented") };
   visitFormalAssignment(index : Int, expr : AnalyzedExpr) : Object { new ObjectUtil.abortObject(self, "visitFormalAssignment: unimplemented") };
   visitVarAssignment(index : Int, expr : AnalyzedExpr) : Object { new ObjectUtil.abortObject(self, "visitVarAssignment: unimplemented") };

   visitAttributeAssignment(attribute : AnalyzedAttribute, expr : AnalyzedExpr) : Object {
      new InterpreterAttributeAssignmentExpr.init(
            getType(attribute.containingType()).getAttribute(attribute.id()).index(),
            analyzeExpr(expr))
   };

   visitSelf() : Object {
      new InterpreterSelfExpr
   };

   visitFormal(index : Int) : Object { new ObjectUtil.abortObject(self, "visitFormal unimplemented") };
   visitVar(index : Int) : Object { new ObjectUtil.abortObject(self, "visitVar unimplemented") };

   visitAttribute(attribute : AnalyzedAttribute) : Object {
      new InterpreterAttributeExpr.init(
            getType(attribute.containingType()).getAttribute(attribute.id()).index(),
            getDefaultValue(attribute.type()))
   };

   visitNew(expr : AnalyzedNewExpr) : Object {
      let type : InterpreterAnalyzerType <- getType(expr.type()) in
         if type = boolType then
            new InterpreterConstantBoolExpr.init(boolType.type(), false)
         else
            if type = intType then
               new InterpreterConstantIntExpr.init(intType.type(), 0)
            else
               if type = stringType then
                  new InterpreterConstantStringExpr.init(stringType.type(), "", 0)
               else
                  let type : InterpreterType <- type.type() in
                     if type.attributeInits().size() = 0 then
                        new InterpreterSimpleNewExpr.init(type)
                     else
                        new InterpreterNewExpr.init(expr.line(), type)
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
         if op = "~" then
            new InterpreterComplementExpr.init(intType.type(), analyzeExpr(expr.expr()))
         else new ObjectUtil.abortObject(self, "visitUnary: unimplemented ".concat(op)) fi
   };

   visitBinary(expr : AnalyzedBinaryExpr) : Object {
      let left : InterpreterExpr <- analyzeExpr(expr.left()),
            right : InterpreterExpr <- analyzeExpr(expr.right()),
            op : String <- expr.op() in
         if op = "+" then
            new InterpreterAddExpr.init(intType.type(), left, right)
         else
            if op = "<" then
               new InterpreterLessExpr.init(boolType.type(), left, right)
            else new ObjectUtil.abortObject(self, "visitBinary: unimplemented".concat(op)) fi
         fi
   };

   visitConstantBool(expr : AnalyzedConstantBoolExpr) : Object {
      new InterpreterConstantBoolExpr.init(boolType.type(), expr.value())
   };

   visitConstantInt(expr : AnalyzedConstantIntExpr) : Object {
      new InterpreterConstantIntExpr.init(intType.type(), expr.value())
   };

   visitConstantString(expr : AnalyzedConstantStringExpr) : Object {
      new InterpreterConstantStringExpr.init(stringType.type(), expr.value(), expr.escapes())
   };
};

class InterpreterValue {
   type : InterpreterType;
   type() : InterpreterType { type };

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

   toString() : String { "error[".concat(value).concat("]") };
};

class InterpreterObjectValue inherits InterpreterValue {
   attributes : IntMap <- new IntTreeMap;
   attributes() : IntMap { attributes };

   init(type_ : InterpreterType) : SELF_TYPE {{
      type <- type_;
      self;
   }};

   toString() : String { "object[".concat(type().name()).concat("]") };
};

class InterpreterBoolValue inherits InterpreterValue {
   value : Bool;
   value() : Bool { value };

   init(type_ : InterpreterType, value_ : Bool) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};

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

   toString() : String { "string[".concat(value).concat("]") };
};

class InterpreterExprState {
   prev : InterpreterExprState;
   prev() : InterpreterExprState { prev };
   setPrev(prev_ : InterpreterExprState) : Object { prev <- prev_ };

   -- Must call proceedValue or pushState
   proceed(interpreter : Interpreter) : Bool { new ObjectUtil.abortBool(self, "proceed: unimplemented") };
   addValue(value : InterpreterValue) : Object { new ObjectUtil.abortObject(self, "addValue: unimplemented") };

   toString() : String { self.type_name() };
};

class InterpreterExpr {
   -- Must call interpretValue or pushState
   interpret(interpreter : Interpreter) : Bool { new ObjectUtil.abortBool(self, "interpret: unimplemented") };

   toString() : String { self.type_name() };
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

class InterpreterAttributeAssignmentExpr inherits InterpreterExpr {
   index : Int;
   expr : InterpreterExpr;

   init(index_ : Int, expr_ : InterpreterExpr) : SELF_TYPE {{
      index <- index_;
      expr <- expr_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(new InterpreterAttributeAssignmentExprState.init(index));
      expr.interpret(interpreter);
   }};

   toString() : String {
      "assignment.attribute[".concat(new StringUtil.fromInt(index)).concat("]")
   };
};

class InterpreterAttributeAssignmentExprState inherits InterpreterExprState {
   index : Int;
   value : InterpreterValue;

   init(index_ : Int) : SELF_TYPE {{
      index <- index_;
      self;
   }};

   addValue(value_ : InterpreterValue) : Object {
      value <- value_
   };

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

class InterpreterAttributeExpr inherits InterpreterExpr {
   index : Int;
   defaultValue : InterpreterValue;

   init(index_ : Int, defaultValue_ : InterpreterValue) : SELF_TYPE {{
      index <- index_;
      defaultValue <- defaultValue_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      let value : Object <- interpreter.selfObject().attributes().getWithInt(index) in
         if isvoid value then
            interpreter.interpretValue(defaultValue)
         else
            interpreter.interpretValue(case value of x : InterpreterValue => x; esac)
         fi
   };

   toString() : String {
      "attribute[".concat(new StringUtil.fromInt(index))
            .concat(if isvoid defaultValue then "" else ":".concat(defaultValue.toString()) fi)
            .concat("]")
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

   toString() : String { "new[".concat(type.name()).concat("]") };
};

class InterpreterNewExprState inherits InterpreterExprState {
   line : Int;
   line() : Int { line };

   type : InterpreterType;
   attrInitIter : Iterator;

   selfObject : InterpreterObjectValue;
   selfObjectAttributes : IntMap;
   attrInit : InterpreterAttributeInit;

   init(line_ : Int, type_ : InterpreterType, interpreter : Interpreter) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      attrInitIter <- type.attributeInits().iterator();

      selfObject <- new InterpreterObjectValue.init(type_);
      selfObjectAttributes <- selfObject.attributes();
      interpreter.setContext(selfObject);

      self;
   }};

   proceed(interpreter : Interpreter) : Bool {
      if attrInitIter.next() then
         {
            attrInit <- case attrInitIter.get() of x : InterpreterAttributeInit => x; esac;
            attrInit.expr().interpret(interpreter);
         }
      else
         interpreter.proceedValue(selfObject)
      fi
   };

   addValue(value : InterpreterValue) : Object {
      selfObjectAttributes.putWithInt(attrInit.index(), value)
   };

   stackEntry() : String {
      "new ".concat(selfObject.type().name())
   };

   toString() : String { "new[".concat(type.name()).concat("]") };
};

class InterpreterSimpleNewExpr inherits InterpreterExpr {
   type : InterpreterType;

   init(type_ : InterpreterType) : SELF_TYPE {{
      type <- type_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      interpreter.interpretValue(new InterpreterObjectValue.init(type))
   };

   toString() : String { "new.simple[".concat(type.name()).concat("]") };
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

   toString() : String { "dispatch[".concat(method.toString()).concat("]") };
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
      target.type().getMethod(method.index())
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
                  interpreter.debugOut("  target")
               else false fi;

               targetExpr.interpret(interpreter);
            }
         else
            if not hasResult then
               if isvoid target then
                  interpreter.proceedError(line, "dispatch on void for method '".concat(method.id())
                        .concat("' in type '").concat(method.containingType().name()).concat("'"))
               else
                  lookupMethod().interpret(interpreter, self)
               fi
            else
               {
                  if interpreter.debug() then
                     interpreter.debugOut("  result")
                  else false fi;

                  interpreter.setContext(savedSelfObject);
                  interpreter.proceedValue(result);
               }
            fi
         fi
      fi
   };

   interpretDispatch(interpreter : Interpreter, expr : InterpreterExpr) : Bool {{
      savedSelfObject <- interpreter.selfObject();
      interpreter.setContext(case target of x : InterpreterObjectValue => x; esac);

      expr.interpret(interpreter);
   }};

   stackEntry() : String {
      if not isvoid target then
         method.containingType().name().concat(".").concat(method.id())
      else "" fi
   };

   toString() : String { "dispatch" };
};

class InterpreterStaticDispatchExpr inherits InterpreterDispatchExpr {
   createState() : InterpreterDispatchExprState { new InterpreterStaticDispatchExprState };
};

class InterpreterStaticDispatchExprState inherits InterpreterDispatchExprState {
   lookupMethod() : InterpreterMethod { method };
};

class InterpreterComplementExpr inherits InterpreterExpr {
   type : InterpreterType;
   expr : InterpreterExpr;

   init(type_ : InterpreterType, expr_ : InterpreterExpr) : SELF_TYPE {{
      type <- type_;
      expr <- expr_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {{
      interpreter.pushState(new InterpreterComplementExprState.init(type));
      expr.interpret(interpreter);
   }};
};

class InterpreterComplementExprState inherits InterpreterExprState {
   type : InterpreterType;
   value : InterpreterIntValue;

   init(type_ : InterpreterType) : SELF_TYPE {{
      type <- type_;
      self;
   }};

   addValue(value_ : InterpreterValue) : Object {
      value <- case value_ of x : InterpreterIntValue => x; esac
   };

   proceed(interpreter : Interpreter) : Bool {
      interpreter.proceedValue(new InterpreterIntValue.init(type, ~value.value()))
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

class InterpreterLessExpr inherits InterpreterBinaryExpr {
   newState() : InterpreterBinaryExprState {
      new InterpreterLessExprState
   };
};

class InterpreterLessExprState inherits InterpreterBinaryExprState {
   interpret(right : InterpreterValue) : InterpreterValue {
      let left : InterpreterIntValue <- case left of x : InterpreterIntValue => x; esac,
            right : InterpreterIntValue <- case right of x : InterpreterIntValue => x; esac in
         new InterpreterBoolValue.init(type, left.value() < right.value())
   };
};

class InterpreterConstantBoolExpr inherits InterpreterExpr {
   type : InterpreterType;
   value : Bool;

   init(type_ : InterpreterType, value_ : Bool) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      interpreter.interpretValue(new InterpreterBoolValue.init(type, value))
   };

   toString() : String { if value then "true" else "false" fi };
};

class InterpreterConstantIntExpr inherits InterpreterExpr {
   type : InterpreterType;
   value : Int;

   init(type_ : InterpreterType, value_ : Int) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      interpreter.interpretValue(new InterpreterIntValue.init(type, value))
   };

   toString() : String { new StringUtil.fromInt(value) };
};

class InterpreterConstantStringExpr inherits InterpreterExpr {
   type : InterpreterType;
   value : String;
   escapes : Int;

   init(type_ : InterpreterType, value_ : String, escapes_ : Int) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      escapes <- escapes_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      interpreter.interpretValue(new InterpreterStringValue.init(type, value, escapes))
   };

   toString() : String { "string[".concat(value).concat("]") };
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

   lineMap : TokenizerLineMap;

   io : IO;
   io() : IO { io };

   init(lineMap_ : TokenizerLineMap, io_ : IO) : SELF_TYPE {{
      lineMap <- lineMap_;
      io <- io_;
      self;
   }};

   selfObject : InterpreterObjectValue;
   selfObject() : InterpreterObjectValue { selfObject };

   setContext(selfObject_ : InterpreterObjectValue) : Object {{
      selfObject <- selfObject_;
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

   debugOut(s : String) : Object {
      new IO.out_string("DEBUG: interpreter: [").out_string(exprState.toString())
            .out_string("] ").out_string(s)
            .out_string("\n")
   };

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
                              if not line = 0 then
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
               pool;

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

   steps : Int;

   proceed() : Bool {{
      steps <- steps + 1;
      if 100 <= steps then
         new ObjectUtil.abortObject(self, "maximum steps exceeded")
      else false fi;

      if debug then
         debugOut("proceed")
      else false fi;

      exprState.proceed(self);
   }};

   interpret() : InterpreterValue {{
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
