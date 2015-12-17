class InterpreterProgram {
   expr : InterpreterExpr;

   init(expr_ : InterpreterExpr) : SELF_TYPE {{
      expr <- expr_;
      self;
   }};

   interpret() : InterpreterValue {
      let interpreter : Interpreter <- new Interpreter in
         {
--new IO.out_string("\ninterpreter: begin\n");
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
   id : String;
   id() : String { id };

   index : Int;
   index() : Int { index };
   setIndex(index_ : Int) : Object { index <- index_ };

   expr : InterpreterExpr;
   expr() : InterpreterExpr { expr };
   setExpr(expr_ : InterpreterExpr) : Object { expr <- expr_ };

   init(id_ : String, index_ : Int) : SELF_TYPE {{
      id <- id_;
      index <- index_;
      self;
   }};
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
   id() : String { analyzedMethod.id() };

   method : InterpreterMethod;
   method() : InterpreterMethod { method };

   index() : Int { method.index() };
   setIndex(index_ : Int) : Object { method.setIndex(index_) };

   init(index_ : Int, analyzedMethod_ : AnalyzedMethod) : SELF_TYPE {{
      analyzedMethod <- analyzedMethod_;
      method <- new InterpreterMethod.init(analyzedMethod_.id(), index_);
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
      type.setInheritsType(inheritsType_.type());
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

   addAttribute(analyzedAttr : AnalyzedAttribute) : Object {
      let attr : InterpreterAnalyzerAttribute <- new InterpreterAnalyzerAttribute.init(attributes.size(), analyzedAttr) in
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

   addMethod(analyzedMethod : AnalyzedMethod) : Object {
      let method : InterpreterAnalyzerMethod <- new InterpreterAnalyzerMethod.init(methods.size(), analyzedMethod) in
         {
            let old : Object <- methods.putWithString(analyzedMethod.id(), method) in
               if not isvoid old then
                  method.setIndex(case old of x : InterpreterAnalyzerMethod => x.index(); esac)
               else false fi;

            type.methods().putWithInt(method.index(), method.method());
         }
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
   boolType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("Bool");
   intType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("Int");
   stringType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("String");

   types : StringMap <- new StringListMap;

   getType(type : AnalyzedType) : InterpreterAnalyzerType {
      case types.getWithString(type.name()) of x : InterpreterAnalyzerType => x; esac
   };

   defineFeatures(type : InterpreterAnalyzerType) : Object {
      let analyzedType : AnalyzedType <- type.analyzedType() in
         {
            let inheritsType : InterpreterAnalyzerType <- getType(analyzedType.inheritsType()) in
               {
                  if not inheritsType.definedFeatures() then
                     {
                        inheritsType.setDefinedFeatures(true);
                        defineFeatures(inheritsType);
                     }
                  else false fi;

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
   };

   analyzeExpr(expr : AnalyzedExpr) : InterpreterExpr {
      case expr.accept(self) of x : InterpreterExpr => x; esac
   };

   analyzeExprCollection(exprs : Collection) : Collection {
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
      let inheritsType : InterpreterAnalyzerType <- type.inheritsType() in
         {
            if not inheritsType.analyzedAttributes() then
               {
                  inheritsType.setAnalyzedAttributes(true);
                  analyzeAttributes(inheritsType);
               }
            else false fi;

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
   };

   analyze(program : AnalyzedProgram) : InterpreterProgram {{
      let objectType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("Object"),
            ioType : InterpreterAnalyzerType <- new InterpreterAnalyzerType.initBasic("IO") in
         {
            types.putWithString(objectType.name(), objectType);
            types.putWithString(ioType.name(), ioType);
            types.putWithString(intType.name(), intType);
            types.putWithString(stringType.name(), stringType);
            types.putWithString(boolType.name(), boolType);
            -- TODO: builtin methods
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
                  newExpr : AnalyzedExpr <- new AnalyzedNewExpr.init(mainType),
                  dispatchExpr : AnalyzedExpr <- new AnalyzedDispatchExpr.init(0, mainMethod.returnType(), newExpr, mainMethod, new Collection) in
               new InterpreterProgram.init(analyzeExpr(dispatchExpr));
         };
   }};

   visitBlock(expr : AnalyzedBlockExpr) : Object { new ObjectUtil.abortObject(self, "visitBlock: unimplemented") };
   visitIf(expr : AnalyzedIfExpr) : Object { new ObjectUtil.abortObject(self, "visitIf: unimplemented") };
   visitWhile(expr : AnalyzedWhileExpr) : Object { new ObjectUtil.abortObject(self, "visitWhile: unimplemented") };
   visitLet(expr : AnalyzedLetExpr) : Object { new ObjectUtil.abortObject(self, "visitLet: unimplemented") };
   visitCase(expr : AnalyzedCaseExpr) : Object { new ObjectUtil.abortObject(self, "visitCase: unimplemented") };
   visitAssignment(expr : AnalyzedAssignmentExpr) : Object { new ObjectUtil.abortObject(self, "visitAssignment: unimplemented") };
   visitObject(expr : AnalyzedObjectExpr) : Object { new ObjectUtil.abortObject(self, "visitObject: unimplemented") };

   visitNew(expr : AnalyzedNewExpr) : Object {
      let type : InterpreterType <- getType(expr.type()).type() in
         -- TODO: detect static inits - can't check attributeInits since it might not be populated yet
         --       class A { a : Object <- new A; };
         --    or class A { a : B <- new B; }; class B { b : Bool <- true; };
         if true then
            new InterpreterSimpleNewExpr.init(type)
         else
            new ObjectUtil.abortObject(self, "visitNew: unimplemented: attrInits")
         fi
   };

   visitDispatch(expr : AnalyzedDispatchExpr) : Object {
      -- TODO: static
      let method : AnalyzedMethod <- expr.method() in
         new InterpreterDispatchExpr.init(
               analyzeExprCollection(expr.arguments()),
               analyzeExpr(expr.expr()),
               getType(method.containingType()).getMethod(method.id()).method())
   };

   visitUnary(expr : AnalyzedUnaryExpr) : Object { new ObjectUtil.abortObject(self, "visitUnary: unimplemented") };
   visitBinary(expr : AnalyzedBinaryExpr) : Object { new ObjectUtil.abortObject(self, "visitBinary: unimplemented") };

   visitConstantBool(expr : AnalyzedConstantBoolExpr) : Object {
      new InterpreterConstantBoolExpr.init(boolType.type(), expr.value())
   };

   visitConstantInt(expr : AnalyzedConstantIntExpr) : Object {
      new InterpreterConstantIntExpr.init(intType.type(), expr.value())
   };

   visitConstantString(expr : AnalyzedConstantStringExpr) : Object {
      new InterpreterConstantStringExpr.init(stringType.type(), expr.value())
   };
};

class InterpreterValue {
   type : InterpreterType;
   type() : InterpreterType { type };
};

class InterpreterObjectValue inherits InterpreterValue {
   attributes : IntMap <- new IntTreeMap;
   attributes() : IntMap { attributes };

   init(type_ : InterpreterType) : SELF_TYPE {{
      type <- type_;
      self;
   }};
};

class InterpreterBoolValue inherits InterpreterValue {
   value : Bool;
   value() : Bool { value };

   init(type_ : InterpreterType, value_ : Bool) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};
};

class InterpreterIntValue inherits InterpreterValue {
   value : Int;
   value() : Int { value };

   init(type_ : InterpreterType, value_ : Int) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};
};

class InterpreterStringValue inherits InterpreterValue {
   value : String;
   value() : String { value };

   init(type_ : InterpreterType, value_ : String) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};
};

class InterpreterExprState {
   prev : InterpreterExprState;
   prev() : InterpreterExprState { prev };
   setPrev(prev_ : InterpreterExprState) : Object { prev <- prev_ };

   -- Must call proceedValue or pushState
   proceed(interpreter : Interpreter) : Bool { new ObjectUtil.abortBool(self, "proceed: unimplemented") };
   addValue(value : InterpreterValue) : Object { new ObjectUtil.abortObject(self, "addValue: unimplemented") };
};

class InterpreterExpr {
   -- Must call interpretValue or pushState
   interpret(interpreter : Interpreter) : Bool { new ObjectUtil.abortBool(self, "interpret: unimplemented") };
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
};

class InterpreterDispatchExpr inherits InterpreterExpr {
   arguments : Collection;
   target : InterpreterExpr;
   method : InterpreterMethod;

   init(arguments_ : Collection, target_ : InterpreterExpr, method_ : InterpreterMethod) : SELF_TYPE {{
      arguments <- arguments_;
      target <- target_;
      method <- method_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      interpreter.pushState(new InterpreterDispatchExprState.init(arguments, target, method))
   };
};

class InterpreterDispatchExprState inherits InterpreterExprState {
   numArgs : Int;
   argExprIter : Iterator;
   targetExpr : InterpreterExpr;
   method : InterpreterMethod;

   args : IntMap <- new IntTreeMap;
   hasTarget : Bool;
   target : InterpreterValue;
   hasResult : Bool;
   result : InterpreterValue;

   init(argExprs : Collection, targetExpr_ : InterpreterExpr, method_ : InterpreterMethod) : SELF_TYPE {{
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

   proceed(interpreter : Interpreter) : Bool {
      if argExprIter.next() then
            case argExprIter.get() of x : InterpreterExpr => x.interpret(interpreter); esac
      else
         if not hasTarget then
            targetExpr.interpret(interpreter)
         else
            if not hasResult then
               {
                  -- TODO: target void check
                  -- TODO: push self object
                  let method : InterpreterMethod <- target.type().getMethod(method.index()) in
                     method.expr().interpret(interpreter);
               }
            else
               {
                  interpreter.proceedValue(result);
                  -- TODO: pop self object
               }
            fi
         fi
      fi
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
};

class InterpreterConstantStringExpr inherits InterpreterExpr {
   type : InterpreterType;
   value : String;

   init(type_ : InterpreterType, value_ : String) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      interpreter.interpretValue(new InterpreterStringValue.init(type, value))
   };
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
};

class Interpreter {
   selfObject : InterpreterObjectValue;
   selfObject() : InterpreterObjectValue { selfObject };
   setSelfObject(selfObject_ : InterpreterObjectValue) : Object { selfObject <- selfObject_ };

   value : InterpreterValue;
   exprState : InterpreterExprState <- new InterpreterExitValueExprState.init(self);
   exitValue : InterpreterValue;
   continue : Bool <- true;

   interpretValue(value_ : InterpreterValue) : Bool {{
--new IO.out_string("interpreter: interpretValue state=").out_string(exprState.type_name()).out_string(", value=").out_string(if isvoid value_ then "void" else value_.type_name() fi).out_string("\n");
      value <- value_;
      true;
   }};

   proceedValue(value_ : InterpreterValue) : Bool {{
--new IO.out_string("interpreter: proceedValue state=").out_string(exprState.type_name()).out_string(", value=").out_string(if isvoid value_ then "void" else value_.type_name() fi).out_string("\n");
      exprState <- exprState.prev();
      value <- value_;
      true;
   }};

   pushState(exprState_ : InterpreterExprState) : Bool {{
      exprState_.setPrev(exprState);
      exprState <- exprState_;
--new IO.out_string("interpreter: pushState state=").out_string(exprState.type_name()).out_string("\n");
      false;
   }};

   addExitValue(value : InterpreterValue) : Object {{
--new IO.out_string("interpreter: addExitValue state=").out_string(exprState.type_name()).out_string(", value=").out_string(if isvoid value then "void" else value.type_name() fi).out_string("\n");
      exitValue <- value;
      continue <- false;
   }};

   pushExitState() : Bool {{
      exprState <- let void : InterpreterExprState in void;
--new IO.out_string("interpreter: pushExitState state=").out_string(exprState.type_name()).out_string("\n");
      false;
   }};

   steps : Int;

   proceed() : Bool {{
      steps <- steps + 1;
      if 100 <= steps then
         new ObjectUtil.abortObject(self, "maximum steps exceeded")
      else false fi;

--new IO.out_string("interpreter: - proceed state=").out_string(exprState.type_name()).out_string("\n");
      exprState.proceed(self);
   }};

   interpret() : InterpreterValue {{
      while continue loop
         {
            while proceed() loop
               {
--new IO.out_string("interpreter: - addValue state=").out_string(exprState.type_name()).out_string(", value=").out_string(if isvoid value then "void" else value.type_name() fi).out_string("\n");
                  exprState.addValue(value);
                  value <- let void : InterpreterValue in void;
               }
            pool;
         }
      pool;

--new IO.out_string("interpreter: - exit value=").out_string(if isvoid exitValue then "void" else exitValue.type_name() fi).out_string("\n");
      exitValue;
   }};
};
