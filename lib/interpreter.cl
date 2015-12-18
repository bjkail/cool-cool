class InterpreterProgram {
   lineMap : TokenizerLineMap;
   expr : InterpreterExpr;

   init(lineMap_ : TokenizerLineMap, expr_ : InterpreterExpr) : SELF_TYPE {{
      lineMap <- lineMap_;
      expr <- expr_;
      self;
   }};

   interpret() : InterpreterValue {
      let interpreter : Interpreter <- new Interpreter.init(lineMap) in
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

   toString() : String {
      containingType.name()
            .concat(".").concat(id())
            .concat(":").concat(new StringUtil.fromInt(index))
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
   id() : String { analyzedMethod.id() };

   method : InterpreterMethod;
   method() : InterpreterMethod { method };

   index() : Int { method.index() };
   setIndex(index_ : Int) : Object { method.setIndex(index_) };

   init(containingType : InterpreterType, index_ : Int, analyzedMethod_ : AnalyzedMethod) : SELF_TYPE {{
      analyzedMethod <- analyzedMethod_;
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

   nextMethodIndex : Int;
   nextMethodIndex() : Int { nextMethodIndex };

   addMethod(analyzedMethod : AnalyzedMethod) : Object {
      let id : String <- analyzedMethod.id(),
            index : Int <-
               let old : InterpreterAnalyzerMethod <- inheritsType.getMethod(analyzedMethod.id()) in
                  if isvoid old then
                     let index : Int <- nextMethodIndex in
                        {
                           nextMethodIndex <- index + 1;
                           index;
                        }
                  else
                     old.index()
                  fi,
            method : InterpreterAnalyzerMethod <- new InterpreterAnalyzerMethod.init(type, index, analyzedMethod) in
         {
            methods.putWithString(id, method);
            type.methods().putWithInt(index, method.method());
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
   defaultStringValue : InterpreterStringValue <- new InterpreterStringValue.init(stringType.type(), "");

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
                  dispatchExpr : AnalyzedExpr <- new AnalyzedDispatchExpr.init(0, mainMethod.returnType(), newExpr, mainMethod, true, new Collection) in
               new InterpreterProgram.init(lineMap, analyzeExpr(dispatchExpr));
         };
   }};

   visitBlock(expr : AnalyzedBlockExpr) : Object { new ObjectUtil.abortObject(self, "visitBlock: unimplemented") };
   visitIf(expr : AnalyzedIfExpr) : Object { new ObjectUtil.abortObject(self, "visitIf: unimplemented") };
   visitWhile(expr : AnalyzedWhileExpr) : Object { new ObjectUtil.abortObject(self, "visitWhile: unimplemented") };
   visitLet(expr : AnalyzedLetExpr) : Object { new ObjectUtil.abortObject(self, "visitLet: unimplemented") };
   visitCase(expr : AnalyzedCaseExpr) : Object { new ObjectUtil.abortObject(self, "visitCase: unimplemented") };
   visitFormalAssignment(index : Int, expr : AnalyzedExpr) : Object { new ObjectUtil.abortObject(self, "visitFormalAssignment: unimplemented") };
   visitVarAssignment(index : Int, expr : AnalyzedExpr) : Object { new ObjectUtil.abortObject(self, "visitVarAssignment: unimplemented") };
   visitAttributeAssignment(attribute : AnalyzedAttribute, expr : AnalyzedExpr) : Object { new ObjectUtil.abortObject(self, "visitAttributeAssignment: unimplemented") };

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
      let method : AnalyzedMethod <- expr.method(),
            dispatchExpr : InterpreterDispatchExpr <-
               if expr.static() then
                  new InterpreterStaticDispatchExpr
               else
                  new InterpreterDispatchExpr
               fi in
         dispatchExpr.init(
               expr.line(),
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

   init(type_ : InterpreterType, value_ : String) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};

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

class InterpreterSelfExpr inherits InterpreterExpr {
   interpret(interpreter : Interpreter) : Bool {
      interpreter.interpretValue(interpreter.selfObject())
   };

   toString() : String { "self" };
};

class InterpreterAttributeExpr inherits InterpreterExpr {
   index : Int;
   defaultValue : InterpreterValue;

   init(index_ : Int, defaultValue : InterpreterValue) : SELF_TYPE {{
      index <- index_;
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

   interpret(interpreter : Interpreter) : Bool {
      interpreter.pushState(new InterpreterDispatchExprState.init(line, arguments, target, method))
   };

   toString() : String { "dispatch[".concat(method.toString()).concat("]") };
};

class InterpreterStaticDispatchExpr inherits InterpreterDispatchExpr {
   interpret(interpreter : Interpreter) : Bool {
      interpreter.pushState(new InterpreterStaticDispatchExprState.init(line, arguments, target, method))
   };
};

class InterpreterDispatchExprState inherits InterpreterExprState {
   line : Int;
   line() : Int { line };

   numArgs : Int;
   argExprIter : Iterator;
   targetExpr : InterpreterExpr;
   method : InterpreterMethod;

   args : IntMap <- new IntTreeMap;
   hasTarget : Bool;
   target : InterpreterValue;
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
                  {
                     savedSelfObject <- interpreter.selfObject();
                     interpreter.setContext(case target of x : InterpreterObjectValue => x; esac);

                     let method : InterpreterMethod <- lookupMethod() in
                        {
                           if interpreter.debug() then
                              interpreter.debugOut("  method=".concat(method.toString())
                                    .concat(", expr=").concat(method.expr().toString()))
                           else false fi;

                           method.expr().interpret(interpreter);
                        };
                  }
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

   stackEntry() : String {
      if not isvoid target then
         method.containingType().name().concat(".").concat(method.id())
      else "" fi
   };

   toString() : String { "dispatch" };
};

class InterpreterStaticDispatchExprState inherits InterpreterDispatchExprState {
   lookupMethod() : InterpreterMethod {
      method
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

   init(type_ : InterpreterType, value_ : String) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};

   interpret(interpreter : Interpreter) : Bool {
      interpreter.interpretValue(new InterpreterStringValue.init(type, value))
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

   init(lineMap_ : TokenizerLineMap) : SELF_TYPE {{
      lineMap <- lineMap_;
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
         debugOut("interpretValue: ".concat(valueString(value_)))
      else false fi;

      exprState <- exprState.prev();
      value <- value_;
      true;
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
                        x : InterpreterDispatchExprState =>
                           {
                              let stackEntry : String <- x.stackEntry() in
                                 if not stackEntry = "" then
                                    stack <- stack.concat("\tat ").concat(stackEntry)
                                       .concat(" (").concat(lineMap.lineToString(line))
                                       .concat(")\n")
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

   formatStackEntry(s : String, line : Int) : String {
      "\tat ".concat(s).concat(" (").concat(lineMap.lineToString(line)).concat(")\n")
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
