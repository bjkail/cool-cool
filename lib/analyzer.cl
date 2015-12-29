class AnalyzedProgram {
   types : StringMap;
   types() : StringMap { types };

   getType(name : String) : AnalyzedType {
      let type : Object <- types.getWithString(name) in
         if isvoid type then
            let void : AnalyzedType in void
         else
            case type of x : AnalyzedType => x; esac
         fi
   };

   mainMethod : AnalyzedMethod;
   mainMethod() : AnalyzedMethod { mainMethod };

   init(types_ : StringMap, mainMethod_ : AnalyzedMethod) : SELF_TYPE {{
      types <- types_;
      mainMethod <- mainMethod_;
      self;
   }};
};

class AnalyzedType {
   name : String;
   name() : String { name };
   isSelfType() : Bool { name = "SELF_TYPE" };

   parsedClass : ParsedClass;
   parsedClass() : ParsedClass { parsedClass };
   unsetParsedClass() : Object { parsedClass <- let void : ParsedClass in void };

   inheritsType : AnalyzedType;
   inheritsType() : AnalyzedType { inheritsType };
   setInheritsType(inheritsType_ : AnalyzedType) : Object { inheritsType <- inheritsType_ };
   inheritsType2() : AnalyzedType {
      if isvoid inheritsType then
         inheritsType
      else
         inheritsType.inheritsType()
      fi
   };

   selfTypeType : AnalyzedType;
   selfTypeType() : AnalyzedType { selfTypeType };

   inheritsDepth : Int;
   inheritsDepth() : Int { inheritsDepth };

   selfTypeTarget() : AnalyzedType {
      inheritsType
   };

   conformsTo(type : AnalyzedType) : Bool {
      let inheritsType : AnalyzedType <- self in
         {
            while if not isvoid inheritsType then
                  not inheritsType = type
               else false
            fi loop
               inheritsType <- inheritsType.inheritsType()
            pool;

            inheritsType = type;
         }
   };

   join(type2 : AnalyzedType) : AnalyzedType {
      if self = type2 then
         self
      else
         let type1 : AnalyzedType <- self in
            {
               if type1.inheritsDepth() < type2.inheritsDepth() then
                  {
                     type2 <- type2.inheritsType();
                     while type1.inheritsDepth() < type2.inheritsDepth() loop
                        type2 <- type2.inheritsType()
                     pool;
                  }
               else
                  if type2.inheritsDepth() < type1.inheritsDepth() then
                     {
                        type1 <- type1.inheritsType();
                        while type2.inheritsDepth() < type1.inheritsDepth() loop
                           type1 <- type1.inheritsType()
                        pool;
                     }
                  else false fi
               fi;

               while not type1 = type2 loop
                  {
                     type1 <- type1.inheritsType();
                     type2 <- type2.inheritsType();
                  }
               pool;

               type1;
            }
      fi
   };

   definedFeatures : Collection <- new LinkedList;
   definedFeatures() : Collection { definedFeatures };

   addDefinedFeature(feature : AnalyzedFeature) : Object {
      definedFeatures.add(feature)
   };

   attributes : StringMap;
   attributes() : StringMap { attributes };

   getAttribute(id : String) : AnalyzedAttribute {
      let attr : Object <- attributes.getWithString(id) in
         if isvoid attr then
            let void : AnalyzedAttribute in void
         else
            case attr of x : AnalyzedAttribute => x; esac
         fi
   };

   addAttribute(attr : AnalyzedAttribute) : AnalyzedAttribute {
      let old : Object <- attributes.putNewWithString(attr.id(), attr) in
         if isvoid old then
            {
               addDefinedFeature(attr);
               let void : AnalyzedAttribute in void;
            }
         else
            case old of x : AnalyzedAttribute => x; esac
         fi
   };

   methods : StringMap;
   methods() : StringMap { methods };

   getMethod(id : String) : AnalyzedMethod {
      let method : Object <- methods.getWithString(id) in
         if isvoid method then
            let void : AnalyzedMethod in void
         else
            case method of x : AnalyzedMethod => x; esac
         fi
   };

   addMethod(method : AnalyzedMethod) : AnalyzedMethod {
      let old : Object <- methods.putNewWithString(method.id(), method) in
         if isvoid old then
            {
               addDefinedFeature(method);
               let void : AnalyzedMethod in void;
            }
         else
            case old of x : AnalyzedMethod => x; esac
         fi
   };

   addMethodOverride(method : AnalyzedMethod) : Object {{
      methods.putWithString(method.id(), method);
      addDefinedFeature(method);
   }};

   processInherits() : Object {{
      inheritsDepth <- inheritsType.inheritsDepth() + 1;
      attributes <- inheritsType.attributes().copy();
      methods <- new StringListMap;
      methods.putAll(inheritsType.methods());
      selfTypeType <- new AnalyzedType.initSelfType(self);
   }};

   initBuiltinObject() : SELF_TYPE {{
      name <- "Object";
      attributes <- new StringListMap;
      methods <- new StringListMap;
      selfTypeType <- new AnalyzedType.initSelfType(self);
      self;
   }};

   initBuiltin(name_ : String, inheritsType_ : AnalyzedType) : SELF_TYPE {{
      name <- name_;
      inheritsType <- inheritsType_;
      self;
   }};

   initSelfType(type : AnalyzedType) : SELF_TYPE {{
      name <- "SELF_TYPE";
      inheritsType <- type;
      inheritsDepth <- type.inheritsDepth() + 1;
      methods <- inheritsType.methods();
      self;
   }};

   init(parsedClass_ : ParsedClass) : SELF_TYPE {{
      parsedClass <- parsedClass_;
      name <- parsedClass_.type();
      self;
   }};

   initError(name_ : String) : SELF_TYPE {{
      name <- name_;
      methods <- new StringListMap;
      self;
   }};
};

class AnalyzedSelfType inherits AnalyzedType {
   getAttribute(id : String) : AnalyzedAttribute {{
      new ObjectUtil.abortObject(self, "getAttribute: unsupported");
      let void : AnalyzedAttribute in void;
   }};

   getMethod(id : String) : AnalyzedMethod {{
      new ObjectUtil.abortObject(self, "getMethod unsupported");
      let void : AnalyzedMethod in void;
   }};
};

class AnalyzedFeature {
   containingType : AnalyzedType;
   containingType() : AnalyzedType { containingType };

   id : String;
   id() : String { id };

   unsetParsedFeature() : Object { new ObjectUtil.abortObject(self, "unsetParsedFeature: unimplemented") };

   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };
   setExpr(expr_ : AnalyzedExpr) : Object { expr <- expr_ };

   asAttribute() : AnalyzedAttribute { let void : AnalyzedAttribute in void };
   asMethod() : AnalyzedMethod { let void : AnalyzedMethod in void };
};

class AnalyzedAttribute inherits AnalyzedFeature {
   parsedAttribute : ParsedAttribute;
   parsedAttribute() : ParsedAttribute { parsedAttribute };
   unsetParsedFeature() : Object { parsedAttribute <- let void : ParsedAttribute in void };

   type : AnalyzedType;
   type() : AnalyzedType { type };

   init(containingType_ : AnalyzedType, parsedAttribute_ : ParsedAttribute, type_ : AnalyzedType) : SELF_TYPE {{
      containingType <- containingType_;
      parsedAttribute <- parsedAttribute_;
      id <- parsedAttribute_.id();
      type <- type_;
      self;
   }};

   asAttribute() : AnalyzedAttribute { self };
};

class AnalyzedMethod inherits AnalyzedFeature {
   parsedMethod : ParsedMethod;
   parsedMethod() : ParsedMethod { parsedMethod };
   unsetParsedFeature() : Object { parsedMethod <- let void : ParsedMethod in void };

   returnType : AnalyzedType;
   returnType() : AnalyzedType { returnType };

   formalTypes : Collection;
   formalTypes() : Collection { formalTypes };

   initBuiltin(containingType_ : AnalyzedType, id_ : String, formalTypes_ : Collection, returnType_ : AnalyzedType) : SELF_TYPE {{
      containingType <- containingType_;
      id <- id_;
      formalTypes <- formalTypes_;
      returnType <- returnType_;
      self;
   }};

   init(containingType : AnalyzedType, parsedMethod_ : ParsedMethod, formalTypes : Collection, returnType : AnalyzedType) : SELF_TYPE {{
      parsedMethod <- parsedMethod_;
      initBuiltin(containingType, parsedMethod_.id(), formalTypes, returnType);
   }};

   toString() : String { containingType().name().concat(".").concat(id()) };

   asMethod() : AnalyzedMethod { self };
};

class AnalyzedObject {
   type : AnalyzedType;
   type() : AnalyzedType { type };

   acceptAssignment(visitor : AnalyzedExprVisitor, expr : AnalyzedExpr) : Object { new ObjectUtil.abortObject(self, "acceptAssignment: unimplemented") };
   accept(visitor : AnalyzedExprVisitor) : Object { new ObjectUtil.abortObject(self, "accept: unimplemented") };
};

class AnalyzedSelfObject inherits AnalyzedObject {
   init(type_ : AnalyzedType) : SELF_TYPE {{
      type <- type_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitSelf(self) };
};

class AnalyzedArgumentObject inherits AnalyzedObject {
   index : Int;
   index() : Int { index };

   init(type_ : AnalyzedType, index_ : Int) : SELF_TYPE {{
      type <- type_;
      index <- index_;
      self;
   }};

   acceptAssignment(visitor : AnalyzedExprVisitor, expr : AnalyzedExpr) : Object { visitor.visitArgumentAssignment(self, expr) };
   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitArgument(self) };
};

class AnalyzedVarObject inherits AnalyzedObject {
   index : Int;
   index() : Int { index };

   init(type_ : AnalyzedType, index_ : Int) : SELF_TYPE {{
      type <- type_;
      index <- index_;
      self;
   }};

   acceptAssignment(visitor : AnalyzedExprVisitor, expr : AnalyzedExpr) : Object { visitor.visitVarAssignment(self, expr) };
   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitVar(self) };
};

class AnalyzedAttributeObject inherits AnalyzedObject {
   attribute : AnalyzedAttribute;
   attribute() : AnalyzedAttribute { attribute };

   init(type_ : AnalyzedType, attribute_ : AnalyzedAttribute) : SELF_TYPE {{
      type <- type_;
      attribute <- attribute_;
      self;
   }};

   acceptAssignment(visitor : AnalyzedExprVisitor, expr : AnalyzedExpr) : Object { visitor.visitAttributeAssignment(self, expr) };
   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitAttribute(self) };
};

class AnalyzedExprVisitor {
   visitBlock(expr : AnalyzedBlockExpr) : Object { new ObjectUtil.abortObject(self, "visitBlock: unimplemented") };
   visitIf(expr : AnalyzedIfExpr) : Object { new ObjectUtil.abortObject(self, "visitIf: unimplemented") };
   visitWhile(expr : AnalyzedWhileExpr) : Object { new ObjectUtil.abortObject(self, "visitWhile: unimplemented") };
   visitLet(expr : AnalyzedLetExpr) : Object { new ObjectUtil.abortObject(self, "visitLet: unimplemented") };
   visitCase(expr : AnalyzedCaseExpr) : Object { new ObjectUtil.abortObject(self, "visitCase: unimplemented") };
   visitArgumentAssignment(object : AnalyzedArgumentObject, expr : AnalyzedExpr) : Object { new ObjectUtil.abortObject(self, "visitArgumentAssignment: unimplemented") };
   visitVarAssignment(object : AnalyzedVarObject, expr : AnalyzedExpr) : Object { new ObjectUtil.abortObject(self, "visitVarAssignment: unimplemented") };
   visitAttributeAssignment(attribute : AnalyzedAttributeObject, expr : AnalyzedExpr) : Object { new ObjectUtil.abortObject(self, "visitAttributeAssignment: unimplemented") };
   visitSelf(object : AnalyzedSelfObject) : Object { new ObjectUtil.abortObject(self, "visitSelf unimplemented") };
   visitArgument(object : AnalyzedArgumentObject) : Object { new ObjectUtil.abortObject(self, "visitArgument unimplemented") };
   visitVar(object : AnalyzedVarObject) : Object { new ObjectUtil.abortObject(self, "visitVar unimplemented") };
   visitAttribute(object : AnalyzedAttributeObject) : Object { new ObjectUtil.abortObject(self, "visitAttribute unimplemented") };
   visitNew(expr : AnalyzedNewExpr) : Object { new ObjectUtil.abortObject(self, "visitNew: unimplemented") };
   visitDispatch(expr : AnalyzedDispatchExpr) : Object { new ObjectUtil.abortObject(self, "visitDispatch: unimplemented") };
   visitUnary(expr : AnalyzedUnaryExpr) : Object { new ObjectUtil.abortObject(self, "visitUnary: unimplemented") };
   visitBinary(expr : AnalyzedBinaryExpr) : Object { new ObjectUtil.abortObject(self, "visitBinary: unimplemented") };
   visitConstantBool(expr : AnalyzedConstantBoolExpr) : Object { new ObjectUtil.abortObject(self, "visitConstantBool: unimplemented") };
   visitConstantInt(expr : AnalyzedConstantIntExpr) : Object { new ObjectUtil.abortObject(self, "visitConstantInt: unimplemented") };
   visitConstantString(expr : AnalyzedConstantStringExpr) : Object { new ObjectUtil.abortObject(self, "visitConstantString: unimplemented") };
};

class AnalyzedExpr {
   type : AnalyzedType;
   type() : AnalyzedType { type };

   accept(visitor : AnalyzedExprVisitor) : Object { new ObjectUtil.abortObject(self, "accept: unimplemented") };
};

class AnalyzedBlockExpr inherits AnalyzedExpr {
   exprs : Collection;
   exprs() : Collection { exprs };

   init(type_ : AnalyzedType, exprs_ : Collection) : SELF_TYPE {{
      type <- type_;
      exprs <- exprs_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitBlock(self) };
};

class AnalyzedIfExpr inherits AnalyzedExpr {
   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   then_ : AnalyzedExpr;
   then_() : AnalyzedExpr { then_ };

   else_ : AnalyzedExpr;
   else_() : AnalyzedExpr { else_ };

   init(type_ : AnalyzedType, expr_ : AnalyzedExpr, then__ : AnalyzedExpr, else__ : AnalyzedExpr) : SELF_TYPE {{
      type <- type_;
      expr <- expr_;
      then_ <- then__;
      else_ <- else__;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitIf(self) };
};

class AnalyzedWhileExpr inherits AnalyzedExpr {
   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   loop_ : AnalyzedExpr;
   loop_() : AnalyzedExpr { loop_ };

   init(type_ : AnalyzedType, expr_ : AnalyzedExpr, loop__ : AnalyzedExpr) : SELF_TYPE {{
      type <- type_;
      expr <- expr_;
      loop_ <- loop__;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitWhile(self) };
};

class AnalyzedLetVar {
   object : AnalyzedVarObject;
   object() : AnalyzedVarObject { object };

   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   init(object_ : AnalyzedVarObject, expr_ : AnalyzedExpr) : SELF_TYPE {{
      object <- object_;
      expr <- expr_;
      self;
   }};
};

class AnalyzedLetExpr inherits AnalyzedExpr {
   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   vars : Collection;
   vars() : Collection { vars };

   init(type_ : AnalyzedType, expr_ : AnalyzedExpr, vars_ : Collection) : SELF_TYPE {{
      type <- type_;
      expr <- expr_;
      vars <- vars_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitLet(self) };
};

class AnalyzedCaseBranch {
   checkType : AnalyzedType;
   checkType() : AnalyzedType { checkType };

   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   init(checkType_ : AnalyzedType, expr_ : AnalyzedExpr) : SELF_TYPE {{
      checkType <- checkType_;
      expr <- expr_;
      self;
   }};
};

class AnalyzedCaseExpr inherits AnalyzedExpr {
   line : Int;
   line() : Int { line };

   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   varIndex : Int;
   varIndex() : Int { varIndex };

   branches : Collection;
   branches() : Collection { branches };

   init(line_ : Int, type_ : AnalyzedType, expr_ : AnalyzedExpr, varIndex_ : Int, branches_ : Collection) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      expr <- expr_;
      varIndex <- varIndex_;
      branches <- branches_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitCase(self) };
};

class AnalyzedAssignmentExpr inherits AnalyzedExpr {
   object : AnalyzedObject;
   object() : AnalyzedObject { object };

   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   init(type_ : AnalyzedType, object_ : AnalyzedObject, expr_ : AnalyzedExpr) : SELF_TYPE {{
      type <- type_;
      object <- object_;
      expr <- expr_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { object.acceptAssignment(visitor, expr) };
};

class AnalyzedObjectExpr inherits AnalyzedExpr {
   object : AnalyzedObject;
   object() : AnalyzedObject { object };

   init(type_ : AnalyzedType, object_ : AnalyzedObject) : SELF_TYPE {{
      type <- type_;
      object <- object_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object {
      object.accept(visitor)
   };
};

class AnalyzedNewExpr inherits AnalyzedExpr {
   line : Int;
   line() : Int { line };

   init(line_ : Int, type_ : AnalyzedType) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitNew(self) };
};

class AnalyzedDispatchExpr inherits AnalyzedExpr {
   line : Int;
   line() : Int { line };

   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   method : AnalyzedMethod;
   method() : AnalyzedMethod { method };

   static : Bool;
   static() : Bool { static };

   arguments : Collection;
   arguments() : Collection { arguments };

   init(line_ : Int, type_ : AnalyzedType, expr_ : AnalyzedExpr, method_ : AnalyzedMethod, static_ : Bool, arguments_ : Collection) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      expr <- expr_;
      method <- method_;
      static <- static_;
      arguments <- arguments_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitDispatch(self) };
};

class AnalyzedUnaryExpr inherits AnalyzedExpr {
   op : String;
   op() : String { op };

   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   init(type_ : AnalyzedType, op_ : String, expr_ : AnalyzedExpr) : SELF_TYPE {{
      type <- type_;
      op <- op_;
      expr <- expr_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitUnary(self) };
};

class AnalyzedBinaryExpr inherits AnalyzedExpr {
   line : Int;
   line() : Int { line };

   op : String;
   op() : String { op };

   left : AnalyzedExpr;
   left() : AnalyzedExpr { left };

   right : AnalyzedExpr;
   right() : AnalyzedExpr { right };

   method : AnalyzedMethod;
   method() : AnalyzedMethod { method };

   arguments : Collection;
   arguments() : Collection { arguments };

   init(line_ : Int, type_ : AnalyzedType, op_ : String, left_ : AnalyzedExpr, right_ : AnalyzedExpr) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      op <- op_;
      left <- left_;
      right <- right_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitBinary(self) };
};

class AnalyzedConstantBoolExpr inherits AnalyzedExpr {
   value : Bool;
   value() : Bool { value };

   init(type_ : AnalyzedType, value_ : Bool) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitConstantBool(self) };
};

class AnalyzedConstantIntExpr inherits AnalyzedExpr {
   value : Int;
   value() : Int { value };

   init(type_ : AnalyzedType, value_ : Int) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitConstantInt(self) };
};

class AnalyzedConstantStringExpr inherits AnalyzedExpr {
   value : String;
   value() : String { value };

   -- The number of literal two-character escape sequences used in this string
   -- (e.g., "\n" actually stored as "\\n").  In UVA mode, the number of "\t"
   -- and "\n" escape sequences (the sequences handled by IO.out_string).
   escapes : Int;
   escapes() : Int { escapes };

   init(type_ : AnalyzedType, value_ : String, escapes_ : Int) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      escapes <- escapes_;
      self;
   }};

   accept(visitor : AnalyzedExprVisitor) : Object { visitor.visitConstantString(self) };
};

class AnalyzedTypeEnv inherits ParsedExprVisitor {
   analyzer : Analyzer;
   analyzer() : Analyzer { analyzer };

   containingType : AnalyzedType;
   containingType() : AnalyzedType { containingType };

   parent : AnalyzedTypeEnv;
   bindings : StringMap <- new StringListMap;

   varIndex : Int;
   varIndex() : Int { varIndex };

   initContainingType(analyzed_ : Analyzer, containingType_ : AnalyzedType) : SELF_TYPE {{
      analyzer <- analyzed_;
      containingType <- containingType_;
      put("self", new AnalyzedSelfObject.init(containingType.selfTypeType()));
      self;
   }};

   init(parent_ : AnalyzedTypeEnv) : SELF_TYPE {{
      analyzer <- parent_.analyzer();
      varIndex <- parent_.varIndex();
      containingType <- parent_.containingType();
      parent <- parent_;
      self;
   }};

   put(id : String, object : AnalyzedObject) : Object {
      bindings.putWithString(id, object)
   };

   putVar(id : String, type : AnalyzedType) : AnalyzedVarObject {
      let object : AnalyzedVarObject <- new AnalyzedVarObject.init(type, varIndex) in
         {
            put(id, object);
            varIndex <- varIndex + 1;
            object;
         }
   };

   get(id : String) : AnalyzedObject {
      let object : Object <- bindings.getWithString(id) in
         if isvoid object then
            if isvoid parent then
               let void : AnalyzedObject in void
            else
               parent.get(id)
            fi
         else
            case object of x : AnalyzedObject => x; esac
         fi
   };

   analyze(expr : ParsedExpr) : AnalyzedExpr {
      case expr.accept(self) of x : AnalyzedExpr => x; esac
   };

   visitBlock(parsedExpr : ParsedBlockExpr) : Object {
      let exprs : LinkedList <- new LinkedList,
            type : AnalyzedType in
         {
            let iter : Iterator <- parsedExpr.exprs().iterator() in
               while iter.next() loop
                  let parsedExpr : ParsedExpr <- case iter.get() of x : ParsedExpr => x; esac,
                        expr : AnalyzedExpr <- analyze(parsedExpr) in
                     {
                        exprs.add(expr);
                        type <- expr.type();
                     }
               pool;

            new AnalyzedBlockExpr.init(type, exprs);
         }
   };

   visitIf(parsedExpr : ParsedIfExpr) : Object {
      let expr : AnalyzedExpr <- analyze(parsedExpr.expr()) in
         {
            if not expr.type() = analyzer.boolType() then
               analyzer.errorAt(parsedExpr.expr(), "expression type '".concat(expr.type().name())
                     .concat("' is not type 'Bool' for predicate in 'if' expression"))
            else false fi;

            let then_ : AnalyzedExpr <- analyze(parsedExpr.then_()),
                  else_ : AnalyzedExpr <- analyze(parsedExpr.else_()) in
               new AnalyzedIfExpr.init(then_.type().join(else_.type()), expr, then_, else_);
         }
   };

   visitWhile(parsedExpr : ParsedWhileExpr) : Object {
      let expr : AnalyzedExpr <- analyze(parsedExpr.expr()) in
         {
            if not expr.type() = analyzer.boolType() then
               analyzer.errorAt(parsedExpr.expr(), "expression type '".concat(expr.type().name())
                     .concat("' is not type 'Bool' for predicate in 'while' expression"))
            else false fi;

            let loop_ : AnalyzedExpr <- analyze(parsedExpr.loop_()) in
               new AnalyzedWhileExpr.init(analyzer.objectType(), expr, loop_);
         }
   };

   visitLet(parsedExpr : ParsedLetExpr) : Object {
      let env : AnalyzedTypeEnv <- new AnalyzedTypeEnv.init(self),
            vars : Collection <- new LinkedList in
         {
            let varIter : Iterator <- parsedExpr.vars().iterator() in
               while varIter.next() loop
                  let var : ParsedVar <- case varIter.get() of x : ParsedVar => x; esac,
                        type : AnalyzedType <- analyzer.getTypeAllowSelf(var, " for 'let' variable", var.type(), containingType),
                        expr : AnalyzedExpr in
                     {
                        let parsedExpr : ParsedExpr <- var.expr() in
                           if not isvoid parsedExpr then
                              {
                                 expr <- env.analyze(var.expr());

                                 if not expr.type().conformsTo(type) then
                                    analyzer.errorAt(var, "expression type '".concat(expr.type().name())
                                          .concat("' does not conform to type '").concat(type.name())
                                          .concat("' of variable '").concat(var.id())
                                          .concat("' in 'let' expression"))
                                 else false fi;
                              }
                           else false fi;

                        let id : String <- var.id() in
                           if id = "self" then
                              analyzer.errorAt(var, "invalid variable name 'self' in 'let' expression")
                           else
                              vars.add(new AnalyzedLetVar.init(env.putVar(id, type), expr))
                           fi;
                     }
               pool;

            let expr : AnalyzedExpr <- env.analyze(parsedExpr.expr()) in
               new AnalyzedLetExpr.init(expr.type(), expr, vars);
         }
   };

   visitCase(parsedExpr : ParsedCaseExpr) : Object {
      let expr : AnalyzedExpr <- analyze(parsedExpr.expr()),
            branches : LinkedList <- new LinkedList,
            used : StringMap <- new StringListMap,
            type : AnalyzedType in
         {
            let branchIter : Iterator <- parsedExpr.branches().iterator() in
               while branchIter.next() loop
                  let branch : ParsedVar <- case branchIter.get() of x : ParsedVar => x; esac,
                        checkTypeName : String <- branch.type(),
                        checkType : AnalyzedType <- analyzer.getType(branch, " for 'case' branch", checkTypeName) in
                     {
                        if not isvoid used.putNewWithString(checkTypeName, branch) then
                           analyzer.errorAt(branch, "duplicate type '".concat(checkTypeName)
                                 .concat("' in 'case' expression"))
                        else false fi;

                        let env : AnalyzedTypeEnv <- new AnalyzedTypeEnv.init(self) in
                           {
                              let id : String <- branch.id() in
                                 if id = "self" then
                                    analyzer.errorAt(branch, "invalid variable name 'self' in 'case' expression")
                                 else
                                    env.putVar(id, checkType)
                                 fi;

                              let branchExpr : AnalyzedExpr <- env.analyze(branch.expr()) in
                                 {
                                    branches.add(new AnalyzedCaseBranch.init(checkType, branchExpr));

                                    let branchType : AnalyzedType <- branchExpr.type() in
                                       if isvoid type then
                                          type <- branchType
                                       else
                                          type <- type.join(branchType)
                                       fi;
                                 };
                           };
                     }
               pool;

            new AnalyzedCaseExpr.init(parsedExpr.line(), type, expr, varIndex(), branches);
         }
   };

   visitAssignment(parsedExpr : ParsedAssignmentExpr) : Object {
      let id : String <- parsedExpr.id(),
            object : AnalyzedObject <- get(id) in
         {
            if isvoid object then
               analyzer.errorAt(parsedExpr, "undefined variable '".concat(id)
                     .concat("' in '<-' expression"))
            else false fi;

            let expr : AnalyzedExpr <- analyze(parsedExpr.expr()) in
               {
                  if not isvoid object then
                     if id = "self" then
                        analyzer.errorAt(parsedExpr, "invalid assignment to 'self' variable")
                     else
                        if not expr.type().conformsTo(object.type()) then
                           analyzer.errorAt(parsedExpr, "expression type '".concat(expr.type().name())
                                 .concat("' does not conform to type '").concat(object.type().name())
                                 .concat("' of variable '").concat(id)
                                 .concat("' in '<-' expression"))
                        else false fi
                     fi
                  else false fi;

                  new AnalyzedAssignmentExpr.init(expr.type(), object, expr);
               };
         }
   };

   visitId(parsedExpr : ParsedIdExpr) : Object {
      let id : String <- parsedExpr.id(),
            object : AnalyzedObject <- get(id),
            type : AnalyzedType in
         {
            if isvoid object then
               {
                  analyzer.errorAt(parsedExpr, "undefined variable '".concat(id).concat("'"));
                  type <- analyzer.objectType();
               }
            else
               type <- object.type()
            fi;

            new AnalyzedObjectExpr.init(type, object);
         }
   };

   visitNew(parsedExpr : ParsedNewExpr) : Object {
      let type : AnalyzedType <- analyzer.getTypeAllowSelf(parsedExpr, " for 'new' expression", parsedExpr.type(), containingType) in
         new AnalyzedNewExpr.init(parsedExpr.line(), type)
   };

   visitDispatch(parsedExpr : ParsedDispatchExpr) : Object {
      let targetParsedExpr : ParsedExpr <- parsedExpr.target(),
            targetExpr : AnalyzedExpr in
         {
            if isvoid targetParsedExpr then
               let selfTypeType : AnalyzedType <- containingType.selfTypeType() in
                  targetExpr <- new AnalyzedObjectExpr.init(selfTypeType, new AnalyzedSelfObject.init(selfTypeType))
            else
               targetExpr <- analyze(targetParsedExpr)
            fi;

            let targetType : AnalyzedType <- targetExpr.type(),
                  staticTypeName : String <- parsedExpr.type(),
                  static : Bool <- not staticTypeName = "",
                  dispatchType : AnalyzedType <-
                     if static then
                        let staticType : AnalyzedType <- analyzer.getType(parsedExpr, " for static dispatch expression", staticTypeName) in
                           {
                              if not targetType.conformsTo(staticType) then
                                 analyzer.errorAt(parsedExpr, "expression type '".concat(targetType.name())
                                       .concat("' does not conform to static type '").concat(staticTypeName)
                                       .concat("' in dispatch expression"))
                              else false fi;

                              staticType;
                           }
                     else
                        if targetType.isSelfType() then
                           targetType.selfTypeTarget()
                        else
                           targetType
                        fi
                     fi,
                  method : AnalyzedMethod <- dispatchType.getMethod(parsedExpr.id()),
                  formalTypeIter : Iterator,
                  returnType : AnalyzedType in
               {
                  if isvoid method then
                     {
                        analyzer.errorAt(parsedExpr, "undefined method '".concat(parsedExpr.id())
                              .concat("' in class '").concat(dispatchType.name())
                              .concat("' in dispatch expression"));

                        formalTypeIter <- new Iterator;
                        returnType <- analyzer.objectType();
                     }
                  else
                     {
                        formalTypeIter <- method.formalTypes().iterator();
                        returnType <- method.returnType();
                        if returnType.isSelfType() then
                           returnType <- targetType
                        else false fi;
                     }
                  fi;

                  let args : Collection <- parsedExpr.arguments() in
                     {
                        if not isvoid method then
                           let numFormals : Int <- method.formalTypes().size() in
                              if not args.size() = numFormals then
                                 analyzer.errorAt(parsedExpr, "expected "
                                       .concat(analyzer.stringUtil().fromInt(numFormals))
                                       .concat(" argument")
                                       .concat(if numFormals = 1 then "" else "s" fi)
                                       .concat(" to method '").concat(parsedExpr.id())
                                       .concat("' in class '").concat(dispatchType.name())
                                       .concat("' in dispatch expression"))
                              else false fi
                        else false fi;

                        let argIter : Iterator <- parsedExpr.arguments().iterator(),
                              exprs : Collection <- new LinkedList in
                           {
                              while argIter.next() loop
                                 let arg : ParsedExpr <- case argIter.get() of x : ParsedExpr => x; esac,
                                       expr : AnalyzedExpr <- analyze(arg) in
                                    {
                                       if formalTypeIter.next() then
                                          let formalType : AnalyzedType <- case formalTypeIter.get() of x : AnalyzedType => x; esac in
                                             if not expr.type().conformsTo(formalType) then
                                                analyzer.errorAt(arg, "type '".concat(expr.type().name())
                                                      .concat("' of argument #")
                                                      .concat(analyzer.stringUtil().fromInt(exprs.size() + 1))
                                                      .concat(" does not conform to type '")
                                                      .concat(formalType.name())
                                                      .concat("' of formal parameter in method '")
                                                      .concat(parsedExpr.id())
                                                      .concat("' in class '").concat(dispatchType.name())
                                                      .concat("' in dispatch expression"))
                                             else false fi
                                       else false fi;

                                       exprs.add(expr);
                                    }
                              pool;

                              new AnalyzedDispatchExpr.init(parsedExpr.line(), returnType, targetExpr, method, static, exprs);
                           };
                     };
               };
         }
   };

   visitUnary(parsedExpr : ParsedUnaryExpr) : Object {
      let expr : AnalyzedExpr <- analyze(parsedExpr.expr()),
            op : String <- parsedExpr.op(),
            type : AnalyzedType in
         {
            if op = "isvoid" then
               type <- analyzer.boolType()
            else
               {
                  type <- expr.type();

                  if op = "not" then
                     if not type = analyzer.boolType() then
                        analyzer.errorAt(parsedExpr.expr(), "expression type '".concat(expr.type().name())
                              .concat("' is not type 'Bool' for 'not' expression"))
                     else false fi
                  else
                     if op = "~" then
                        if not type = analyzer.intType() then
                           analyzer.errorAt(parsedExpr.expr(), "expression type '".concat(expr.type().name())
                                 .concat("' is not type 'Int' for '~' expression"))
                        else false fi
                     else
                        new ObjectUtil.abortObject(self, "visitUnary: invalid op=".concat(op))
                     fi
                  fi;
               }
            fi;

            new AnalyzedUnaryExpr.init(type, op, expr);
         }
   };

   isObjectComparisonOp(op : String) : Bool {
      if op = "=" then
         true
      else
         if analyzer.uva() then
            if op = "<" then
               true
            else
               op = "<="
            fi
         else
            false
         fi
      fi
   };

   visitBinary(parsedExpr : ParsedBinaryExpr) : Object {
      let left : AnalyzedExpr <- analyze(parsedExpr.left()),
            right : AnalyzedExpr <- analyze(parsedExpr.right()),
            op : String <- parsedExpr.op(),
            type : AnalyzedType in
         {
            if isObjectComparisonOp(op) then
               {
                  if if left.type() = analyzer.intType() then
                        true
                     else
                        if left.type() = analyzer.stringType() then
                           true
                        else
                           left.type() = analyzer.boolType()
                        fi
                  fi then
                     if not right.type() = left.type() then
                        analyzer.errorAt(parsedExpr, "left expression type '".concat(left.type().name())
                              .concat("' is not the same as right expression type '").concat(right.type().name())
                              .concat("' in '").concat(op).concat("' expression"))
                     else false fi
                  else false fi;

                  type <- analyzer.boolType();
               }
            else
               {
                  if not left.type() = analyzer.intType() then
                     analyzer.errorAt(parsedExpr, "left expression type '".concat(left.type().name())
                           .concat("' is not type 'Int' for '").concat(op)
                           .concat("' expression"))
                  else false fi;

                  if not right.type() = analyzer.intType() then
                     analyzer.errorAt(parsedExpr, "right expression type '".concat(right.type().name())
                           .concat("' is not type 'Int' for '").concat(op)
                           .concat("' expression"))
                  else false fi;

                  if if op = "<" then
                        true
                     else
                        op = "<="
                  fi then
                     type <- analyzer.boolType()
                  else
                     if if op = "+" then true else
                        if op = "-" then true else
                        if op = "*" then true else
                           op = "/"
                        fi fi
                     fi then
                        type <- analyzer.intType()
                     else
                        new ObjectUtil.abortObject(self, "visitBinary invalid op=".concat(op))
                     fi
                  fi;
               }
            fi;

            new AnalyzedBinaryExpr.init(parsedExpr.line(), type, op, left, right);
         }
   };

   visitConstantBool(parsedExpr : ParsedConstantBoolExpr) : Object {
      new AnalyzedConstantBoolExpr.init(analyzer.boolType(), parsedExpr.value())
   };

   visitConstantInt(parsedExpr : ParsedConstantIntExpr) : Object {
      new AnalyzedConstantIntExpr.init(analyzer.intType(), parsedExpr.value())
   };

   visitConstantString(parsedExpr : ParsedConstantStringExpr) : Object {
      new AnalyzedConstantStringExpr.init(analyzer.stringType(), parsedExpr.value(), parsedExpr.escapes())
   };
};

class Analyzer {
   stringUtil : StringUtil <- new StringUtil;
   stringUtil() : StringUtil { stringUtil };

   uva : Bool;
   uva() : Bool { uva };

   setUva(uva_ : Bool) : SELF_TYPE {{
      uva <- uva_;
      self;
   }};

   objectType : AnalyzedType <- new AnalyzedType.initBuiltinObject();
   objectType() : AnalyzedType { objectType };

   ioType : AnalyzedType <- new AnalyzedType.initBuiltin("IO", objectType);

   intType : AnalyzedType <- new AnalyzedType.initBuiltin("Int", objectType);
   intType() : AnalyzedType { intType };

   stringType : AnalyzedType <- new AnalyzedType.initBuiltin("String", objectType);
   stringType() : AnalyzedType { stringType };

   boolType : AnalyzedType <- new AnalyzedType.initBuiltin("Bool", objectType);
   boolType() : AnalyzedType { boolType };

   types : StringMap <- initBuiltinTypes();

   initBuiltinTypes() : StringMap {
      let types : StringMap <- new StringListMap in
         {
            types.putWithString(objectType.name(), objectType);
            types.putWithString(ioType.name(), ioType);
            types.putWithString(intType.name(), intType);
            types.putWithString(stringType.name(), stringType);
            types.putWithString(boolType.name(), boolType);

            let collEmpty : Collection <- new Collection,
                  collString : Collection <- new LinkedList.add(stringType),
                  collInt : Collection <- new LinkedList.add(intType),
                  collIntInt : Collection <- new LinkedList.add(intType).add(intType) in
               {
                  objectType.addMethod(new AnalyzedMethod.initBuiltin(objectType, "abort", collEmpty, objectType));
                  objectType.addMethod(new AnalyzedMethod.initBuiltin(objectType, "type_name", collEmpty, stringType));
                  objectType.addMethod(new AnalyzedMethod.initBuiltin(objectType, "copy", collEmpty, objectType.selfTypeType()));

                  ioType.processInherits();
                  ioType.addMethod(new AnalyzedMethod.initBuiltin(ioType, "out_string", collString, ioType.selfTypeType()));
                  ioType.addMethod(new AnalyzedMethod.initBuiltin(ioType, "out_int", collInt, ioType.selfTypeType()));
                  ioType.addMethod(new AnalyzedMethod.initBuiltin(ioType, "in_string", collEmpty, stringType));
                  ioType.addMethod(new AnalyzedMethod.initBuiltin(ioType, "in_int", collEmpty, intType));

                  intType.processInherits();

                  stringType.processInherits();
                  stringType.addMethod(new AnalyzedMethod.initBuiltin(stringType, "length", collEmpty, intType));
                  stringType.addMethod(new AnalyzedMethod.initBuiltin(stringType, "concat", collString, stringType));
                  stringType.addMethod(new AnalyzedMethod.initBuiltin(stringType, "substr", collIntInt, stringType));

                  boolType.processInherits();
               };

            types;
         }
   };

   error : Bool;

   reportError(line : Int, s : String) : Object { new ObjectUtil.abortObject(self, "reportError: unimplemented") };

   error(s : String) : Object {{
      reportError(0, s);
      error <- true;
   }};

   errorAt(node : ParsedNode, s : String) : Object {{
      reportError(node.line(), s);
      error <- true;
   }};

   getTypeImpl(node : ParsedNode, where : String, name : String) : AnalyzedType {
      let type : Object <- types.getWithString(name) in
         if isvoid type then
            {
               errorAt(node, "undefined type '".concat(name).concat("'").concat(where));

               let type : AnalyzedType <- new AnalyzedType.initError(name) in
                  {
                     type.setInheritsType(objectType);
                     type.processInherits();
                     type;
                  };
            }
         else
            case type of x : AnalyzedType => x; esac
         fi
   };

   getType(node : ParsedNode, where : String, name : String) : AnalyzedType {
      if name = "SELF_TYPE" then
         {
            errorAt(node, "invalid type '".concat(name).concat("'").concat(where));
            objectType;
         }
      else
         getTypeImpl(node, where, name)
      fi
   };

   getTypeAllowSelf(node : ParsedNode, where : String, name : String, type : AnalyzedType) : AnalyzedType {
      if name = "SELF_TYPE" then
         type.selfTypeType()
      else
         getTypeImpl(node, where, name)
      fi
   };

   getInheritsType(node : ParsedClass, name : String) : AnalyzedType {
      if name = "" then
         objectType
      else
         let type : AnalyzedType <- getType(node, " for 'inherits' of class '".concat(node.type()).concat("'"), name) in
            if if type = intType then
                  true
               else
                  if type = stringType then
                     true
                  else
                     type = boolType
                  fi
               fi
            then
               {
                  errorAt(node, "invalid type '".concat(name).concat("' for 'inherits'"));
                  objectType;
               }
            else
               type
            fi
      fi
   };

   createAnalyzedAttribute(type : AnalyzedType, attr : ParsedAttribute) : AnalyzedAttribute {
      new AnalyzedAttribute.init(type, attr,
            getTypeAllowSelf(attr, " for attribute '".concat(attr.id())
                  .concat("' in class '").concat(type.name()).concat("'"),
            attr.type(), type))
   };

   createAnalyzedMethod(type : AnalyzedType, method : ParsedMethod) : AnalyzedMethod {
      let returnType : AnalyzedType <- getTypeAllowSelf(method, " for return", method.returnType(), type),
            formalTypes : Collection <- new LinkedList in
         {
            let formalIter : Iterator <- method.formals().iterator(),
                  index : Int <- 1 in
               while formalIter.next() loop
                  let formal : ParsedFormal <- case formalIter.get() of x : ParsedFormal => x; esac in
                     {
                        formalTypes.add(getType(
                              method,
                              " for formal parameter #".concat(stringUtil.fromInt(index)),
                              formal.type()));
                        index <- index + 1;
                     }
               pool;

            new AnalyzedMethod.init(type, method, formalTypes, returnType);
         }
   };

   analyzeMethodOverride(method : AnalyzedMethod, oldMethod : AnalyzedMethod) : Bool {
      let type : AnalyzedType <- method.containingType(),
            oldType : AnalyzedType <- oldMethod.containingType(),
            result : Bool <- true in
         {
            if type = oldMethod.containingType() then
               {
                  errorAt(method.parsedMethod(), "redefinition of method '".concat(method.id())
                        .concat("' in class '").concat(type.name()).concat("'"));
                  result <- false;
               }
            else
               {
                  let formalTypes : Collection <- method.formalTypes(),
                        numFormalTypes : Int <- formalTypes.size(),
                        oldFormalTypes : Collection <- oldMethod.formalTypes(),
                        numOldFormalTypes : Int <- oldFormalTypes.size() in
                     if not numFormalTypes = numOldFormalTypes then
                        {
                           errorAt(method.parsedMethod(), "redefinition of method '".concat(method.id())
                                 .concat("' in class '").concat(type.name())
                                 .concat("' with ").concat(stringUtil.fromInt(numFormalTypes))
                                 .concat(" formal parameter")
                                 .concat(if numFormalTypes = 1 then "" else "s" fi)
                                 .concat(" is not the same as ")
                                 .concat(stringUtil.fromInt(numOldFormalTypes))
                                 .concat(" in class '")
                                 .concat(oldType.name())
                                 .concat("'"));
                           result <- false;
                        }
                     else
                        let formalTypeIter : Iterator <- formalTypes.iterator(),
                              oldFormalTypeIter : Iterator <- oldFormalTypes.iterator(),
                              index : Int <- 1 in
                           while formalTypeIter.next() loop
                              {
                                 oldFormalTypeIter.next();
                                 let formalType : AnalyzedType <- case formalTypeIter.get() of x : AnalyzedType => x; esac,
                                       oldFormalType : AnalyzedType <- case oldFormalTypeIter.get() of x : AnalyzedType => x; esac in
                                    if not formalType = oldFormalType then
                                       {
                                          errorAt(method.parsedMethod(), "redefinition of method '".concat(method.id())
                                                .concat("' in class '").concat(type.name())
                                                .concat("' with type '").concat(formalType.name())
                                                .concat("' for formal parameter #").concat(stringUtil.fromInt(index))
                                                .concat(" is not the same as type '").concat(oldFormalType.name())
                                                .concat("' in class '").concat(oldType.name()).concat("'"));
                                          result <- false;
                                       }
                                    else false fi;

                                 index <- index + 1;
                              }
                           pool
                     fi;

                  let returnType : AnalyzedType <- method.returnType(),
                        oldReturnType : AnalyzedType <- oldMethod.returnType() in
                     -- Compare names to allow SELF_TYPE.
                     if not returnType.name() = oldReturnType.name() then
                        {
                           errorAt(method.parsedMethod(), "redefinition of method '".concat(method.id())
                                 .concat("' in class '").concat(type.name())
                                 .concat("' with return type '").concat(returnType.name())
                                 .concat("' is not the same as type '").concat(oldReturnType.name())
                                 .concat("' in class '").concat(oldType.name()).concat("'"));
                           result <- false;
                        }
                     else false fi;
               }
            fi;

            result;
         }
   };

   defineFeatures(type : AnalyzedType) : Object {
      if isvoid type.attributes() then
         {
            defineFeatures(type.inheritsType());
            type.processInherits();

            let parsedClass : ParsedClass <- type.parsedClass() in
               if not isvoid parsedClass then
                  let featureIter : Iterator <- parsedClass.features().iterator() in
                     while featureIter.next() loop
                        let feature : ParsedFeature <- case featureIter.get() of x : ParsedFeature => x; esac,
                              attr : ParsedAttribute <- feature.asAttribute() in
                           if isvoid attr then
                              let method : AnalyzedMethod <- createAnalyzedMethod(type, feature.asMethod()),
                                    oldMethod : AnalyzedMethod <- type.addMethod(method) in
                                 if not isvoid oldMethod then
                                    if analyzeMethodOverride(method, oldMethod) then
                                       type.addMethodOverride(method)
                                    else false fi
                                 else false fi
                           else
                              if not isvoid type.addAttribute(createAnalyzedAttribute(type, attr)) then
                                 errorAt(feature, "redefinition of attribute '".concat(feature.id())
                                       .concat("' in class '").concat(type.name()).concat("'"))
                              else false fi
                           fi
                     pool
               else false fi;
         }
      else false fi
   };

   createTypeEnv(type : AnalyzedType) : AnalyzedTypeEnv {
      let env : AnalyzedTypeEnv <- new AnalyzedTypeEnv.initContainingType(self, type) in
         {
            let attrIter : StringMapIterator <- type.attributes().iterator() in
               while attrIter.next() loop
                  let attr : AnalyzedAttribute <- case attrIter.value() of x : AnalyzedAttribute => x; esac in
                     env.put(attr.id(), new AnalyzedAttributeObject.init(attr.type(), attr))
               pool;

            env;
         }
   };

   analyzeAttribute(env : AnalyzedTypeEnv, attr : AnalyzedAttribute) : Object {
      if attr.id() = "self" then
         errorAt(attr.parsedAttribute(), "invalid attribute name 'self'")
      else
         let expr : AnalyzedExpr in
            {
               let parsedExpr : ParsedExpr <- attr.parsedAttribute().expr() in
                  if not isvoid parsedExpr then
                     {
                        expr <- env.analyze(parsedExpr);

                        if not expr.type().conformsTo(attr.type()) then
                           errorAt(parsedExpr, "expression type '".concat(expr.type().name())
                                 .concat("' does not conform to type '").concat(attr.type().name())
                                 .concat("' of attribute '").concat(attr.id()).concat("'"))
                        else false fi;
                     }
                  else false fi;

               attr.setExpr(expr);
            }
      fi
   };

   analyzeMethod(classEnv : AnalyzedTypeEnv, method : AnalyzedMethod) : Object {
      let env : AnalyzedTypeEnv <- new AnalyzedTypeEnv.init(classEnv) in
         {
            let formalIter : Iterator <- method.parsedMethod().formals().iterator(),
                  formalTypeIter : Iterator <- method.formalTypes().iterator(),
                  usedIds : StringMap <- new StringListMap,
                  index : Int <- 0 in
               {
                  formalTypeIter.next();
                  while formalIter.next() loop
                     let formal : ParsedFormal <- case formalIter.get() of x : ParsedFormal => x; esac,
                           formalType : AnalyzedType <- case formalTypeIter.get() of x : AnalyzedType => x; esac,
                           id : String <- formal.id() in
                        {
                           if id = "self" then
                              errorAt(formal, "invalid formal parameter name 'self'")
                           else
                              if not isvoid usedIds.putWithString(id, true) then
                                 errorAt(formal, "duplicate formal parameter name '".concat(id)
                                       .concat("' for method '").concat(method.id()).concat("'"))
                              else
                                 env.put(id, new AnalyzedArgumentObject.init(formalType, index))
                              fi
                           fi;

                           index <- index + 1;
                           formalTypeIter.next();
                        }
                  pool;
               };

            let parsedExpr : ParsedExpr <- method.parsedMethod().expr(),
                  expr : AnalyzedExpr <- env.analyze(parsedExpr),
                  exprType : AnalyzedType <- expr.type(),
                  returnType : AnalyzedType <- method.returnType() in
               {
                  if not exprType.conformsTo(returnType) then
                     errorAt(parsedExpr, "expression type '".concat(exprType.name())
                           .concat("' does not conform to return type '").concat(returnType.name())
                           .concat("' of method '").concat(method.id()).concat("'"))
                  else false fi;

                  method.setExpr(expr);
               };
         }
   };

   analyze(prog : ParsedProgram) : AnalyzedProgram {
      let typeList : Collection <- new LinkedList in
         {
            -- Register all classes.
            let classIter : Iterator <- prog.classes().iterator() in
               while classIter.next() loop
                  let class_ : ParsedClass <- case classIter.get() of x : ParsedClass => x; esac,
                        typeName : String <- class_.type() in
                     if typeName = "SELF_TYPE" then
                        errorAt(class_, "definition of type '".concat(typeName).concat("'"))
                     else
                        let type : AnalyzedType <- new AnalyzedType.init(class_) in
                           if isvoid types.putNewWithString(typeName, type) then
                              typeList.add(type)
                           else
                              errorAt(class_, "redefinition of class '".concat(typeName).concat("'"))
                           fi
                     fi
               pool;

            -- Resolve inherits
            let classIter : Iterator <- typeList.iterator() in
               while classIter.next() loop
                  let type : AnalyzedType <- case classIter.get() of x : AnalyzedType => x; esac,
                        class_ : ParsedClass <- type.parsedClass() in
                     type.setInheritsType(getInheritsType(class_, class_.inherits_()))
               pool;

            -- Diagnose recursive inherits.
            let classIter : Iterator <- typeList.iterator() in
               while classIter.next() loop
                  let type : AnalyzedType <- case classIter.get() of x : AnalyzedType => x; esac,
                        slow : AnalyzedType <- type.inheritsType(),
                        fast : AnalyzedType <- type.inheritsType2(),
                        continue : Bool <- true in
                     {
                        if isvoid fast then
                           slow <- fast
                        else false fi;

                        while not slow = fast loop
                           {
                              slow <- slow.inheritsType();
                              fast <- fast.inheritsType2();

                              if isvoid fast then
                                 slow <- fast
                              else false fi;
                           }
                        pool;

                        if not isvoid slow then
                           {
                              errorAt(type.parsedClass(), "hierarchy of class '"
                                    .concat(type.name()).concat("' contains a cycle"));
                              type.setInheritsType(objectType);
                           }
                        else false fi;
                     }
               pool;

            -- Define features.
            let classIter : Iterator <- typeList.iterator() in
               while classIter.next() loop
                  let type : AnalyzedType <- case classIter.get() of x : AnalyzedType => x; esac in
                     defineFeatures(type)
               pool;

            -- Semantic analysis.
            let classIter : Iterator <- typeList.iterator() in
               while classIter.next() loop
                  let type : AnalyzedType <- case classIter.get() of x : AnalyzedType => x; esac,
                        env : AnalyzedTypeEnv <- createTypeEnv(type),
                        featureIter : Iterator <- type.definedFeatures().iterator() in
                     while featureIter.next() loop
                        let feature : AnalyzedFeature <- case featureIter.get() of x : AnalyzedFeature => x; esac,
                              attr : AnalyzedAttribute <- feature.asAttribute() in
                           if isvoid attr then
                              analyzeMethod(env, feature.asMethod())
                           else
                              analyzeAttribute(env, attr)
                           fi
                     pool
               pool;

            -- Find main
            let mainMethod : AnalyzedMethod in
               {
                  let mainTypeObject : Object <- types.getWithString("Main") in
                     if isvoid mainTypeObject then
                        error("expected class 'Main'")
                     else
                        let mainType : AnalyzedType <- case mainTypeObject of x : AnalyzedType => x; esac in
                           {
                              mainMethod <- mainType.getMethod("main");

                              if isvoid mainMethod then
                                 errorAt(mainType.parsedClass(), "expected method 'main' in class 'Main'")
                              else
                                 let numFormals : Int <- mainMethod.formalTypes().size() in
                                    if not numFormals = 0 then
                                       errorAt(mainMethod.parsedMethod(),
                                             "expected 0 formal parameters for method 'main' in class 'Main'")
                                    else false fi
                              fi;
                           }
                     fi;

                     -- Allow parse nodes to be GC'ed.
                     let classIter : Iterator <- typeList.iterator() in
                        while classIter.next() loop
                           let type : AnalyzedType <- case classIter.get() of x : AnalyzedType => x; esac in
                              {
                                 let featureIter : Iterator <- type.definedFeatures().iterator() in
                                    while featureIter.next() loop
                                       let feature : AnalyzedFeature <- case featureIter.get() of x : AnalyzedFeature => x; esac in
                                          feature.unsetParsedFeature()
                                    pool;

                                 type.unsetParsedClass();
                              }
                        pool;

                     if error then
                        let void : AnalyzedProgram in void
                     else
                        new AnalyzedProgram.init(types, mainMethod)
                     fi;
               };
         }
   };
};
