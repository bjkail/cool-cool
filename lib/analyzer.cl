class AnalyzedProgram {
   types : Collection;
   types() : Collection { types };

   mainMethod : AnalyzedMethod;
   mainMethod() : AnalyzedMethod { mainMethod };

   init(types_ : Collection, mainMethod_ : AnalyzedMethod) : SELF_TYPE {{
      types <- types_;
      mainMethod <- mainMethod_;
      self;
   }};
};

class AnalyzedType {
   name : String;
   name() : String { name };

   parsedClass : ParsedClass;
   parsedClass() : ParsedClass { parsedClass };
   unsetParsedClass() : Object { parsedClass <- let void : ParsedClass in void };

   inheritsType : AnalyzedType;
   inheritsType() : AnalyzedType { inheritsType };
   inheritsType2() : AnalyzedType {
      if isvoid inheritsType then
         inheritsType
      else
         inheritsType.inheritsType()
      fi
   };

   inheritsDepth : Int;
   inheritsDepth() : Int { inheritsDepth };

   setInheritsType(inheritsType_ : AnalyzedType) : SELF_TYPE {{
      inheritsType <- inheritsType_;
      self;
   }};

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
                  {
                     type1 <- type1.inheritsType();
                     while type2.inheritsDepth() < type1.inheritsDepth() loop
                        type1 <- type1.inheritsType()
                     pool;
                  }
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

   features : Collection <- new LinkedList;
   features() : Collection { features };

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
               features.add(attr);
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
               features.add(method);
               let void : AnalyzedMethod in void;
            }
         else
            case old of x : AnalyzedMethod => x; esac
         fi
   };

   processInherits() : Object {{
      inheritsDepth <- inheritsType.inheritsDepth() + 1;
      attributes <- inheritsType.attributes().copy();
      methods <- inheritsType.methods().copy();
   }};

   initBuiltinObject() : SELF_TYPE {{
      name <- "Object";
      attributes <- new StringListMap;
      methods <- new StringListMap;
      self;
   }};

   initBuiltin(name_ : String, inheritsType_ : AnalyzedType) : SELF_TYPE {{
      name <- name_;
      inheritsType <- inheritsType_;
      self;
   }};

   init(parsedClass_ : ParsedClass) : SELF_TYPE {{
      parsedClass <- parsedClass_;
      name <- parsedClass_.type();
      self;
   }};

   initError(name_ : String) : SELF_TYPE {{
      name <- name_;
      self;
   }};
};

class AnalyzedSelfType inherits AnalyzedType {
   getAttribute(id : String) : AnalyzedAttribute {{
      new Object.abort();
      let void : AnalyzedAttribute in void;
   }};

   getMethod(id : String) : AnalyzedMethod {{
      new Object.abort();
      let void : AnalyzedMethod in void;
   }};
};

class AnalyzedFeature {
   id : String;
   id() : String { id };

   unsetParsedFeature() : Object { new Object.abort() };

   asAttribute() : AnalyzedAttribute { let void : AnalyzedAttribute in void };
   asMethod() : AnalyzedMethod { let void : AnalyzedMethod in void };
};

class AnalyzedAttribute inherits AnalyzedFeature {
   parsedAttribute : ParsedAttribute;
   parsedAttribute() : ParsedAttribute { parsedAttribute };
   unsetParsedFeature() : Object { parsedAttribute <- let void : ParsedAttribute in void };

   type : AnalyzedType;
   type() : AnalyzedType { type };

   init(parsedAttribute_ : ParsedAttribute, type_ : AnalyzedType) : SELF_TYPE {{
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

   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };
   setExpr(expr_ : AnalyzedExpr) : Object { expr <- expr_ };

   initBuiltin(id_ : String, formalTypes_ : Collection, returnType_ : AnalyzedType) : SELF_TYPE {{
      id <- id_;
      formalTypes <- formalTypes_;
      returnType <- returnType_;
      self;
   }};

   init(parsedMethod_ : ParsedMethod, formalTypes : Collection, returnType : AnalyzedType) : SELF_TYPE {{
      parsedMethod <- parsedMethod_;
      initBuiltin(parsedMethod_.id(), formalTypes, returnType);
   }};

   asMethod() : AnalyzedMethod { self };
};

class AnalyzedObject {
   type : AnalyzedType;
   type() : AnalyzedType { type };
};

class AnalyzedSelfObject inherits AnalyzedObject {
   init(type_ : AnalyzedType) : SELF_TYPE {{
      type <- type_;
      self;
   }};
};

class AnalyzedFormalObject inherits AnalyzedObject {
   index : Int;
   index() : Int { index };

   init(type_ : AnalyzedType, index_ : Int) : SELF_TYPE {{
      type <- type_;
      index <- index_;
      self;
   }};
};

class AnalyzedVarObject inherits AnalyzedObject {
   index : Int;
   index() : Int { index };

   init(type_ : AnalyzedType, index_ : Int) : SELF_TYPE {{
      type <- type_;
      index <- index_;
      self;
   }};
};

class AnalyzedAttributeObject inherits AnalyzedObject {
   attribute : AnalyzedAttribute;
   attribute() : AnalyzedAttribute { attribute };

   init(type_ : AnalyzedType, attribute_ : AnalyzedAttribute) : SELF_TYPE {{
      type <- type_;
      attribute <- attribute_;
      self;
   }};
};

class AnalyzedExpr {
   type : AnalyzedType;
   type() : AnalyzedType { type };
};

class AnalyzedBlockExpr inherits AnalyzedExpr {
   exprs : Collection;
   exprs() : Collection { exprs };

   init(type_ : AnalyzedType, exprs_ : Collection) : SELF_TYPE {{
      type <- type_;
      exprs <- exprs_;
      self;
   }};
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
};

class AnalyzedWhileExpr inherits AnalyzedExpr {
   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   loop_ : AnalyzedExpr;
   loop_() : AnalyzedExpr { loop_ };

   init(type_ : AnalyzedType, expr_ : AnalyzedExpr, loop__ : AnalyzedExpr) : SELF_TYPE {{
      type <- type_;
      expr <- expr_;
      loop_ <- loop_;
      self;
   }};
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

class AnalyzedCaseBranchComparator inherits Comparator {
   compare(o1 : Object, o2 : Object) : Int {
      let inheritsDepth1 : Int <- case o1 of x : AnalyzedCaseBranch => x.checkType().inheritsDepth(); esac,
            inheritsDepth2 : Int <- case o2 of x : AnalyzedCaseBranch => x.checkType().inheritsDepth(); esac in
         if inheritsDepth1 = inheritsDepth2 then
            0
         else
            -- Sort highest entries first.
            if inheritsDepth1 < inheritsDepth2 then
               1
            else
               0-1
            fi
         fi
   };
};

class AnalyzedCaseExpr inherits AnalyzedExpr {
   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   branches : Collection;
   branches() : Collection { branches };

   init(type_ : AnalyzedType, expr_ : AnalyzedExpr, branches_ : Collection) : SELF_TYPE {{
      type <- type_;
      expr <- expr_;
      branches <- branches_;
      self;
   }};
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
};

class AnalyzedObjectExpr inherits AnalyzedExpr {
   object : AnalyzedObject;
   object() : AnalyzedObject { object };

   init(type_ : AnalyzedType, object_ : AnalyzedObject) : SELF_TYPE {{
      type <- type_;
      object <- object_;
      self;
   }};
};

class AnalyzedNewExpr inherits AnalyzedExpr {
   init(type_ : AnalyzedType) : SELF_TYPE {{
      type <- type_;
      self;
   }};
};

class AnalyzedDispatchExpr inherits AnalyzedExpr {
   line : Int;
   line() : Int { line };

   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   method : AnalyzedMethod;
   method() : AnalyzedMethod { method };

   arguments : Collection;
   arguments() : Collection { arguments };

   init(line_ : Int, type_ : AnalyzedType, expr_ : AnalyzedExpr, method_ : AnalyzedMethod, arguments_ : Collection) : SELF_TYPE {{
      line <- line_;
      type <- type_;
      expr <- expr_;
      method <- method_;
      arguments <- arguments_;
      self;
   }};
};

class AnalyzedUnaryExpr inherits AnalyzedExpr {
   op : String;
   op() : String { op };

   expr : AnalyzedExpr;
   expr() : AnalyzedExpr { expr };

   init(type_ : AnalyzedType, op_ : String, expr_ : AnalyzedExpr) : SELF_TYPE {{
      type <- type_;
      expr <- expr_;
      self;
   }};
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
};

class AnalyzedConstantBoolExpr inherits AnalyzedExpr {
   value : Bool;
   value() : Bool { value };

   init(type_ : AnalyzedType, value_ : Bool) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};
};

class AnalyzedConstantIntExpr inherits AnalyzedExpr {
   value : Int;
   value() : Int { value };

   init(type_ : AnalyzedType, value_ : Int) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};
};

class AnalyzedConstantStringExpr inherits AnalyzedExpr {
   value : String;
   value() : String { value };

   init(type_ : AnalyzedType, value_ : String) : SELF_TYPE {{
      type <- type_;
      value <- value_;
      self;
   }};
};


class AnalyzedTypeEnv inherits ParsedExprVisitor {
   analyzer : Analyzer;
   analyzer() : Analyzer { analyzer };

   selfType : AnalyzedType;
   parent : AnalyzedTypeEnv;
   bindings : StringMap <- new StringListMap;

   varIndex : Int;
   varIndex() : Int { varIndex };

   initSelfType(analyzed_ : Analyzer, selfType_ : AnalyzedType) : SELF_TYPE {{
      analyzer <- analyzed_;
      selfType <- selfType_;
      self;
   }};

   init(parent_ : AnalyzedTypeEnv) : SELF_TYPE {{
      analyzer <- parent_.analyzer();
      varIndex <- parent_.varIndex();
      parent <- parent_;
      self;
   }};

   put(id : String, object : AnalyzedObject) : Object {
      bindings.putWithString(id, object)
   };

   putVar(id : String, type : AnalyzedType) : Object {{
      put(id, new AnalyzedVarObject.init(type, varIndex));
      varIndex <- varIndex + 1;
   }};

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
                        exprs.add(type);
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
                     .concat("' is not required type 'Bool' for predicate in 'if' expression"))
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
                     .concat("' is not required type 'Bool' for predicate in 'while' expression"))
            else false fi;

            let loop_ : AnalyzedExpr <- analyze(parsedExpr.loop_()) in
               new AnalyzedWhileExpr.init(analyzer.objectType(), expr, loop_);
         }
   };

   visitLet(parsedExpr : ParsedLetExpr) : Object {
      let env : AnalyzedTypeEnv <- new AnalyzedTypeEnv.init(self) in
         {
            let varIter : Iterator <- parsedExpr.vars().iterator() in
               while varIter.next() loop
                  let var : ParsedVar <- case varIter.get() of x : ParsedVar => x; esac,
                        type : AnalyzedType <- analyzer.getTypeAllowSelf(var, " for 'let' variable", var.type()),
                        expr : AnalyzedExpr <- env.analyze(var.expr()) in
                     {
                        if not expr.type().conformsTo(type) then
                           analyzer.errorAt(var, "expression type '".concat(expr.type().name())
                                 .concat("' does not conform to type '").concat(type.name())
                                 .concat("' of variable '").concat(var.id())
                                 .concat("' in 'let' expression"))
                        else false fi;

                        let id : String <- var.id() in
                           if id = "self" then
                              analyzer.errorAt(var, "invalid redefinition of 'self' variable in 'let' expression")
                           else
                              env.putVar(id, type)
                           fi;
                     }
               pool;

            env.analyze(parsedExpr.expr());
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
                                    analyzer.errorAt(branch, "invalid redefinition of 'self' variable in 'case' expression")
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

            branches.sort(new AnalyzedCaseBranchComparator);
            new AnalyzedCaseExpr.init(type, expr, branches);
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
                        let type : AnalyzedType <- analyzer.replaceSelfType(object.type(), selfType) in
                           if not expr.type().conformsTo(type) then
                              analyzer.errorAt(parsedExpr, "expression type '".concat(expr.type().name())
                                    .concat("' does not conform to type '").concat(type.name())
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
               type <- analyzer.replaceSelfType(object.type(), selfType)
            fi;

            new AnalyzedObjectExpr.init(type, object);
         }
   };

   visitNew(parsedExpr : ParsedNewExpr) : Object {
      let type : AnalyzedType <- analyzer.getTypeAllowSelf(parsedExpr, " for 'new' expression", parsedExpr.type()) in
         new AnalyzedNewExpr.init(analyzer.replaceSelfType(type, selfType))
   };

   visitDispatch(parsedExpr : ParsedDispatchExpr) : Object {
      let targetParsedExpr : ParsedExpr <- parsedExpr.target(),
            targetExpr : AnalyzedExpr in
         {
            if isvoid targetParsedExpr then
               targetExpr <- new AnalyzedObjectExpr.init(selfType, new AnalyzedSelfObject.init(selfType))
            else
               targetExpr <- analyze(targetParsedExpr)
            fi;

            let targetTypeName : String <- parsedExpr.type(),
                  targetType : AnalyzedType <- if targetTypeName = "" then
                           targetExpr.type()
                        else
                           analyzer.getType(parsedExpr, " for dispatch expression", targetTypeName)
                     fi,
                  method : AnalyzedMethod <- targetType.getMethod(parsedExpr.id()),
                  formalTypeIter : Iterator,
                  returnType : AnalyzedType in
               {
                  if isvoid method then
                     {
                        analyzer.errorAt(parsedExpr, "undefined method '".concat(parsedExpr.id())
                              .concat("' in class '").concat(targetType.name())
                              .concat("' in dispatch expression"));

                        formalTypeIter <- new Iterator;
                        returnType <- analyzer.objectType();
                     }
                  else
                     {
                        formalTypeIter <- method.formalTypes().iterator();
                        returnType <- analyzer.replaceSelfType(method.returnType(), targetType);
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
                                       .concat("' in class '").concat(targetType.name())
                                       .concat(" in dispatch expression"))
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
                                                analyzer.errorAt(arg, "argument "
                                                      .concat(analyzer.stringUtil().fromInt(args.size() + 1))
                                                      .concat(" type '").concat(expr.type().name())
                                                      .concat("' does not conform to type '")
                                                      .concat(formalType.name())
                                                      .concat("' of formal parameter in method '")
                                                      .concat(parsedExpr.id())
                                                      .concat("' in class '").concat(targetType.name())
                                                      .concat(" in dispatch expression"))
                                             else false fi
                                       else false fi;

                                       args.add(expr);
                                    }
                              pool;

                              new AnalyzedDispatchExpr.init(parsedExpr.line(), returnType, targetExpr, method, args);
                           };
                     };
               };
         }
   };

   visitUnary(parsedExpr : ParsedUnaryExpr) : Object {
      let expr : AnalyzedExpr <- analyze(parsedExpr.expr()),
            op : String <- parsedExpr.op() in
         {
            if not op = "void" then
               if op = "not" then
                  if not expr.type() = analyzer.boolType() then
                     analyzer.errorAt(parsedExpr.expr(), "expression type '".concat(expr.type().name())
                           .concat("' is not required type 'Bool' for 'not' expression"))
                  else false fi
               else
                  if op = "~" then
                     if not expr.type() = analyzer.intType() then
                        analyzer.errorAt(parsedExpr.expr(), "expression type '".concat(expr.type().name())
                              .concat("' is not required type 'Bool' for '~' expression"))
                     else false fi
                  else new Object.abort() fi
               fi
            else false fi;

            new AnalyzedUnaryExpr.init(expr.type(), op, expr);
         }
   };

   visitBinary(parsedExpr : ParsedBinaryExpr) : Object {
      let left : AnalyzedExpr <- analyze(parsedExpr.left()),
            right : AnalyzedExpr <- analyze(parsedExpr.right()),
            op : String <- parsedExpr.op(),
            type : AnalyzedType in
         {
            if op = "=" then
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
                           .concat("' is not required type 'Int' for '").concat(op)
                           .concat("' expression"))
                  else false fi;

                  if not right.type() = analyzer.intType() then
                     analyzer.errorAt(parsedExpr, "right expression type '".concat(right.type().name())
                           .concat("' is not required type 'Int' for '").concat(op)
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
                     else new Object.abort() fi
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
      new AnalyzedConstantStringExpr.init(analyzer.stringType(), parsedExpr.value())
   };
};

class Analyzer {
   stringUtil : StringUtil <- new StringUtil;
   stringUtil() : StringUtil { stringUtil };

   objectType : AnalyzedType <- new AnalyzedType.initBuiltinObject();
   objectType() : AnalyzedType { objectType };

   ioType : AnalyzedType <- new AnalyzedType.initBuiltin("IO", objectType);

   intType : AnalyzedType <- new AnalyzedType.initBuiltin("Int", objectType);
   intType() : AnalyzedType { intType };

   stringType : AnalyzedType <- new AnalyzedType.initBuiltin("String", objectType);
   stringType() : AnalyzedType { stringType };

   boolType : AnalyzedType <- new AnalyzedType.initBuiltin("Bool", objectType);
   boolType() : AnalyzedType { boolType };

   selfTypeType : AnalyzedType <- new AnalyzedSelfType.initError("SELF_TYPE");
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
                  objectType.addMethod(new AnalyzedMethod.initBuiltin("abort", collEmpty, objectType));
                  objectType.addMethod(new AnalyzedMethod.initBuiltin("type_name", collEmpty, stringType));
                  objectType.addMethod(new AnalyzedMethod.initBuiltin("copy", collEmpty, selfTypeType));

                  ioType.processInherits();
                  ioType.addMethod(new AnalyzedMethod.initBuiltin("out_string", collString, selfTypeType));
                  ioType.addMethod(new AnalyzedMethod.initBuiltin("out_int", collInt, selfTypeType));
                  ioType.addMethod(new AnalyzedMethod.initBuiltin("in_string", collEmpty, stringType));
                  ioType.addMethod(new AnalyzedMethod.initBuiltin("in_int", collEmpty, intType));

                  intType.processInherits();

                  stringType.processInherits();
                  stringType.addMethod(new AnalyzedMethod.initBuiltin("length", collEmpty, intType));
                  stringType.addMethod(new AnalyzedMethod.initBuiltin("concat", collString, stringType));
                  stringType.addMethod(new AnalyzedMethod.initBuiltin("substr", collIntInt, stringType));

                  boolType.processInherits();
               };

            types;
         }
   };

   lineMap : TokenizerLineMap;

   init(lineMap_ : TokenizerLineMap) : SELF_TYPE {{
      lineMap <- lineMap_;
      self;
   }};

   error : Bool;

   error(s : String) : Object {{
      new IO.out_string("ANALYZER ERROR: ")
            .out_string(s)
            .out_string("\n");
      error <- true;
   }};

   errorAt(node : ParsedNode, s : String) : Object {
      error(lineMap.lineToString(node.line()).concat(": ").concat(s))
   };

   getType(node : ParsedNode, where : String, name : String) : AnalyzedType {
      let type : Object <- types.getWithString(name) in
         if isvoid type then
            {
               errorAt(node, "undefined type '".concat(name).concat("'").concat(where));
               new AnalyzedType.initError(name).setInheritsType(objectType);
            }
         else
            case type of x : AnalyzedType => x; esac
         fi
   };

   getTypeAllowSelf(node : ParsedNode, where : String, name : String) : AnalyzedType {
      if name = "SELF_TYPE" then
         selfTypeType
      else
         getType(node, where, name)
      fi
   };

   createAnalyzedAttribute(attr : ParsedAttribute) : AnalyzedAttribute {
      new AnalyzedAttribute.init(attr, getTypeAllowSelf(attr, " for attribute", attr.type()))
   };

   createAnalyzedMethod(method : ParsedMethod) : AnalyzedMethod {
      let returnType : AnalyzedType <- getTypeAllowSelf(method, " for return", method.returnType()),
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

            new AnalyzedMethod.init(method, formalTypes, returnType);
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
                              -- TODO: allow override
                              if not isvoid type.addMethod(createAnalyzedMethod(feature.asMethod())) then
                                 errorAt(feature, "unexpected redefinition of method '".concat(feature.id())
                                       .concat("' in class '").concat(type.name()).concat("'"))
                              else false fi
                           else
                              if not isvoid type.addAttribute(createAnalyzedAttribute(attr)) then
                                 errorAt(feature, "unexpected redefinition of attribute '".concat(feature.id())
                                       .concat("' in class '").concat(type.name()).concat("'"))
                              else false fi
                           fi
                     pool
               else false fi;
         }
      else false fi
   };

   replaceSelfType(type : AnalyzedType, selfType : AnalyzedType) : AnalyzedType {
      if type = selfTypeType then
         selfType
      else
         type
      fi
   };

   createTypeEnv(type : AnalyzedType) : AnalyzedTypeEnv {
      let env : AnalyzedTypeEnv <- new AnalyzedTypeEnv.initSelfType(self, type) in
         {
            let attrIter : StringMapIterator <- type.attributes().iterator() in
               while attrIter.next() loop
                  let attr : AnalyzedAttribute <- case attrIter.value() of x : AnalyzedAttribute => x; esac in
                     env.put(attr.id(), new AnalyzedAttributeObject.init(replaceSelfType(attr.type(), type), attr))
               pool;

            env;
         }
   };

   analyzeAttribute(env : AnalyzedTypeEnv, attr : AnalyzedAttribute) : Object {
      false
   };

   analyzeMethod(classEnv : AnalyzedTypeEnv, method : AnalyzedMethod) : Object {
      let env : AnalyzedTypeEnv <- new AnalyzedTypeEnv.init(classEnv) in
         {
            let formalIter : Iterator <- method.parsedMethod().formals().iterator(),
                  formalTypeIter : Iterator <- method.formalTypes().iterator(),
                  index : Int <- 0 in
               {
                  formalTypeIter.next();
                  while formalIter.next() loop
                     let formal : ParsedFormal <- case formalIter.get() of x : ParsedFormal => x; esac,
                           formalType : AnalyzedType <- case formalTypeIter.get() of x : AnalyzedType => x; esac,
                           id : String <- formal.id() in
                        {
                           if id = "self" then
                              errorAt(formal, "unexpected formal parameter 'self'")
                           else
                              env.put(id, new AnalyzedFormalObject.init(formalType, index))
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
                        typeName : String <- class_.type(),
                        type : AnalyzedType <- new AnalyzedType.init(class_) in
                     if isvoid types.putNewWithString(typeName, type) then
                        typeList.add(type)
                     else
                        errorAt(class_, "unexpected redefinition of class '".concat(typeName).concat("'"))
                     fi
               pool;

            -- Resolve inherits
            let classIter : Iterator <- typeList.iterator() in
               while classIter.next() loop
                  let type : AnalyzedType <- case classIter.get() of x : AnalyzedType => x; esac,
                        class_ : ParsedClass <- type.parsedClass(),
                        inherits_ : String <- class_.inherits_() in
                     type.setInheritsType(if inherits_ = "" then
                              objectType
                           else
                              getType(class_, " for 'inherits'", inherits_)
                           fi)
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
                        featureIter : Iterator <- type.features().iterator() in
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
                                 let featureIter : Iterator <- type.features().iterator() in
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
                        new AnalyzedProgram.init(typeList, mainMethod)
                     fi;
               };
         }
   };
};
