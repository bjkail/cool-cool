class CoolasmType {
   analyzedType : AnalyzedType;
   analyzedType() : AnalyzedType { analyzedType };

   name : String;
   name() : String { name };

   label : CoolasmLabel;
   label() : CoolasmLabel { label };

   nameLabel : CoolasmLabel;
   nameLabel() : CoolasmLabel { nameLabel };

   analyzed : Bool;

   analyze() : Bool {
      if analyzed then
         false
      else
         analyzed <- true
      fi
   };

   inheritsType : CoolasmType;
   inheritsType() : CoolasmType { inheritsType };

   inheritsDepth : Int;
   inheritsDepth() : Int { inheritsDepth };

   hierarchy : Collection <- new LinkedList;
   hierarchy() : Collection { hierarchy };

   setInheritsType(inheritsType_ : CoolasmType) : SELF_TYPE {{
      inheritsType <- inheritsType_;
      inheritsDepth <- inheritsType_.inheritsDepth() + 1;
      hierarchy.addAll(inheritsType_.hierarchy());
      hierarchy.add(self);
      nextAttributeIndex <- inheritsType_.nextAttributeIndex();
      nextMethodIndex <- inheritsType_.nextMethodIndex();
      self;
   }};

   attributes : StringMap <- new StringListMap;

   getAttribute(id : String) : CoolasmAttribute {
      let attr : Object <- attributes.getWithString(id) in
         if isvoid attr then
            inheritsType.getAttribute(id)
         else
            case attr of x : CoolasmAttribute => x; esac
         fi
   };

   definedAttributes : Collection <- new LinkedList;
   definedAttributes() : Collection { definedAttributes };

   nextAttributeIndex : Int;
   nextAttributeIndex() : Int { nextAttributeIndex };
   allocSize() : Int { nextAttributeIndex };

   addAttribute(analyzedAttr : AnalyzedAttribute) : Object {
      let index : Int <-
               let index : Int <- nextAttributeIndex in
                  {
                     nextAttributeIndex <- index + 1;
                     index;
                  },
            attr : CoolasmAttribute <- new CoolasmAttribute.init(index, analyzedAttr) in
         {
            attributes.putWithString(attr.id(), attr);
            definedAttributes.add(attr);
         }
   };

   newLabel : CoolasmLabel;
   newLabel() : CoolasmLabel { newLabel };
   initNewLabel() : Object { newLabel <- new CoolasmLabel.init(name.concat("..new")) };

   methods : StringMap <- new StringListMap;

   getMethod(id : String) : CoolasmMethod {
      let method : Object <- methods.getWithString(id) in
         if isvoid method then
            if isvoid inheritsType then
               let void : CoolasmMethod in void
            else
               inheritsType.getMethod(id)
            fi
         else
            case method of x : CoolasmMethod => x; esac
         fi
   };

   methodDispatches : IntTreeMap <- new IntTreeMap;
   methodDispatches() : IntTreeMap { methodDispatches };

   definedMethods : Collection <- new LinkedList;
   definedMethods() : Collection { definedMethods };

   nextMethodIndex : Int;
   nextMethodIndex() : Int { nextMethodIndex };

   addMethod(analyzedMethod : AnalyzedMethod) : CoolasmMethod {
      let id : String <- analyzedMethod.id(),
            index : Int <-
               let old : CoolasmMethod <- inheritsType.getMethod(analyzedMethod.id()) in
                  if isvoid old then
                     let index : Int <- nextMethodIndex in
                        {
                           nextMethodIndex <- index + 1;
                           index;
                        }
                  else
                     old.index()
                  fi,
            method : CoolasmMethod <- new CoolasmMethod.init(self, index, analyzedMethod) in
         {
            methods.putWithString(method.id(), method);
            methodDispatches.putWithInt(method.index(), method);
            definedMethods.add(method);
            method;
         }
   };

   initBasicObject(analyzedType : AnalyzedType, nextAttributeIndex_ : Int, nextMethodIndex_ : Int) : SELF_TYPE {{
      hierarchy.add(self);
      nextAttributeIndex <- nextAttributeIndex_;
      nextMethodIndex <- nextMethodIndex_;
      initBasic(analyzedType);
   }};

   initBasic(analyzedType : AnalyzedType) : SELF_TYPE {{
      analyzed <- true;
      init(analyzedType);
   }};

   init(analyzedType_ : AnalyzedType) : SELF_TYPE {{
      analyzedType <- analyzedType_;
      name <- analyzedType_.name();
      label <- new CoolasmLabel.init(name.concat("..type"));
      nameLabel <- new CoolasmLabel.init("string.type.".concat(name));
      self;
   }};
};

class CoolasmAttribute {
   analyzedAttribute : AnalyzedAttribute;
   analyzedAttribute() : AnalyzedAttribute { analyzedAttribute };

   id : String;
   id() : String { id };

   index : Int;
   index() : Int { index };

   init(index_ : Int, analyzedAttribute_ : AnalyzedAttribute) : SELF_TYPE {{
      index <- index_;
      analyzedAttribute <- analyzedAttribute_;
      self;
   }};
};

class CoolasmMethod {
   containingType : CoolasmType;
   containingType() : CoolasmType { containingType };

   analyzedMethod : AnalyzedMethod;
   analyzedMethod() : AnalyzedMethod { analyzedMethod };

   asm : Collection;
   asm() : Collection { asm };
   setAsm(asm_ : Collection) : Collection { asm <- asm_ };

   id : String;
   id() : String { id };

   label : CoolasmLabel;
   label() : CoolasmLabel { label };

   index : Int;
   index() : Int { index };

   init(containingType_ : CoolasmType, index_ : Int, analyzedMethod_ : AnalyzedMethod) : SELF_TYPE {{
      containingType <- containingType_;
      index <- index_;
      analyzedMethod <- analyzedMethod_;
      id <- analyzedMethod.id();
      label <- new CoolasmLabel.init(containingType.name().concat(".").concat(id));
      self;
   }};
};

class CoolasmStringLabel inherits CoolasmLabel {
   value : String;
   value() : String { value };

   index : Int;
   index() : Int { index };

   initString(value_ : String, index_ : Int, name : String) : SELF_TYPE {{
      value <- value_;
      index <- index_;
      init(name);
   }};
};

class CoolasmErrorLabel inherits CoolasmLabel {
   stringLabel : CoolasmLabel;
   stringLabel() : CoolasmLabel { stringLabel };

   initError(stringLabel_ : CoolasmLabel, name : String) : SELF_TYPE {{
      stringLabel <- stringLabel_;
      init(name);
   }};
};

class CoolasmGenerator inherits AnalyzedExprVisitor {
   stringUtil : StringUtil <- new StringUtil;
   backslash : String <- stringUtil.backslash();
   doubleQuote : String <- stringUtil.doubleQuote();

   types : StringMap <- new StringListMap;
   typeList : Collection <- new LinkedList;

   objectType : CoolasmType;
   intType : CoolasmType;
   stringType : CoolasmType;
   boolType : CoolasmType;

   getType(type : AnalyzedType) : CoolasmType {
      case types.getWithString(type.name()) of x : CoolasmType => x; esac
   };

   analyzeType(type : CoolasmType) : Object {
      if type.analyze() then
         let analyzedType : AnalyzedType <- type.analyzedType() in
            {
               let inheritsType : CoolasmType <- getType(analyzedType.inheritsType()) in
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

   stringLabels : StringMap <- new StringListMap;
   stringLabelList : Collection <- new LinkedList;

   getStringLabelImpl(s : String) : CoolasmStringLabel {
      let label : Object <- stringLabels.getWithString(s) in
         if isvoid label then
            let index : Int <- stringLabelList.size(),
                  name : String <- "string.".concat(stringUtil.fromInt(index)),
                  label : CoolasmStringLabel <- new CoolasmStringLabel.initString(s, index, name) in
               {
                  stringLabels.putWithString(s, label);
                  stringLabelList.add(label);
                  label;
               }
         else
            case label of x : CoolasmStringLabel => x; esac
         fi
   };

   getStringLabel(s : String) : CoolasmLabel {
      let type : Object <- types.getWithString(s) in
         if isvoid type then
            getStringLabelImpl(s)
         else
            case type of x : CoolasmType => x.nameLabel(); esac
         fi
   };

   errorLabels : IntTreeMap <- new IntTreeMap;

   getErrorLabel(s : String) : CoolasmLabel {
      let stringLabel : CoolasmStringLabel <- getStringLabelImpl(s),
            index : Int <- stringLabel.index(),
            label : Object <- errorLabels.getWithInt(index) in
         if isvoid label then
            let name : String <- "error.".concat(stringUtil.fromInt(index)),
                  label : CoolasmLabel <- new CoolasmErrorLabel.initError(stringLabel, name) in
               {
                  errorLabels.putWithInt(index, label);
                  label;
               }
         else
            case label of x : CoolasmLabel => x; esac
         fi
   };

   getExceptionLabel(line : Int, s : String) : CoolasmLabel {
      getErrorLabel("ERROR: ".concat(stringUtil.fromInt(line)).concat(": Exception: ").concat(s).concat("\n"))
   };

   objectTypeIndex() : Int { 0 };
   objectAttributeOffset() : Int { 1 };

   stringValueIndex() : Int { objectAttributeOffset() };
   stringSize() : Int { stringValueIndex() + 1 };

   boolValueIndex() : Int { objectAttributeOffset() };

   typeDepthIndex() : Int { 0 };
   typeNameIndex() : Int { 1 };
   typeSizeIndex() : Int { 2 };
   typeNewIndex() : Int { 3 };
   typeDispatchOffset() : Int { 4 };

   spArgOffset() : Int { 1 };

   r0 : CoolasmReg <- new CoolasmReg.init(0);
   r1 : CoolasmReg <- new CoolasmReg.init(1);
   r2 : CoolasmReg <- new CoolasmReg.init(2);
   r3 : CoolasmReg <- new CoolasmReg.init(3);
   r4 : CoolasmReg <- new CoolasmReg.init(4);
   r5 : CoolasmReg <- new CoolasmReg.init(5);
   r6 : CoolasmReg <- new CoolasmReg.init(6);
   r7 : CoolasmReg <- new CoolasmReg.init(7);
   sp : CoolasmReg <- new CoolasmReg.init(8);
   fp : CoolasmReg <- new CoolasmReg.init(9);
   ra : CoolasmReg <- new CoolasmReg.init(10);

   li(dst : CoolasmReg, value : Int) : CoolasmLiInstr { new CoolasmLiInstr.init(dst, value) };
   mov(dst : CoolasmReg, src : CoolasmReg) : CoolasmMovInstr { new CoolasmMovInstr.init(dst, src) };
   add(dst : CoolasmReg, reg1 : CoolasmReg, reg2 : CoolasmReg) : CoolasmAddInstr { new CoolasmAddInstr.init(dst, reg1, reg2) };
   sub(dst : CoolasmReg, reg1 : CoolasmReg, reg2 : CoolasmReg) : CoolasmSubInstr { new CoolasmSubInstr.init(dst, reg1, reg2) };
   mul(dst : CoolasmReg, reg1 : CoolasmReg, reg2 : CoolasmReg) : CoolasmMulInstr { new CoolasmMulInstr.init(dst, reg1, reg2) };
   div(dst : CoolasmReg, reg1 : CoolasmReg, reg2 : CoolasmReg) : CoolasmDivInstr { new CoolasmDivInstr.init(dst, reg1, reg2) };
   jmp(label : CoolasmLabel) : CoolasmJmpInstr { new CoolasmJmpInstr.init(label) };
   bz(reg : CoolasmReg, label : CoolasmLabel) : CoolasmBzInstr { new CoolasmBzInstr.init(reg, label) };
   bnz(reg : CoolasmReg, label : CoolasmLabel) : CoolasmBnzInstr { new CoolasmBnzInstr.init(reg, label) };
   beq(reg1 : CoolasmReg, reg2 : CoolasmReg, label : CoolasmLabel) : CoolasmBeqInstr { new CoolasmBeqInstr.init(reg1, reg2, label) };
   blt(reg1 : CoolasmReg, reg2 : CoolasmReg, label : CoolasmLabel) : CoolasmBltInstr { new CoolasmBltInstr.init(reg1, reg2, label) };
   ble(reg1 : CoolasmReg, reg2 : CoolasmReg, label : CoolasmLabel) : CoolasmBleInstr { new CoolasmBleInstr.init(reg1, reg2, label) };
   callLabel(label : CoolasmLabel) : CoolasmCallLabelInstr { new CoolasmCallLabelInstr.init(label) };
   callReg(reg : CoolasmReg) : CoolasmCallRegInstr { new CoolasmCallRegInstr.init(reg) };
   return : CoolasmReturnInstr <- new CoolasmReturnInstr;
   push(reg : CoolasmReg) : CoolasmPushInstr { new CoolasmPushInstr.init(reg) };
   pop(reg : CoolasmReg) : CoolasmPopInstr { new CoolasmPopInstr.init(reg) };
   ld(dst : CoolasmReg, src : CoolasmReg, srcoff : Int) : CoolasmLdInstr { new CoolasmLdInstr.init(dst, src, srcoff) };
   st(dst : CoolasmReg, dstoff : Int, src : CoolasmReg) : CoolasmStInstr { new CoolasmStInstr.init(dst, dstoff, src) };
   la(dst : CoolasmReg, label : CoolasmLabel) : CoolasmLaInstr { new CoolasmLaInstr.init(dst, label) };
   alloc(dst : CoolasmReg, size : CoolasmReg) : CoolasmAllocInstr { new CoolasmAllocInstr.init(dst, size) };
   constantInteger(value : Int) : CoolasmConstantIntegerInstr { new CoolasmConstantIntegerInstr.init(value) };
   constantString(value : String) : CoolasmConstantStringInstr { new CoolasmConstantStringInstr.init(value) };
   constantLabel(label : CoolasmLabel) : CoolasmConstantLabelInstr { new CoolasmConstantLabelInstr.init(label) };
   syscall(s : String) : CoolasmSyscallInstr { new CoolasmSyscallInstr.init(s) };

   systemInstrs : Collection <- new LinkedList;

   labelStringCreate : CoolasmLabel;
   labelStringCreate() : CoolasmLabel {{
      if isvoid labelStringCreate then
         {
            labelStringCreate <- new CoolasmLabel.init("String..create");
            systemInstrs.add(labelStringCreate);
            systemInstrs.add(li(r0, stringSize()));
            systemInstrs.add(alloc(r0, r0));
            systemInstrs.add(la(r2, stringType.label()));
            systemInstrs.add(st(r0, objectTypeIndex(), r2).setComment("type"));
            systemInstrs.add(st(r0, stringValueIndex(), r1).setComment("value"));
            systemInstrs.add(return);
         }
      else false fi;

      labelStringCreate;
   }};

   labelStringCreateConstant : CoolasmLabel;
   labelStringCreateConstant() : CoolasmLabel {{
      if isvoid labelStringCreateConstant then
         {
            labelStringCreateConstant <- new CoolasmLabel.init("String..create.constant");
            systemInstrs.add(labelStringCreateConstant);
            systemInstrs.add(li(r0, stringSize()));
            systemInstrs.add(alloc(r0, r0));
            systemInstrs.add(la(r2, stringType.label()));
            systemInstrs.add(st(r0, objectTypeIndex(), r2).setComment("type"));
            systemInstrs.add(ld(r1, r1, 0));
            systemInstrs.add(st(r0, stringValueIndex(), r1).setComment("value"));
            systemInstrs.add(return);
         }
      else false fi;

      labelStringCreateConstant;
   }};

   labelInt0 : CoolasmLabel;
   labelInt0() : CoolasmLabel {{
      if isvoid labelInt0 then
         {
            labelInt0 <- new CoolasmLabel.init("Int..object.0");
            systemInstrs.add(labelInt0);
            systemInstrs.add(constantLabel(intType.label()));
            systemInstrs.add(constantInteger(0));
         }
      else false fi;

      labelInt0;
   }};

   labelStringEmpty : CoolasmLabel;
   labelStringEmpty() : CoolasmLabel {{
      if isvoid labelStringEmpty then
         {
            labelStringEmpty <- new CoolasmLabel.init("String..object.empty");
            systemInstrs.add(labelStringEmpty);
            systemInstrs.add(constantLabel(stringType.label()));
            systemInstrs.add(constantLabel(getStringLabel("")));
         }
      else false fi;

      labelStringEmpty;
   }};

   labelBoolFalse : CoolasmLabel;
   labelBoolFalse() : CoolasmLabel {{
      if isvoid labelBoolFalse then
         {
            labelBoolFalse <- new CoolasmLabel.init("Bool..false");
            systemInstrs.add(labelBoolFalse);
            systemInstrs.add(constantLabel(boolType.label()));
            systemInstrs.add(constantInteger(0));
         }
      else false fi;

      labelBoolFalse;
   }};

   labelBoolTrue : CoolasmLabel;
   labelBoolTrue() : CoolasmLabel {{
      if isvoid labelBoolTrue then
         {
            labelBoolTrue <- new CoolasmLabel.init("Bool..true");
            systemInstrs.add(labelBoolTrue);
            systemInstrs.add(constantLabel(boolType.label()));
            systemInstrs.add(constantInteger(2));
         }
      else false fi;

      labelBoolTrue;
   }};

   nextLabelId : Int;

   allocLabel() : CoolasmLabel {
      let id : Int <- nextLabelId in
         {
            nextLabelId <- nextLabelId + 1;
            new CoolasmLabel.init("label.".concat(stringUtil.fromInt(id)));
         }
   };

   instrs : Collection <- new LinkedList;

   addLabel(label : CoolasmLabel) : Object {
      instrs.add(label)
   };

   addInstr(instr : CoolasmInstr) : Object {
      instrs.add(instr)
   };

   addAllInstrs(instrs_ : Collection) : Object {
      instrs.addAll(instrs_)
   };

   generate(program : AnalyzedProgram) : CoolasmProgram {{
      -- TODO: initialize Object methods
      objectType <- new CoolasmType.initBasicObject(program.objectType(), objectAttributeOffset(), typeDispatchOffset());
      objectType.initNewLabel();

      -- Int, String, and Bool are the only type addresses less than Object.
      let analyzedIntType : AnalyzedType <- program.intType() in
         {
            intType <- new CoolasmType.initBasic(analyzedIntType);
            intType.setInheritsType(objectType);

            types.putWithString(intType.name(), intType);
            typeList.add(intType);
         };

      let analyzedStringType : AnalyzedType <- program.stringType() in
         {
            stringType <- new CoolasmType.initBasic(analyzedStringType);
            stringType.setInheritsType(objectType);

            -- TODO: initialize String methods
            types.putWithString(stringType.name(), stringType);
            typeList.add(stringType);
         };

      let analyzedBoolType : AnalyzedType <- program.boolType() in
         {
            boolType <- new CoolasmType.initBasic(analyzedBoolType);
            boolType.setInheritsType(objectType);

            types.putWithString(boolType.name(), boolType);
            typeList.add(boolType);
         };

      types.putWithString(objectType.name(), objectType);
      typeList.add(objectType);

      let analyzedIoType : AnalyzedType <- program.ioType(),
            ioType : CoolasmType <- new CoolasmType.initBasic(analyzedIoType) in
         {
            ioType.setInheritsType(objectType);
            ioType.initNewLabel();
            ioType.addMethod(analyzedIoType.getMethod("out_string")).setAsm(new LinkedList
                  .add(ld(r1, sp, spArgOffset()).setComment("arg1"))
                  .add(li(r2, stringValueIndex()))
                  .add(add(r1, r1, r2).setComment("attribute String.value"))
                  .add(syscall("IO.out_string"))
                  .add(return));

            -- TODO: initialize IO methods
            types.putWithString(ioType.name(), ioType);
            typeList.add(ioType);
         };

      -- Create CoolasmType
      let analyzedTypeIter : Iterator <- program.definedTypes().iterator() in
         while analyzedTypeIter.next() loop
            let analyzedType : AnalyzedType <- case analyzedTypeIter.get() of x : AnalyzedType => x; esac,
                  type : CoolasmType <- new CoolasmType.init(analyzedType) in
               {
                  type.initNewLabel();
                  types.putWithString(type.name(), type);
                  typeList.add(type);
               }
         pool;

      -- Set inheritsType, and create attributes and methods.
      let typeIter : Iterator <- typeList.iterator() in
         while typeIter.next() loop
            analyzeType(case typeIter.get() of x : CoolasmType => x; esac)
         pool;

      -- Generate program entry point.
      addLabel(new CoolasmLabel.init("start"));
      addInstr(li(r7, 0).setComment("reserve r7 as constant 0"));

      let mainMethod : AnalyzedMethod <- program.mainMethod(),
            mainType : AnalyzedType <- mainMethod.containingType(),
            type : CoolasmType <- getType(mainType),
            -- Manually build and analyze a "new Main.main()" expression
            -- to use as the program entry point.
            newExpr : AnalyzedExpr <- new AnalyzedNewExpr.init(0, mainType),
            dispatchExpr : AnalyzedExpr <- new AnalyzedDispatchExpr.init(0, mainMethod.returnType(), newExpr, mainMethod, true, new Collection) in
         dispatchExpr.accept(self);

      addInstr(syscall("exit"));

      -- Generate type methods.
      let iter : Iterator <- typeList.iterator() in
         while iter.next() loop
            let type : CoolasmType <- case iter.get() of x : CoolasmType => x; esac in
               let iter : Iterator <- type.definedMethods().iterator() in
                  while iter.next() loop
                     let method : CoolasmMethod <- case iter.get() of x : CoolasmMethod => x; esac in
                        {
                           addLabel(method.label());

                           let asm : Collection <- method.asm() in
                              if isvoid asm then
                                 {
                                    addInstr(push(fp));
                                    addInstr(mov(fp, sp));
                                    addInstr(push(ra));
                                    method.analyzedMethod().expr().accept(self);
                                    addInstr(pop(ra));
                                    addInstr(pop(fp));
                                    addInstr(return);
                                 }
                              else
                                 addAllInstrs(asm)
                              fi;
                        }
                  pool
         pool;

      -- Generate type tables.
      let iter : Iterator <- typeList.iterator() in
         while iter.next() loop
            let type : CoolasmType <- case iter.get() of x : CoolasmType => x; esac in
               {
                  -- ${Type}..type.hierarchy
                  -- Lay out the hierarchy in reverse prior to the type label.
                  addLabel(new CoolasmLabel.init(type.name().concat("..type.hierarchy")));
                  let typeIter : CoolasmType <- type in
                     while not typeIter = objectType loop
                        {
                           addInstr(constantLabel(type.label()));
                           typeIter <- typeIter.inheritsType();
                        }
                     pool;

                  -- ${Type}..type
                  addLabel(type.label());
                  addInstr(constantInteger(~type.inheritsDepth()).setComment("type hierarchy depth"));
                  addLabel(type.nameLabel());
                  addInstr(constantString(type.name()));
                  addInstr(constantInteger(type.allocSize()).setComment("alloc size"));
                  let newLabel : CoolasmLabel <- type.newLabel() in
                     if isvoid newLabel then
                        addInstr(constantInteger(0).setComment("new"))
                     else
                        addInstr(constantLabel(newLabel))
                     fi;

                  let iter : IntMapIterator <- type.methodDispatches().iterator() in
                     while iter.next() loop
                        let method : CoolasmMethod <- case iter.value() of x : CoolasmMethod => x; esac in
                           addInstr(constantLabel(method.label()))
                     pool;

                  -- ${Type}..new
                  let newLabel : CoolasmLabel <- type.newLabel() in
                     if not isvoid newLabel then
                        {
                           addLabel(newLabel);
                           generateNew(program, type);
                        }
                     else false fi;
               }
         pool;

      addAllInstrs(systemInstrs);

      -- Generate error handlers.
      if not errorLabels.size() = 0 then
         let labelError : CoolasmLabel <- new CoolasmLabel.init("error") in
            {
               let any : Bool,
                     iter : IntMapIterator <- errorLabels.iterator() in
                  while iter.next() loop
                     let label : CoolasmErrorLabel <- case iter.value() of x : CoolasmErrorLabel => x; esac in
                        {
                           if any then
                              addInstr(jmp(labelError))
                           else false fi;

                           addLabel(label);
                           addInstr(la(r0, label.stringLabel()));
                           any <- true;
                        }
                  pool;

               addLabel(labelError);
               addInstr(mov(r1, sp));
               addInstr(push(r0));
               addInstr(syscall("IO.out_string"));
               addInstr(syscall("exit"));
            }
      else false fi;

      -- Generate string constants.
      let iter : Iterator <- stringLabelList.iterator() in
         while iter.next() loop
            let label : CoolasmStringLabel <- case iter.get() of x : CoolasmStringLabel => x; esac in
               {
                  addLabel(label);
                  addInstr(constantString(label.value()));
               }
         pool;

      new CoolasmProgram.init(instrs);
   }};

   generateNew(program : AnalyzedProgram, type : CoolasmType) : Object {{
      addInstr(li(r0, type.allocSize()));
      addInstr(alloc(r0, r0));
      addInstr(la(r1, type.label()));
      addInstr(st(r0, objectTypeIndex(), r1).setComment("type"));

      let intDefaultInitReg : CoolasmReg,
            stringDefaultInitReg : CoolasmReg,
            boolDefaultInitReg : CoolasmReg,
            exprInit : Bool in
         {
            -- Determine if there are any string attributes.
            let iter : Iterator <- type.hierarchy().iterator() in
               while iter.next() loop
                  let type : CoolasmType <- case iter.get() of x : CoolasmType => x; esac in
                     let iter : Iterator <- type.definedAttributes().iterator() in
                        while iter.next() loop
                           let attr : CoolasmAttribute <- case iter.get() of x : CoolasmAttribute => x; esac,
                                 analyzedAttr : AnalyzedAttribute <- attr.analyzedAttribute(),
                                 type : AnalyzedType <- analyzedAttr.type() in
                              {
                                 if type = program.intType() then
                                    intDefaultInitReg <- r1
                                 else
                                    if type = program.stringType() then
                                       stringDefaultInitReg <- r2
                                    else
                                       if type = program.boolType() then
                                          boolDefaultInitReg <- r3
                                       else false fi
                                    fi
                                 fi;

                                 if not isvoid analyzedAttr.expr() then
                                    exprInit <- true
                                 else false fi;
                              }
                        pool
               pool;

            if not isvoid intDefaultInitReg then
               addInstr(la(intDefaultInitReg, labelInt0()))
            else false fi;

            if not isvoid stringDefaultInitReg then
               addInstr(la(stringDefaultInitReg, labelStringEmpty()))
            else false fi;

            if not isvoid boolDefaultInitReg then
               addInstr(la(boolDefaultInitReg, labelBoolFalse()))
            else false fi;

            -- Default initializations.
            let iter : Iterator <- type.hierarchy().iterator() in
               while iter.next() loop
                  let type : CoolasmType <- case iter.get() of x : CoolasmType => x; esac in
                     let iter : Iterator <- type.definedAttributes().iterator() in
                        while iter.next() loop
                           let attr : CoolasmAttribute <- case iter.get() of x : CoolasmAttribute => x; esac,
                                 analyzedAttr : AnalyzedAttribute <- attr.analyzedAttribute(),
                                 type : AnalyzedType <-  analyzedAttr.type(),
                                 reg : CoolasmReg <-
                                       if type = program.intType() then
                                          intDefaultInitReg
                                       else
                                          if type = program.stringType() then
                                             stringDefaultInitReg
                                          else
                                             if type = program.boolType() then
                                                boolDefaultInitReg
                                             else
                                                r7
                                             fi
                                          fi
                                       fi in
                              addInstr(st(r0, attr.index(), reg).setComment(attr.id()))
                        pool
               pool;

            -- Expression initializations.
            if exprInit then
               {
                  addInstr(push(r0));

                  let iter : Iterator <- type.hierarchy().iterator() in
                     while iter.next() loop
                        let type : CoolasmType <- case iter.get() of x : CoolasmType => x; esac in
                           let iter : Iterator <- type.definedAttributes().iterator() in
                              while iter.next() loop
                                 let attr : CoolasmAttribute <- case iter.get() of x : CoolasmAttribute => x; esac,
                                       expr : AnalyzedExpr <- attr.analyzedAttribute().expr() in
                                    if not isvoid expr then
                                       {
                                          expr.accept(self);
                                          addInstr(ld(r1, sp, 1));
                                          addInstr(st(r1, attr.index(), r0).setComment(attr.id()));
                                       }
                                    else false fi
                              pool
                     pool;

                  addInstr(pop(r0));
               }
            else false fi;
         };

      addInstr(return);
   }};

   visitBlock(expr : AnalyzedBlockExpr) : Object { new ObjectUtil.abortObject(self, "visitBlock: unimplemented") };

   visitIf(expr : AnalyzedIfExpr) : Object {
      let else_ : CoolasmLabel <- allocLabel(),
            fi_ : CoolasmLabel <- allocLabel() in
         {
            expr.expr().accept(self);
            addInstr(ld(r0, r0, boolValueIndex()).setComment("attribute Bool.value"));
            addInstr(bz(r0, else_).setComment("else"));

            expr.then_().accept(self);
            addInstr(jmp(fi_).setComment("fi"));

            addLabel(else_);
            expr.else_().accept(self);

            addLabel(fi_);
         }
   };

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

   visitNew(expr : AnalyzedNewExpr) : Object {
      let type : CoolasmType <- getType(expr.type()) in
         addInstr(callLabel(type.newLabel()))
   };

   visitDispatch(expr : AnalyzedDispatchExpr) : Object {
      let args : Collection <- expr.arguments() in
         {
            let argIter : Iterator <- args.iterator(),
                  index : Int in
               while argIter.next() loop
                  let expr : AnalyzedExpr <- case argIter.get() of x : AnalyzedExpr => x; esac in
                     {
                        expr.accept(self);
                        addInstr(push(r0).setComment("push arg".concat(stringUtil.fromInt(index + 1))));
                     }
               pool;

            expr.expr().accept(self);
            addInstr(bz(r0, getExceptionLabel(expr.line(), "dispatch on void")).setComment("dispatch on void"));

            let method : AnalyzedMethod <- expr.method(),
                  method : CoolasmMethod <- getType(method.containingType()).getMethod(method.id()) in
               if expr.static() then
                  addInstr(callLabel(method.label()))
               else
                  {
                     addInstr(ld(r1, r1, method.index()).setComment("method ".concat(method.label().name())));
                     addInstr(callReg(r1));
                  }
               fi;

            if not args.size() = 0 then
               {
                  addInstr(li(r1, args.size()));
                  addInstr(add(sp, sp, r1).setComment("pop arguments"));
               }
            else false fi;
         }
   };

   visitUnary(expr : AnalyzedUnaryExpr) : Object { new ObjectUtil.abortObject(self, "visitUnary: unimplemented") };
   visitBinary(expr : AnalyzedBinaryExpr) : Object { new ObjectUtil.abortObject(self, "visitBinary: unimplemented") };

   visitConstantBool(expr : AnalyzedConstantBoolExpr) : Object {
      if expr.value() then
         addInstr(la(r0, labelBoolTrue()))
      else
         addInstr(la(r0, labelBoolFalse()))
      fi
   };

   visitConstantInt(expr : AnalyzedConstantIntExpr) : Object { new ObjectUtil.abortObject(self, "visitConstantInt: unimplemented") };

   visitConstantString(expr : AnalyzedConstantStringExpr) : Object {{
      addInstr(la(r1, getStringLabel(expr.value())));
      addInstr(callLabel(labelStringCreateConstant()));
   }};
};
