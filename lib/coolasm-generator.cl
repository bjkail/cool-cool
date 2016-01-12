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
      methodDispatches.putAll(inheritsType_.methodDispatches());
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

   allocNextMethodIndex() : Int {
      let index : Int <- nextMethodIndex in
         {
            nextMethodIndex <- index + 1;
            index;
         }
   };

   addMethod(analyzedMethod : AnalyzedMethod) : CoolasmMethod {
      let id : String <- analyzedMethod.id(),
            index : Int <-
               if isvoid inheritsType then
                  allocNextMethodIndex()
               else
                  let old : CoolasmMethod <- inheritsType.getMethod(analyzedMethod.id()) in
                     if isvoid old then
                        allocNextMethodIndex()
                     else
                        old.index()
                     fi
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
      id <- analyzedAttribute_.id();
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

   program : AnalyzedProgram;

   types : StringMap <- new StringListMap;
   typeList : Collection <- new LinkedList;
   maxInheritsDepth : Int;

   objectType : CoolasmType;
   intType : CoolasmType;
   stringType : CoolasmType;
   boolType : CoolasmType;

   getType(type : AnalyzedType) : CoolasmType {
      case types.getWithString(type.name()) of x : CoolasmType => x; esac
   };

   getTypeAllowSelf(type : AnalyzedType) : CoolasmType {
      if type.isSelfType() then
         let void : CoolasmType in void
      else
         case types.getWithString(type.name()) of x : CoolasmType => x; esac
      fi
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

               let inheritsDepth : Int <- type.inheritsDepth() in
                  if maxInheritsDepth < inheritsDepth then
                     maxInheritsDepth <- inheritsDepth
                  else false fi;

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

   errorLabel : CoolasmLabel;

   errorLabel() : CoolasmLabel {
      if isvoid errorLabel then
         errorLabel <- new CoolasmLabel.init("error")
      else
         errorLabel
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

   getExceptionMessage(line : Int, s : String) : String {
      "ERROR: ".concat(stringUtil.fromInt(line)).concat(": Exception: ").concat(s)
   };

   getExceptionLabel(line : Int, s : String) : CoolasmLabel {
      getErrorLabel(getExceptionMessage(line, s).concat("\n"))
   };

   caseUnmatchedErrorLabels : IntTreeMap <- new IntTreeMap;

   getCaseUnmatchedExceptionLabel(line : Int) : CoolasmLabel {
      let stringLabel : CoolasmStringLabel <- getStringLabelImpl(getExceptionMessage(line, "case branch not matched for type '")),
            index : Int <- stringLabel.index(),
            label : Object <- caseUnmatchedErrorLabels.getWithInt(index) in
         if isvoid label then
            let name : String <- "error.case.".concat(stringUtil.fromInt(index)),
                  label : CoolasmLabel <- new CoolasmErrorLabel.initError(stringLabel, name) in
               {
                  caseUnmatchedErrorLabels.putWithInt(index, label);
                  label;
               }
         else
            case label of x : CoolasmLabel => x; esac
         fi
   };

   objectTypeIndex() : Int { 0 };
   objectAttributeOffset() : Int { 1 };

   -- Index of value attribute for Int, String, and Bool.
   valueIndex() : Int { objectAttributeOffset() };

   intValueIndex() : Int { valueIndex() };
   intSize() : Int { intValueIndex() + 1 };

   stringValueIndex() : Int { valueIndex() };
   stringSize() : Int { stringValueIndex() + 1 };

   boolValueIndex() : Int { valueIndex() };

   typeDepthIndex() : Int { 0 };
   typeNameIndex() : Int { 1 };
   typeSizeIndex() : Int { 2 };
   typeNewIndex() : Int { 3 };
   typeDispatchOffset() : Int { 4 };

   numArgs : Int;

   -- Frameless stack layout:
   --  arg0
   --  arg1
   --  ...
   --  argN-1
   --  void         <-- sp

   spFramelessArgOffset(n : Int) : Int { numArgs - n };

   beginFramelessMethod(method : CoolasmMethod) : CoolasmMethod {{
      numArgs <- method.analyzedMethod().formalTypes().size();
      method;
   }};

   -- Stack layout:
   --   arg0
   --   arg1
   --   ...
   --   argN-1
   --   saved fp
   --   saved ra    <-- fp
   --   saved self
   --   var0
   --   var1
   --   ...
   --   varN-1
   --   void        <-- sp

   fpArgOffset(n : Int) : Int { 1 + numArgs - n };
   fpSelfOffset() : Int { ~1 };
   fpVarOffset(n : Int) : Int { ~2 - n };

   beginMethod(method : CoolasmMethod) : Object {{
      addInstr(push(fp));
      addInstr(mov(fp, sp));
      addInstr(push(ra));
      addInstr(push(r0).setComment("save self"));
      beginFramelessMethod(method);
   }};

   endMethod() : Object {{
      addInstr(pop(r1).setComment("unsave self"));
      addInstr(pop(ra));
      addInstr(pop(fp));
      addInstr(return);
   }};

   r0 : CoolasmReg <- new CoolasmReg.init(0);
   r0() : CoolasmReg { r0 };
   r1 : CoolasmReg <- new CoolasmReg.init(1);
   r1() : CoolasmReg { r1 };
   r2 : CoolasmReg <- new CoolasmReg.init(2);
   r2() : CoolasmReg { r2 };
   r3 : CoolasmReg <- new CoolasmReg.init(3);
   r3() : CoolasmReg { r3 };
   r4 : CoolasmReg <- new CoolasmReg.init(4);
   r4() : CoolasmReg { r4 };
   r5 : CoolasmReg <- new CoolasmReg.init(5);
   r5() : CoolasmReg { r5 };
   r6 : CoolasmReg <- new CoolasmReg.init(6);
   r6() : CoolasmReg { r6 };
   r7 : CoolasmReg <- new CoolasmReg.init(7);
   r7() : CoolasmReg { r7 };
   sp : CoolasmReg <- new CoolasmReg.init(8);
   sp() : CoolasmReg { sp };
   fp : CoolasmReg <- new CoolasmReg.init(9);
   fp() : CoolasmReg { fp };
   ra : CoolasmReg <- new CoolasmReg.init(10);
   ra() : CoolasmReg { ra };

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

   labelIntCreate : CoolasmLabel;
   labelIntCreate() : CoolasmLabel {{
      if isvoid labelIntCreate then
         {
            labelIntCreate <- new CoolasmLabel.init("Int..create");
            systemInstrs.add(labelIntCreate);
            systemInstrs.add(li(r0, intSize()));
            systemInstrs.add(alloc(r0, r0));
            systemInstrs.add(la(r2, intType.label()));
            systemInstrs.add(st(r0, objectTypeIndex(), r2).setComment("type"));
            systemInstrs.add(st(r0, intValueIndex(), r1).setComment("value"));
            systemInstrs.add(return);
         }
      else false fi;

      labelIntCreate;
   }};

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
            systemInstrs.add(ld(r1, r1, 0));
            systemInstrs.add(st(r0, stringValueIndex(), r1).setComment("value"));
            systemInstrs.add(return);
         }
      else false fi;

      labelStringCreate;
   }};

   labelStringEmpty : CoolasmLabel;
   labelStringEmpty() : CoolasmLabel {{
      if isvoid labelStringEmpty then
         {
            labelStringEmpty <- new CoolasmLabel.init("String..object.empty");
            systemInstrs.add(labelStringEmpty);
            systemInstrs.add(constantLabel(stringType.label()));
            systemInstrs.add(constantString(""));
         }
      else false fi;

      labelStringEmpty;
   }};

   labelBoolFalse : CoolasmLabel;
   labelBoolTrue : CoolasmLabel;

   -- labelBoolFalse() + labelBoolTrueOffset() = labelBoolTrue()
   labelBoolTrueOffset() : Int { 2 };

   initLabelBool() : Object {{
      if isvoid labelBoolFalse then
         {
            labelBoolFalse <- new CoolasmLabel.init("Bool..false");
            systemInstrs.add(labelBoolFalse);
            systemInstrs.add(constantLabel(boolType.label()));
            systemInstrs.add(constantInteger(0));

            labelBoolTrue <- new CoolasmLabel.init("Bool..true");
            systemInstrs.add(labelBoolTrue);
            systemInstrs.add(constantLabel(boolType.label()));
            systemInstrs.add(constantInteger(labelBoolTrueOffset()));
         }
      else false fi;
   }};

   labelBoolFalse() : CoolasmLabel {{
      initLabelBool();
      labelBoolFalse;
   }};

   labelBoolTrue() : CoolasmLabel {{
      initLabelBool();
      labelBoolTrue;
   }};

   labelObjectEqual : CoolasmLabel;
   labelObjectEqual() : CoolasmLabel {{
      labelBoolFalse();
      labelBoolTrue();

      if isvoid labelObjectEqual then
         let labelFalse : CoolasmLabel <- allocLabel(),
               labelTrue : CoolasmLabel <- allocLabel() in
            {
               labelObjectEqual <- new CoolasmLabel.init("Object..equal");
               systemInstrs.add(labelObjectEqual);

               systemInstrs.add(beq(r1, r0, labelTrue).setComment("equal"));
               systemInstrs.add(bz(r1, labelFalse).setComment("left void"));
               systemInstrs.add(bz(r0, labelFalse).setComment("right void"));
               systemInstrs.add(la(r2, boolType.label()));
               systemInstrs.add(ld(r3, r1, objectTypeIndex()).setComment("type"));
               systemInstrs.add(ble(r2, r3, labelFalse).setComment("left non-value"));
               systemInstrs.add(ld(r3, r0, objectTypeIndex()).setComment("type"));
               systemInstrs.add(ble(r2, r3, labelFalse).setComment("right non-value"));
               -- At this point, r1/r0 are either Int or String, so we can load
               -- their values (integer or raw string) and compare.
               systemInstrs.add(ld(r1, r1, valueIndex()).setComment("attribute basic value"));
               systemInstrs.add(ld(r0, r0, valueIndex()).setComment("attribute basic value"));
               systemInstrs.add(beq(r1, r0, labelTrue).setComment("value equal"));

               systemInstrs.add(labelFalse);
               systemInstrs.add(la(r0, labelBoolFalse()));
               systemInstrs.add(return);

               systemInstrs.add(labelTrue);
               systemInstrs.add(la(r0, labelBoolTrue()));
               systemInstrs.add(return);
            }
      else false fi;

      labelObjectEqual;
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

   generate(program_ : AnalyzedProgram) : CoolasmProgram {{
      program <- program_;

      -- TODO: initialize Object methods
      let analyzedObjectType : AnalyzedType <- program.objectType() in
         {
            objectType <- new CoolasmType.initBasicObject(program.objectType(), objectAttributeOffset(), typeDispatchOffset());
            objectType.initNewLabel();

            let method : CoolasmMethod <- beginFramelessMethod(objectType.addMethod(analyzedObjectType.getMethod("abort"))) in
               method.setAsm(new LinkedList
                     .add(la(r1, getStringLabel("abort\n")))
                     .add(syscall("IO.out_string"))
                     .add(syscall("exit")));
         };

      -- labelObjectEqual relies on {Int,String} < Bool < {other}.
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

            let method : CoolasmMethod <- beginFramelessMethod(ioType.addMethod(analyzedIoType.getMethod("out_string"))) in
               method.setAsm(new LinkedList
                     .add(ld(r1, sp, spFramelessArgOffset(0)).setComment("arg0"))
                     .add(li(r2, stringValueIndex()))
                     .add(add(r1, r1, r2).setComment("attribute String.value"))
                     .add(syscall("IO.out_string"))
                     .add(return));

            let method : CoolasmMethod <- beginFramelessMethod(ioType.addMethod(analyzedIoType.getMethod("out_int"))) in
               method.setAsm(new LinkedList
                     .add(ld(r1, sp, spFramelessArgOffset(0)).setComment("arg0"))
                     .add(ld(r1, r1, intValueIndex()).setComment("attribute Int.value"))
                     .add(syscall("IO.out_int"))
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
                                    beginMethod(method);
                                    method.analyzedMethod().expr().accept(self);
                                    endMethod();
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

                  let i : Int <- type.inheritsDepth() in
                     while i < maxInheritsDepth loop
                        {
                           addInstr(constantInteger(0));
                           i <- i + 1;
                        }
                     pool;

                  let typeIter : CoolasmType <- type in
                     while not typeIter = objectType loop
                        {
                           addInstr(constantLabel(typeIter.label()));
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
                           generateNew(type);
                        }
                     else false fi;
               }
         pool;

      addAllInstrs(systemInstrs);

      -- Generate error handlers.
      let errorLabel : CoolasmLabel <- errorLabel(),
            iter : IntMapIterator <- errorLabels.iterator() in
         while iter.next() loop
            let label : CoolasmErrorLabel <- case iter.value() of x : CoolasmErrorLabel => x; esac in
               {
                  addLabel(label);
                  addInstr(la(r1, label.stringLabel()));
                  addInstr(jmp(errorLabel));
               }
         pool;

      if not caseUnmatchedErrorLabels.size() = 0 then
         let caseErrorLabel : CoolasmLabel <- new CoolasmLabel.init("error.case"),
               iter : IntMapIterator <- caseUnmatchedErrorLabels.iterator() in
            {
               while iter.next() loop
                  let label : CoolasmErrorLabel <- case iter.value() of x : CoolasmErrorLabel => x; esac in
                     {
                        addLabel(label);
                        addInstr(la(r1, label.stringLabel()));
                        addInstr(jmp(caseErrorLabel));
                     }
               pool;

               addLabel(caseErrorLabel);
               addInstr(li(r3, typeNameIndex()));
               addInstr(add(r2, r2, r3));
               addInstr(syscall("String.concat"));
               addInstr(la(r2, getStringLabel("'\n")));
               addInstr(syscall("String.concat"));
               addInstr(jmp(errorLabel()));
            }
      else false fi;

      if not isvoid errorLabel then
         {
            addLabel(errorLabel);
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

   generateNew(type : CoolasmType) : Object {{
      addInstr(li(r0, type.allocSize()));
      addInstr(alloc(r0, r0));
      addInstr(la(r1, type.label()));
      addInstr(st(r0, objectTypeIndex(), r1).setComment("type"));

      let varInitGen : CoolasmDefaultInitGenerator <- new CoolasmDefaultInitGenerator.init(self, program),
            exprInit : Bool in
         {
            -- Determine if there are any string attributes.
            let iter : Iterator <- type.hierarchy().iterator() in
               while iter.next() loop
                  let type : CoolasmType <- case iter.get() of x : CoolasmType => x; esac in
                     let iter : Iterator <- type.definedAttributes().iterator() in
                        while iter.next() loop
                           let attr : CoolasmAttribute <- case iter.get() of x : CoolasmAttribute => x; esac,
                                 analyzedAttr : AnalyzedAttribute <- attr.analyzedAttribute() in
                              {
                                 varInitGen.add(analyzedAttr.type());

                                 if not isvoid analyzedAttr.expr() then
                                    exprInit <- true
                                 else false fi;
                              }
                        pool
               pool;

            varInitGen.generateSetup();

            -- Default initializations.
            let iter : Iterator <- type.hierarchy().iterator() in
               while iter.next() loop
                  let type : CoolasmType <- case iter.get() of x : CoolasmType => x; esac in
                     let iter : Iterator <- type.definedAttributes().iterator() in
                        while iter.next() loop
                           let attr : CoolasmAttribute <- case iter.get() of x : CoolasmAttribute => x; esac,
                                 analyzedAttr : AnalyzedAttribute <- attr.analyzedAttribute(),
                                 type : AnalyzedType <-  analyzedAttr.type(),
                                 reg : CoolasmReg <- varInitGen.getReg(type) in
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

   visitBlock(expr : AnalyzedBlockExpr) : Object {
      let iter : Iterator <- expr.exprs().iterator() in
         while iter.next() loop
            let expr : AnalyzedExpr <- case iter.get() of x : AnalyzedExpr => x; esac in
               expr.accept(self)
         pool
   };

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

   visitLet(expr : AnalyzedLetExpr) : Object {
      let vars : Collection <- expr.vars(),
            numVars : Int <- vars.size() in
         {
            -- Reserve frame pointer slots.
            addInstr(li(r1, ~numVars));
            addInstr(add(sp, sp, r1).setComment("reserve let vars"));

            -- Generate variable initializations.
            let varInitGen : CoolasmDefaultInitGenerator <- new CoolasmDefaultInitGenerator.init(self, program) in
               {
                  let iter : Iterator <- vars.iterator() in
                     while iter.next() loop
                        let var : AnalyzedLetVar <- case iter.get() of x : AnalyzedLetVar => x; esac in
                           if isvoid var.expr() then
                              varInitGen.add(var.object().type())
                           else false fi
                     pool;

                  varInitGen.generateSetup();

                  let iter : Iterator <- vars.iterator() in
                     while iter.next() loop
                        let var : AnalyzedLetVar <- case iter.get() of x : AnalyzedLetVar => x; esac in
                           if isvoid var.expr() then
                              let object : AnalyzedVarObject <- var.object() in
                                 addInstr(st(fp, fpVarOffset(object.index()), varInitGen.getReg(object.type())))
                           else false fi
                     pool;

                  let iter : Iterator <- vars.iterator() in
                     while iter.next() loop
                        let var : AnalyzedLetVar <- case iter.get() of x : AnalyzedLetVar => x; esac,
                              object : AnalyzedVarObject <- var.object(),
                              expr : AnalyzedExpr <- var.expr() in
                           if not isvoid expr then
                              {
                                 expr.accept(self);
                                 addInstr(st(fp, fpVarOffset(object.index()), r0));
                              }
                           else false fi
                     pool;
               };

            -- Generate let expression.
            expr.expr().accept(self);

            -- Unreserve frame pointer slots.
            addInstr(li(r1, numVars));
            addInstr(add(sp, sp, r1).setComment("unreserve let vars"));
         }
   };

   visitCase(expr : AnalyzedCaseExpr) : Object {{
      expr.expr().accept(self);
      addInstr(push(r0).setComment("save case var"));
      addInstr(bz(r0, getExceptionLabel(expr.line(), "case on void")).setComment("case on void"));

      -- Use r2 for getCaseUnmatchedExceptionLabel.
      addInstr(ld(r2, r0, objectTypeIndex()).setComment("type"));

      let branches : LinkedList <- new LinkedList,
            labels : Collection <- new LinkedList,
            objectBranch : Bool,
            esac_ : CoolasmLabel <- allocLabel() in
         {
            branches.addAll(expr.branches());
            branches.sort(new CoolasmGeneratorCaseBranchComparator);

            let lastInheritsDepth : Int <- ~1,
                  iter : Iterator <- branches.iterator() in
               while iter.next() loop
                  let branch : AnalyzedCaseBranch <- case iter.get() of x : AnalyzedCaseBranch => x; esac,
                        checkType : AnalyzedType <- branch.checkType() in
                     if checkType = program.objectType() then
                        {
                           -- The Object "default" branch.
                           objectBranch <- true;
                           branch.expr().accept(self);
                           addInstr(jmp(esac_));
                        }
                     else
                        {
                           -- Load the nth hierarchy type of the target type.
                           let inheritsDepth : Int <- checkType.inheritsDepth() in
                              if not inheritsDepth = lastInheritsDepth then
                                 {
                                    addInstr(ld(r1, r2, ~inheritsDepth));
                                    lastInheritsDepth <- inheritsDepth;
                                 }
                              else false fi;

                           -- Test the hierarchy type with the branch type.
                           let label : CoolasmLabel <- allocLabel().setComment("case ".concat(checkType.name())) in
                              {
                                 addInstr(la(r3, getType(checkType).label()));
                                 addInstr(beq(r1, r3, label));
                                 labels.add(label);
                              };
                        }
                     fi
               pool;

            if not objectBranch then
               addInstr(jmp(getCaseUnmatchedExceptionLabel(expr.line())))
            else false fi;

            let iter : Iterator <- branches.iterator(),
                  labelIter : Iterator <- labels.iterator() in
               while iter.next() loop
                  let branch : AnalyzedCaseBranch <- case iter.get() of x : AnalyzedCaseBranch => x; esac in
                     if not branch.checkType() = program.objectType() then
                        {
                           labelIter.next();
                           let label : CoolasmLabel <- case labelIter.get() of x : CoolasmLabel => x; esac in
                              {
                                 addLabel(label);
                                 branch.expr().accept(self);
                                 addInstr(jmp(esac_));
                              };
                        }
                     else false fi
               pool;

            addLabel(esac_);
            addInstr(pop(r1).setComment("unsave case var"));
         };
   }};

   visitArgumentAssignment(object : AnalyzedArgumentObject, expr : AnalyzedExpr) : Object {{
      expr.accept(self);
      let index : Int <- object.index() in
         addInstr(st(fp, fpArgOffset(index), r0).setComment("arg".concat(stringUtil.fromInt(index))));
   }};

   visitVarAssignment(object : AnalyzedVarObject, expr : AnalyzedExpr) : Object {{
      expr.accept(self);
      let index : Int <- object.index() in
         addInstr(st(fp, fpVarOffset(index), r0).setComment("var".concat(stringUtil.fromInt(index))));
   }};

   visitAttributeAssignment(object : AnalyzedAttributeObject, expr : AnalyzedExpr) : Object {
      let attr : AnalyzedAttribute <- object.attribute(),
            type : CoolasmType <- getType(attr.containingType()),
            id : String <- attr.id(),
            attr : CoolasmAttribute <- type.getAttribute(id) in
         {
            expr.accept(self);
            addInstr(ld(r1, fp, fpSelfOffset()).setComment("self"));
            addInstr(st(r1, attr.index(), r0).setComment(type.name().concat(".").concat(id)));
         }
   };

   visitSelf(object : AnalyzedSelfObject) : Object {
      addInstr(ld(r0, fp, fpSelfOffset()).setComment("self"))
   };

   visitArgument(object : AnalyzedArgumentObject) : Object {
      let index : Int <- object.index() in
         addInstr(ld(r0, fp, fpArgOffset(index)).setComment("arg".concat(stringUtil.fromInt(index))))
   };

   visitVar(object : AnalyzedVarObject) : Object {
      let index : Int <- object.index() in
         addInstr(ld(r0, fp, fpVarOffset(index)).setComment("var".concat(stringUtil.fromInt(index))))
   };

   visitAttribute(object : AnalyzedAttributeObject) : Object {
      let attr : AnalyzedAttribute <- object.attribute(),
            type : CoolasmType <- getType(attr.containingType()),
            id : String <- attr.id(),
            attr : CoolasmAttribute <- type.getAttribute(id) in
         {
            addInstr(ld(r0, fp, fpSelfOffset()).setComment("self"));
            addInstr(ld(r0, r0, attr.index()).setComment(type.name().concat(".").concat(id)));
         }
   };

   visitNew(expr : AnalyzedNewExpr) : Object {
      let type : CoolasmType <- getTypeAllowSelf(expr.type()) in
         if isvoid type then
            {
               addInstr(ld(r0, fp, fpSelfOffset()).setComment("self"));
               addInstr(ld(r0, r0, objectTypeIndex()).setComment("type"));
               addInstr(ld(r0, r0, typeNewIndex()).setComment("new SELF_TYPE"));
               addInstr(callReg(r0));
            }
         else
            if type = intType then
               addInstr(la(r0, labelInt0()))
            else
               if type = stringType then
                  addInstr(la(r0, labelStringEmpty()))
               else
                  if type = boolType then
                     addInstr(la(r0, labelBoolFalse()))
                  else
                     addInstr(callLabel(type.newLabel()))
                  fi
               fi
            fi
         fi
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
                        addInstr(push(r0).setComment("push arg".concat(stringUtil.fromInt(index))));
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
                     addInstr(ld(r1, r0, objectTypeIndex()).setComment("type"));
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

   visitUnary(expr : AnalyzedUnaryExpr) : Object {{
      expr.expr().accept(self);

      let op : String <- expr.op() in
         if op = "isvoid" then
            let labelIsvoid : CoolasmLabel <- allocLabel(),
                  labelEnd : CoolasmLabel <- allocLabel() in
               {
                  addInstr(bz(r0, labelIsvoid).setComment("isvoid"));
                  addInstr(la(r0, labelBoolFalse()));
                  addInstr(jmp(labelEnd));

                  addLabel(labelIsvoid);
                  addInstr(la(r0, labelBoolTrue()));

                  addLabel(labelEnd);
               }
         else
            if op = "~" then
               {
                  addInstr(ld(r0, r0, intValueIndex()).setComment("attribute Int.value"));
                  addInstr(sub(r1, r7, r0).setComment("complement"));
                  addInstr(callLabel(labelIntCreate()));
               }
            else
               if op = "not" then
                  {
                     -- value is either 0 (false) or 1 (true)
                     -- result = labelBoolTrue - value
                     addInstr(ld(r0, r0, boolValueIndex()).setComment("attribute Bool.value"));
                     addInstr(la(r1, labelBoolTrue()));
                     addInstr(sub(r0, r1, r0).setComment("not"));
                  }
               else new ObjectUtil.abortObject(self, "visitUnary: unimplemented ".concat(op)) fi
            fi
         fi;
   }};

   visitBinary(expr : AnalyzedBinaryExpr) : Object {{
      expr.left().accept(self);
      addInstr(push(r0).setComment("save left"));
      expr.right().accept(self);
      addInstr(pop(r1).setComment("unsave left"));

      let op : String <- expr.op() in
         if op = "+" then
            {
               addInstr(ld(r1, r1, intValueIndex()).setComment("attribute Int.value"));
               addInstr(ld(r0, r0, intValueIndex()).setComment("attribute Int.value"));
               addInstr(add(r1, r1, r0));
               addInstr(callLabel(labelIntCreate()));
            }
         else
            if op = "-" then
               {
                  addInstr(ld(r1, r1, intValueIndex()).setComment("attribute Int.value"));
                  addInstr(ld(r0, r0, intValueIndex()).setComment("attribute Int.value"));
                  addInstr(sub(r1, r1, r0));
                  addInstr(callLabel(labelIntCreate()));
               }
            else
               if op = "*" then
                  {
                     addInstr(ld(r1, r1, intValueIndex()).setComment("attribute Int.value"));
                     addInstr(ld(r0, r0, intValueIndex()).setComment("attribute Int.value"));
                     addInstr(mul(r1, r1, r0));
                     addInstr(callLabel(labelIntCreate()));
                  }
               else
                  if op = "/" then
                     {
                        addInstr(ld(r1, r1, intValueIndex()).setComment("attribute Int.value"));
                        addInstr(ld(r0, r0, intValueIndex()).setComment("attribute Int.value"));
                        addInstr(bz(r0, getExceptionLabel(expr.line(), "divide by 0")).setComment("divide by 0"));
                        addInstr(div(r1, r1, r0));
                        addInstr(callLabel(labelIntCreate()));
                     }
                  else
                     if op = "=" then
                        let leftType : CoolasmType <- getTypeAllowSelf(expr.left().type()),
                              rightType : CoolasmType <- getTypeAllowSelf(expr.right().type()) in
                           if if leftType = objectType then
                                 true
                              else
                                 rightType = objectType
                              fi
                           then
                              addInstr(callLabel(labelObjectEqual()))
                           else
                              let labelTrue : CoolasmLabel <- allocLabel(),
                                    labelEnd : CoolasmLabel <- allocLabel() in
                                 {
                                    if if leftType = intType then
                                          true
                                       else
                                          leftType = stringType
                                       fi
                                    then
                                       {
                                          addInstr(ld(r1, r1, valueIndex()).setComment("attribute ".concat(leftType.name()).concat(" value")));
                                          addInstr(ld(r0, r0, valueIndex()).setComment("attribute ".concat(leftType.name()).concat(" value")));
                                       }
                                    else false fi;

                                    addInstr(beq(r1, r0, labelTrue).setComment("equal"));

                                    addInstr(la(r0, labelBoolFalse()));
                                    addInstr(jmp(labelEnd));

                                    addLabel(labelTrue);
                                    addInstr(la(r0, labelBoolTrue()));

                                    addLabel(labelEnd);
                                 }
                           fi
                     else
                        if op = "<" then
                           let labelTrue : CoolasmLabel <- allocLabel(),
                                 labelEnd : CoolasmLabel <- allocLabel() in
                              {
                                 addInstr(ld(r1, r1, intValueIndex()).setComment("attribute Int.value"));
                                 addInstr(ld(r0, r0, intValueIndex()).setComment("attribute Int.value"));
                                 addInstr(blt(r1, r0, labelTrue).setComment("less"));

                                 addInstr(la(r0, labelBoolFalse()));
                                 addInstr(jmp(labelEnd));

                                 addLabel(labelTrue);
                                 addInstr(la(r0, labelBoolTrue()));

                                 addLabel(labelEnd);
                              }
                        else
                           if op = "<=" then
                              let labelTrue : CoolasmLabel <- allocLabel(),
                                    labelEnd : CoolasmLabel <- allocLabel() in
                                 {
                                    addInstr(ld(r1, r1, intValueIndex()).setComment("attribute Int.value"));
                                    addInstr(ld(r0, r0, intValueIndex()).setComment("attribute Int.value"));
                                    addInstr(ble(r1, r0, labelTrue).setComment("less"));

                                    addInstr(la(r0, labelBoolFalse()));
                                    addInstr(jmp(labelEnd));

                                    addLabel(labelTrue);
                                    addInstr(la(r0, labelBoolTrue()));

                                    addLabel(labelEnd);
                                 }
                           else new ObjectUtil.abortObject(self, "visitBinary: unimplemented ".concat(op)) fi
                        fi
                     fi
                  fi
               fi
            fi
         fi;
   }};

   visitConstantBool(expr : AnalyzedConstantBoolExpr) : Object {
      if expr.value() then
         addInstr(la(r0, labelBoolTrue()))
      else
         addInstr(la(r0, labelBoolFalse()))
      fi
   };

   visitConstantInt(expr : AnalyzedConstantIntExpr) : Object {{
      addInstr(li(r1, expr.value()));
      addInstr(callLabel(labelIntCreate()));
   }};

   visitConstantString(expr : AnalyzedConstantStringExpr) : Object {{
      addInstr(la(r1, getStringLabel(expr.value())));
      addInstr(callLabel(labelStringCreate()));
   }};
};

class CoolasmDefaultInitGenerator {
   gen : CoolasmGenerator;
   program : AnalyzedProgram;

   init(gen_ : CoolasmGenerator, program_ : AnalyzedProgram) : SELF_TYPE {{
      gen <- gen_;
      program <- program_;
      self;
   }};

   intDefaultInitReg : CoolasmReg;
   stringDefaultInitReg : CoolasmReg;
   boolDefaultInitReg : CoolasmReg;

   add(type : AnalyzedType) : Object {
      if type = program.intType() then
         intDefaultInitReg <- gen.r1()
      else
         if type = program.stringType() then
            stringDefaultInitReg <- gen.r2()
         else
            if type = program.boolType() then
               boolDefaultInitReg <- gen.r3()
            else false fi
         fi
      fi
   };

   generateSetup() : Object {{
      if not isvoid intDefaultInitReg then
         gen.addInstr(gen.la(intDefaultInitReg, gen.labelInt0()))
      else false fi;

      if not isvoid stringDefaultInitReg then
         gen.addInstr(gen.la(stringDefaultInitReg, gen.labelStringEmpty()))
      else false fi;

      if not isvoid boolDefaultInitReg then
         gen.addInstr(gen.la(boolDefaultInitReg, gen.labelBoolFalse()))
      else false fi;
   }};

   getReg(type : AnalyzedType) : CoolasmReg {
      if type = program.intType() then
         intDefaultInitReg
      else
         if type = program.stringType() then
            stringDefaultInitReg
         else
            if type = program.boolType() then
               boolDefaultInitReg
            else
               gen.r7()
            fi
         fi
      fi
   };
};

class CoolasmGeneratorCaseBranchComparator inherits Comparator {
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
               ~1
            fi
         fi
   };
};
