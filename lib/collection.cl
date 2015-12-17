class Iterator {
   next() : Bool { false };
   get() : Object { let void : Object in void };
};

class Iterable {
   iterator() : Iterator { new Iterator };
};

class Collection inherits Iterable {
   size() : Int { 0 };
   add(o : Object) : SELF_TYPE {{ new ObjectUtil.abortObject(self, "add: unimplemented"); self; }};
};

class StringMapIterator {
   next() : Bool { false };
   key() : String { new ObjectUtil.abortString(self, "key unimplemented") };
   value() : Object { new ObjectUtil.abortObject(self, "value unimplemented") };
};

class StringMap {
   size() : Int { 0 };
   iterator() : StringMapIterator { new StringMapIterator };
   getWithString(key : String) : Object { let void : Object in void };
   putWithString(key : String, value : Object) : Object { new ObjectUtil.abortObject(self, "putWithString: unimplemented") };
   putNewWithString(key : String, value : Object) : Object { new ObjectUtil.abortObject(self, "putNewWithString: unimplemented") };
};

class Comparator {
   compare(o1 : Object, o2 : Object) : Int { new ObjectUtil.abortInt(self, "compare: unimplemented") };
};
