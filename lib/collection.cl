class Iterator {
   next() : Bool { false };
   get() : Object { let void : Object in void };
};

class Iterable {
   iterator() : Iterator { new Iterator };
};

class Collection inherits Iterable {
   size() : Int { 0 };
   add(o : Object) : SELF_TYPE {{ new Object.abort(); self; }};
};

class StringMapIterator {
   next() : Bool { false };
   key() : String {{ new Object.abort(); ""; }};
   value() : Object { new Object.abort() };
};

class StringMap {
   iterator() : StringMapIterator { new StringMapIterator };
   getWithString(key : String) : Object { let void : Object in void };
   putWithString(key : String, value : Object) : Object { new Object.abort() };
   putNewWithString(key : String, value : Object) : Object { new Object.abort() };
};

class Comparator {
   compare(o1 : Object, o2 : Object) : Int {{ new Object.abort(); 0; }};
};
