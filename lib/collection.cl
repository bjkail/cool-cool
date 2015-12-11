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

class StringMap {
   getWithString(key : String) : Object { let void : Object in void };
   putWithString(key : String, value : Object) : Object { new Object.abort() };
};
