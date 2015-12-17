class StringListMapNode {
   key : String;
   key() : String { key };

   value : Object;
   value() : Object { value };
   setValue(value_ : Object) : Object { value <- value_ };

   next : StringListMapNode;
   next() : StringListMapNode { next };
   setNext(next_ : StringListMapNode) : Object { next <- next_ };

   init(key_ : String, value_ : Object, next_ : StringListMapNode) : SELF_TYPE {{
      key <- key_;
      value <- value_;
      next <- next_;
      self;
   }};
};

class StringListMapIterator inherits StringMapIterator {
   head : StringListMapNode;
   current : StringListMapNode;

   init(head_ : StringListMapNode) : SELF_TYPE {{
      head <- head_;
      self;
   }};

   next() : Bool {{
      if isvoid current then
         {
            current <- head;
            head <- let void : StringListMapNode in void;
         }
      else
         current <- current.next()
      fi;

      not isvoid current;
   }};

   key() : String {
      current.key()
   };

   value() : Object {
      current.value()
   };
};

class StringListMap inherits StringMap {
   head : StringListMapNode;

   size : Int;
   size() : Int { size };

   iterator() : StringMapIterator {
      new StringListMapIterator.init(head)
   };

   findNode(key : String) : StringListMapNode {
      let node : StringListMapNode <- head,
            continue : Bool <- true in
         {
            while continue loop
               if isvoid node then
                  continue <- false
               else
                  if key = node.key() then
                     continue <- false
                  else
                     node <- node.next()
                  fi
               fi
            pool;

            node;
         }
   };

   putWithString(key : String, value : Object) : Object {
      let node : StringListMapNode <- findNode(key) in
         if isvoid node then
            {
               head <- new StringListMapNode.init(key, value, head);
               size <- size + 1;
               node;
            }
         else
            let oldValue : Object <- node.value() in
               {
                  node.setValue(value);
                  oldValue;
               }
         fi
   };

   putNewWithString(key : String, value : Object) : Object {
      let node : StringListMapNode <- findNode(key) in
         if isvoid node then
            {
               head <- new StringListMapNode.init(key, value, head);
               size <- size + 1;
               node;
            }
         else
            node.value()
         fi
   };

   getWithString(key : String) : Object {
      let node : StringListMapNode <- findNode(key) in
         if isvoid node then
            node
         else
            node.value()
         fi
   };
};
