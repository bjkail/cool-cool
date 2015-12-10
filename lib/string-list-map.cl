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

class StringListMap inherits StringMap {
   head : StringListMapNode;

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

   getWithString(key : String) : Object {
      let node : StringListMapNode <- findNode(key) in
         if isvoid node then
            node
         else
            node.value()
         fi
   };
};
