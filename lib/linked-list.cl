class LinkedListNode {
   value : Object;
   next : LinkedListNode;

   init(value_ : Object) : SELF_TYPE {{
      value <- value_;
      self;
   }};

   value() : Object { value };
   next() : LinkedListNode { next };

   setNext(next_ : LinkedListNode) : SELF_TYPE {{
      next <- next_;
      self;
   }};
};

class LinkedListIterator inherits Iterator {
   current : LinkedListNode;

   init(head : LinkedListNode) : SELF_TYPE {{
      current <- head;
      self;
   }};

   next() : Bool {
      if isvoid current then
         false
      else
         {
            current <- current.next();
            isvoid current;
         }
      fi
   };

   get() : Object {
      current.value()
   };
};

class LinkedList inherits Collection {
   size : Int;
   size() : Int { size };

   head : LinkedListNode;
   tail : LinkedListNode;

   iterator() : Iterator {
      new LinkedListIterator.init(head)
   };

   get(n : Int) : Object {
      let node : LinkedListNode <- head in
         {
            while 0 < n loop
               {
                  node <- node.next();
                  n <- n - 1;
               }
            pool;

            node.value();
         }
   };

   add(o : Object) : SELF_TYPE {{
      if isvoid tail then
         head <- tail <- new LinkedListNode.init(o)
      else
         let oldTail : LinkedListNode <- tail in
            oldTail.setNext(tail <- new LinkedListNode.init(o))
      fi;

      size <- size + 1;
      self;
   }};

   addFirst(o : Object) : SELF_TYPE {{
      let oldHead : LinkedListNode <- head in
         (head <- new LinkedListNode.init(o)).setNext(oldHead);

      if isvoid tail then
         tail <- head
      else false fi;

      size <- size + 1;
      self;
   }};

   removeFirst() : Object {
      let oldHead : LinkedListNode <- head in
         {
            head <- head.next();

            if isvoid head then
               tail <- head
            else false fi;

            size <- size - 1;
            oldHead.value();
         }
   };
};
