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
   head : LinkedListNode;

   init(head_ : LinkedListNode) : SELF_TYPE {{
      head <- head_;
      self;
   }};

   next() : Bool {{
      if isvoid current then
         {
            current <- head;
            head <- let void : LinkedListNode in void;
         }
      else
         current <- current.next()
      fi;

      not isvoid current;
   }};

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

   sortMerge(comparator : Comparator, node1 : LinkedListNode, node2 : LinkedListNode) : LinkedListNode {
      let head : LinkedListNode,
            tail : LinkedListNode in
         {
            let cmp : Int <- comparator.compare(node1.value(), node2.value()) in
               if cmp <= 0 then
                  {
                     head <- node1;
                     node1 <- node1.next();
                  }
               else
                  {
                     head <- node2;
                     node2 <- node2.next();
                  }
               fi;
            tail <- head;

            let continue : Bool <- true in
               while continue loop
                  if isvoid node1 then
                     {
                        tail.setNext(node2);
                        continue <- false;
                     }
                  else
                     if isvoid node2 then
                        {
                           tail.setNext(node1);
                           continue <- false;
                        }
                     else
                        let cmp : Int <- comparator.compare(node1.value(), node2.value()) in
                           if cmp <= 0 then
                              {
                                 tail.setNext(node1);
                                 tail <- node1;
                                 node1 <- node1.next();
                              }
                           else
                              {
                                 tail.setNext(node2);
                                 tail <- node2;
                                 node2 <- node2.next();
                              }
                           fi
                     fi
                  fi
               pool;

            head;
         }
   };

   sortImpl(comparator : Comparator, head : LinkedListNode) : LinkedListNode {
      if isvoid head then
         head
      else
         if isvoid head.next() then
            head
         else
            let slow : LinkedListNode <- head,
                  fast : LinkedListNode <- head in
               {
                  while if not isvoid fast.next() then
                        not isvoid fast.next().next()
                     else false
                  fi loop
                     {
                        slow <- slow.next();
                        fast <- fast.next().next();
                     }
                  pool;

                  let head2 : LinkedListNode <- slow.next() in
                     {
                        slow.setNext(let void : LinkedListNode in void);
                        sortMerge(comparator,
                              sortImpl(comparator, head),
                              sortImpl(comparator, head2));
                     };
               }
         fi
      fi
   };

   sort(comparator : Comparator) : SELF_TYPE {{
      head <- sortImpl(comparator, head);
      self;
   }};
};
