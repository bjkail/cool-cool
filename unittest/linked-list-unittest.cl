class Main inherits Test {
   test() : Object {{
      testSize();
      testIterator();
      testGet();
      testAddFirst();
      testRemoveFirst();
   }};

   testSize() : Object {
      if begin("size") then
         {
            assertIntEquals("0", 0, new LinkedList.size());
            assertIntEquals("1", 1, new LinkedList.add(0).size());
            assertIntEquals("2", 2, new LinkedList.add(0).add(0).size());

            let list : LinkedList <- new LinkedList.add(0) in
               {
                  list.removeFirst();
                  assertIntEquals("remove", 0, list.size());
               };
         }
      else false fi
   };

   testIterator() : Object {
      if begin("iterator") then
         {
            let iter : Iterator <- new LinkedList.iterator() in
               {
                  assertFalse("0 next", iter.next());
                  assertFalse("0 next 2", iter.next());
               };

            let iter : Iterator <- new LinkedList.add(0).iterator() in
               {
                  assertTrue("1 next 0", iter.next());
                  assertIntEquals("1 get 0", 0, case iter.get() of x : Int => x; esac);
                  assertIntEquals("1 get 0 2", 0, case iter.get() of x : Int => x; esac);
                  assertFalse("1 next 1", iter.next());
               };

            let iter : Iterator <- new LinkedList.add(0).add(1).iterator() in
               {
                  assertTrue("2 next 0", iter.next());
                  assertIntEquals("2 get 0", 0, case iter.get() of x : Int => x; esac);
                  assertTrue("2 next 1", iter.next());
                  assertIntEquals("2 get 1", 1, case iter.get() of x : Int => x; esac);
                  assertFalse("2 next 2", iter.next());
               };
         }
      else false fi
   };

   testGet() : Object {
      if begin("get") then
         {
            let list : LinkedList <- new LinkedList.add(0) in
               assertIntEquals("1 get 0", 0, case list.get(0) of x : Int => x; esac);

            let list : LinkedList <- new LinkedList.add(0).add(1) in
               {
                  assertIntEquals("2 get 0", 0, case list.get(0) of x : Int => x; esac);
                  assertIntEquals("2 get 1", 1, case list.get(1) of x : Int => x; esac);
               };
         }
      else false fi
   };

   testAddFirst() : Object {
      if begin("addFirst") then
         {
            let list : LinkedList <- new LinkedList.addFirst(0) in
               assertIntEquals("1 get 0", 0, case list.get(0) of x : Int => x; esac);

            let list : LinkedList <- new LinkedList.addFirst(0).addFirst(1) in
               {
                  assertIntEquals("2 get 0", 1, case list.get(0) of x : Int => x; esac);
                  assertIntEquals("2 get 1", 0, case list.get(1) of x : Int => x; esac);
               };
         }
      else false fi
   };

   testRemoveFirst() : Object {
      if begin("removeFirst") then
         let list : LinkedList <- new LinkedList.add(0).add(1) in
            {
               assertIntEquals("remove 0", 0, case list.removeFirst() of x : Int => x; esac);
               assertIntEquals("get", 1, case list.get(0) of x : Int => x; esac);
               assertIntEquals("remove 1", 1, case list.removeFirst() of x : Int => x; esac);
               assertFalse("iterator", list.iterator().next());
            }
      else false fi
   };
};
