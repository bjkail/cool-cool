class Main inherits Test {
   test() : Object {{
      testBasic();
   }};

   testBasic() : Object {
      if begin("basic") then
         {
            let map : IntMap <- new IntTreeMap in
               {
                  assertIntEquals("0 size", 0, map.size());
                  assertVoid("0 void", map.getWithInt(0));
                  assertFalse("0 iterator next", map.iterator().next());
               };

            let map : IntMap <- new IntTreeMap in
               {
                  assertVoid("1 put", map.putWithInt(1, "A"));
                  assertIntEquals("1 size", 1, map.size());
                  assertVoid("1 get void", map.getWithInt(0));
                  assertStringEquals("1 get", "A", case map.getWithInt(1) of x : String => x; esac);

                  let iter : IntMapIterator <- map.iterator() in
                     {
                        assertTrue("1 iterator next", iter.next());
                        assertIntEquals("1 iterator key", 1, iter.key());
                        assertStringEquals("1 iterator value", "A", case iter.value() of x : String => x; esac);
                        assertFalse("1 iterator end", iter.next());
                     };
               };

            let map : IntMap <- new IntTreeMap in
               {
                  assertVoid("2 put", map.putWithInt(1, "A"));
                  assertVoid("2 put", map.putWithInt(2, "B"));
                  assertIntEquals("2 size", 2, map.size());
                  assertVoid("2 get void", map.getWithInt(0));
                  assertStringEquals("2 get", "A", case map.getWithInt(1) of x : String => x; esac);
                  assertStringEquals("2 get", "B", case map.getWithInt(2) of x : String => x; esac);

                  let iter : IntMapIterator <- map.iterator() in
                     {
                        assertTrue("2 iterator 1 next", iter.next());
                        assertIntEquals("2 iterator 1 key", 1, iter.key());
                        assertStringEquals("2 iterator 1 value", "A", case iter.value() of x : String => x; esac);
                        assertTrue("2 iterator 2 next", iter.next());
                        assertIntEquals("2 iterator 2 key", 2, iter.key());
                        assertStringEquals("2 iterator 2 value", "B", case iter.value() of x : String => x; esac);
                        assertFalse("2 iterator end", iter.next());
                     };
               };

            let map : IntMap <- new IntTreeMap in
               {
                  map.putWithInt(1, "A");
                  assertStringEquals("reput", "A", case map.putWithInt(1, "B") of x : String => x; esac);
                  assertIntEquals("reput size", 1, map.size());
                  assertStringEquals("reput get", "B", case map.getWithInt(1) of x : String => x; esac);
               };

            let map : IntMap <- new IntTreeMap in
               {
                  map.putWithInt(0, 10);
                  map.putWithInt(1, 11);
                  map.putWithInt(2, 12);
                  map.putWithInt(3, 13);
                  map.putWithInt(4, 14);
                  map.putWithInt(5, 15);
                  map.putWithInt(6, 16);
                  map.putWithInt(7, 17);

                  let i : Int,
                        iter : IntMapIterator <- map.iterator() in
                     {
                        while i < 8 loop
                           {
                              assertIntEquals("get ordered", 10 + i, case map.getWithInt(i) of x : Int => x; esac);
                              assertTrue("ordered iterator next", iter.next());
                              assertIntEquals("ordered iterator key", i, iter.key());
                              assertIntEquals("ordered iterator value", 10 + i, case iter.value() of x : Int => x; esac);
                              i <- i + 1;
                           }
                        pool;

                        assertFalse("ordered iterator", iter.next());
                     };
               };

            let map : IntMap <- new IntTreeMap in
               {
                  map.putWithInt(7, 17);
                  map.putWithInt(6, 16);
                  map.putWithInt(5, 15);
                  map.putWithInt(4, 14);
                  map.putWithInt(3, 13);
                  map.putWithInt(2, 12);
                  map.putWithInt(1, 11);
                  map.putWithInt(0, 10);

                  let i : Int,
                        iter : IntMapIterator <- map.iterator() in
                     {
                        while i < 8 loop
                           {
                              assertIntEquals("get ordered", 10 + i, case map.getWithInt(i) of x : Int => x; esac);
                              assertTrue("ordered iterator next", iter.next());
                              assertIntEquals("ordered iterator key", i, iter.key());
                              assertIntEquals("ordered iterator value", 10 + i, case iter.value() of x : Int => x; esac);
                              i <- i + 1;
                           }
                        pool;

                        assertFalse("reversed iterator", iter.next());
                     };
               };

            let map : IntMap <- new IntTreeMap in
               {
                  map.putWithInt(4, 14);
                  map.putWithInt(3, 13);
                  map.putWithInt(5, 15);
                  map.putWithInt(2, 12);
                  map.putWithInt(6, 16);
                  map.putWithInt(1, 11);
                  map.putWithInt(7, 17);
                  map.putWithInt(0, 10);

                  let i : Int,
                        iter : IntMapIterator <- map.iterator() in
                     {
                        while i < 8 loop
                           {
                              assertIntEquals("get mixed", 10 + i, case map.getWithInt(i) of x : Int => x; esac);
                              assertTrue("mixed iterator next", iter.next());
                              assertIntEquals("mixed iterator key", i, iter.key());
                              assertIntEquals("mixed iterator value", 10 + i, case iter.value() of x : Int => x; esac);
                              i <- i + 1;
                           }
                        pool;

                        assertFalse("mixed iterator", iter.next());
                     };
               };
         }
      else false fi
   };
};
