class Main inherits Test {
   test() : Object {{
      testIterator();
      testPut();
      testPutNew();
   }};

   testIterator() : Object {
      if begin("iterator") then
         let map : StringMap <- new StringListMap in
            {
               let iter : StringMapIterator <- map.iterator() in
                  {
                     assertFalse("empty", iter.next());
                     assertFalse("empty 2", iter.next());
                  };

               map.putWithString("a", "A");
               let iter : StringMapIterator <- map.iterator() in
                  {
                     assertTrue("1", iter.next());
                     assertStringEquals("1", "a", iter.key());
                     assertStringEquals("1", "A", case iter.value() of x : String => x; esac);
                     assertFalse("1", iter.next());
                  };

               map.putWithString("b", "B");
               let iter : StringMapIterator <- map.iterator() in
                  {
                     assertTrue("2 1", iter.next());
                     assertStringEquals("2", "b", iter.key());
                     assertStringEquals("2", "B", case iter.value() of x : String => x; esac);
                     assertTrue("2 2", iter.next());
                     assertStringEquals("2", "a", iter.key());
                     assertStringEquals("2", "A", case iter.value() of x : String => x; esac);
                     assertFalse("2", iter.next());
                  };
            }
      else false fi
   };

   testPut() : Object {
      if begin("put") then
         {
            assertIntEquals("0 size", 0, new StringListMap.size());
            assertVoid("0 void", new StringListMap.getWithString(""));

            let map : StringMap <- new StringListMap in
               {
                  assertVoid("1 put", map.putWithString("a", "A"));
                  assertIntEquals("1 size", 1, map.size());
                  assertVoid("1 get void", map.getWithString(""));
                  assertStringEquals("1 get", "A", case map.getWithString("a") of x : String => x; esac);
               };

            let map : StringMap <- new StringListMap in
               {
                  assertVoid("2 put", map.putWithString("a", "A"));
                  assertVoid("2 put", map.putWithString("b", "B"));
                  assertIntEquals("2 size", 2, map.size());
                  assertVoid("2 get void", map.getWithString(""));
                  assertStringEquals("2 get", "A", case map.getWithString("a") of x : String => x; esac);
                  assertStringEquals("2 get", "B", case map.getWithString("b") of x : String => x; esac);
               };

            let map : StringMap <- new StringListMap in
               {
                  map.putWithString("a", "A");
                  assertStringEquals("reput", "A", case map.putWithString("a", "B") of x : String => x; esac);
                  assertIntEquals("reput size", 1, map.size());
                  assertStringEquals("reput get", "B", case map.getWithString("a") of x : String => x; esac);
               };
         }
      else false fi
   };

   testPutNew() : Object {
      if begin("putNew") then
         {
            let map : StringMap <- new StringListMap in
               {
                  assertVoid("1 put", map.putNewWithString("a", "A"));
                  assertIntEquals("1 size", 1, map.size());
                  assertVoid("1 get void", map.getWithString(""));
                  assertStringEquals("1 get", "A", case map.getWithString("a") of x : String => x; esac);
               };

            let map : StringMap <- new StringListMap in
               {
                  assertVoid("2 put", map.putNewWithString("a", "A"));
                  assertVoid("2 put", map.putNewWithString("b", "B"));
                  assertIntEquals("2 size", 2, map.size());
                  assertVoid("2 get void", map.getWithString(""));
                  assertStringEquals("2 get", "A", case map.getWithString("a") of x : String => x; esac);
                  assertStringEquals("2 get", "B", case map.getWithString("b") of x : String => x; esac);
               };

            let map : StringMap <- new StringListMap in
               {
                  map.putNewWithString("a", "A");
                  assertStringEquals("reput", "A", case map.putNewWithString("a", "B") of x : String => x; esac);
                  assertIntEquals("reput size", 1, map.size());
                  assertStringEquals("reput get", "A", case map.getWithString("a") of x : String => x; esac);
               };
         }
      else false fi
   };
};
