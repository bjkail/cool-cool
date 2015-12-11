class Main inherits Test {
   test() : Object {{
      testIterator();
      testBasic();
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

   testBasic() : Object {
      if begin("basic") then
         {
            assertVoid("0 void", new StringListMap.getWithString(""));

            let map : StringMap <- new StringListMap in
               {
                  assertVoid("1 put", map.putWithString("a", "A"));
                  assertVoid("1 get void", map.getWithString(""));
                  assertStringEquals("1 get", "A", case map.getWithString("a") of x : String => x; esac);
               };

            let map : StringMap <- new StringListMap in
               {
                  assertVoid("2 put", map.putWithString("a", "A"));
                  assertVoid("2 put", map.putWithString("b", "B"));
                  assertVoid("2 get void", map.getWithString(""));
                  assertStringEquals("2 get", "A", case map.getWithString("a") of x : String => x; esac);
                  assertStringEquals("2 get", "B", case map.getWithString("b") of x : String => x; esac);
               };

            let map : StringMap <- new StringListMap in
               {
                  map.putWithString("a", "A");
                  assertStringEquals("reput", "A", case map.putWithString("a", "B") of x : String => x; esac);
                  assertStringEquals("reput get", "B", case map.getWithString("a") of x : String => x; esac);
               };
         }
      else false fi
   };
};
