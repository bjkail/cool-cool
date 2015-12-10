class Main inherits Test {
   test() : Object {{
      testBasic();
   }};

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
