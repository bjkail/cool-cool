class Main inherits Test {
   test() : Object {{
      testPut();
   }};

   testPut() : Object {
      if begin("put") then
         {
            assertIntEquals("0 size", 0, new IntTreeMap.size());
            assertVoid("0 void", new IntTreeMap.getWithInt(0));

            let map : IntMap <- new IntTreeMap in
               {
                  assertVoid("1 put", map.putWithInt(1, "A"));
                  assertIntEquals("1 size", 1, map.size());
                  assertVoid("1 get void", map.getWithInt(0));
                  assertStringEquals("1 get", "A", case map.getWithInt(1) of x : String => x; esac);
               };

            let map : IntMap <- new IntTreeMap in
               {
                  assertVoid("2 put", map.putWithInt(1, "A"));
                  assertVoid("2 put", map.putWithInt(2, "B"));
                  assertIntEquals("2 size", 2, map.size());
                  assertVoid("2 get void", map.getWithInt(0));
                  assertStringEquals("2 get", "A", case map.getWithInt(1) of x : String => x; esac);
                  assertStringEquals("2 get", "B", case map.getWithInt(2) of x : String => x; esac);
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

                  let i : Int in
                     while i < 8 loop
                        {
                           assertIntEquals("get ordered", 10 + i, case map.getWithInt(i) of x : Int => x; esac);
                           i <- i + 1;
                        }
                     pool;
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

                  let i : Int in
                     while i < 8 loop
                        {
                           assertIntEquals("get ordered", 10 + i, case map.getWithInt(i) of x : Int => x; esac);
                           i <- i + 1;
                        }
                     pool;
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

                  let i : Int in
                     while i < 8 loop
                        {
                           assertIntEquals("get ordered", 10 + i, case map.getWithInt(i) of x : Int => x; esac);
                           i <- i + 1;
                        }
                     pool;
               };
         }
      else false fi
   };
};
