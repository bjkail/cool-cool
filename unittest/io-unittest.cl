class Main inherits Test {
   test() : Object {{
      testIOInputStream();
   }};

   testIOInputStream() : Object {
      if begin("IOInputStream") then
         {
            let lines : LinkedList <- new LinkedList,
                  is : InputStream <- new TestIOInputStream.init(new TestIO.init(lines)) in
               {
                  assertStringEquals("empty [0]", "", is.read());
                  assertStringEquals("empty [1]", "", is.read());
               };

            let lines : LinkedList <- new LinkedList.add("a"),
                  is : InputStream <- new TestIOInputStream.init(new TestIO.init(lines)) in
               {
                  assertStringEquals("char [0]", "a", is.read());
                  assertStringEquals("char [1]", "\n", is.read());
                  assertStringEquals("char [2]", "", is.read());
               };

            let lines : LinkedList <- new LinkedList.add("").add("a"),
                  is : InputStream <- new TestIOInputStream.init(new TestIO.init(lines)) in
               {
                  assertStringEquals("blank [0]", "\n", is.read());
                  assertStringEquals("blank [1]", "a", is.read());
                  assertStringEquals("blank [2]", "\n", is.read());
                  assertStringEquals("blank [3]", "", is.read());
               };

            let lines : LinkedList <- new LinkedList.add("abc"),
                  is : InputStream <- new TestIOInputStream.init(new TestIO.init(lines)) in
               {
                  assertStringEquals("chars [0]", "a", is.read());
                  assertStringEquals("chars [1]", "b", is.read());
                  assertStringEquals("chars [2]", "c", is.read());
                  assertStringEquals("chars [3]", "\n", is.read());
                  assertStringEquals("chars [4]", "", is.read());
               };

            let lines : LinkedList <- new LinkedList.add("ab").add("cd").add("").add("ef").add("").add("").add("gh"),
                  is : InputStream <- new TestIOInputStream.init(new TestIO.init(lines)) in
               {
                  assertStringEquals("lines [0]", "a", is.read());
                  assertStringEquals("lines [1]", "b", is.read());
                  assertStringEquals("lines [2]", "\n", is.read());
                  assertStringEquals("lines [3]", "c", is.read());
                  assertStringEquals("lines [4]", "d", is.read());
                  assertStringEquals("lines [5]", "\n", is.read());
                  assertStringEquals("lines [6]", "\n", is.read());
                  assertStringEquals("lines [7]", "e", is.read());
                  assertStringEquals("lines [8]", "f", is.read());
                  assertStringEquals("lines [9]", "\n", is.read());
                  assertStringEquals("lines [10]", "\n", is.read());
                  assertStringEquals("lines [11]", "\n", is.read());
                  assertStringEquals("lines [12]", "g", is.read());
                  assertStringEquals("lines [13]", "h", is.read());
                  assertStringEquals("lines [14]", "\n", is.read());
               };
         }
      else false fi
   };
};

-- Custom IO instance that uses a LinkedList
class TestIO inherits IO {
   iter : Iterator;

   init(lines : Collection) : SELF_TYPE {{
      iter <- lines.iterator();
      self;
   }};

   in_string() : String {
      if iter.next() then
         case iter.get() of x : String => x; esac
      else
         ""
      fi
   };
};

class TestIOInputStream inherits IOInputStream {
   init(io_ : IO) : SELF_TYPE {{
      -- Use a custom IO instance.
      io <- io_;
      self;
   }};
};
