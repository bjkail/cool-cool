class Main {
   main() : Object {
      call(0)
   };

   call(depth : Int) : Object {
      if depth + 1 < 1000 then
         call(depth + 1)
      else
         0
      fi
   };
};
