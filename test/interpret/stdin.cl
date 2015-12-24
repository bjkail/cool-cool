--test=
class Main {
   main() : Object {
      let io : IO <- new IO in
         {
            io.out_string("a").out_string(io.in_string()).out_string("c").out_string("\n");
            io.out_int(io.in_int() + 1).out_string("\n");
         }
   };
};
