class Main {
   a : Main;

   main() : Object { a() };

   a() : Object { b() };

   b() : Object { c() };

   c() : Object { a.void() };

   void() : Object { false };
};
