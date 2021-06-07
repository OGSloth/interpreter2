/*
 No nested functions
*/

int main () {
  int x ;
  int y ;
  x = 4 ;
  y = 2 ;
  y = y * 2 ;
  int res
  res = function() ;
  return 0;
}

int function (int y) {
   int x = 7;
   void function2 () {
      print("I am nested function");
   }
   if ( y == x ) print("That should not happen") ;
   function2();
   return 0;
}
