/*
 Static and functions
*/

int main () {
  int x ;
  int y ;
  x = 4 ;
  y = 2 ;
  y = y * 2 ;
  if (y == 4) {
     y = y * 2;
     int das_auto = 1234;
  }
  else {
     y = y * 3;
  }
  print(y);
  return 0;
}

int function (int y) {
   int x = 7;
   if ( y == x ) print("That should not happen") ;
   return 2;
}
