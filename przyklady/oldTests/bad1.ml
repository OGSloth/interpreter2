/*
	Trying read only
*/

int main () {
  int x[10] ;
  x[4] = 3;
  print(x[4]) ;
  x[6] = 7;
  for (i = x[4] to x[6] ) {
     i = i + 4; 
  }
  return 0 ;
}
