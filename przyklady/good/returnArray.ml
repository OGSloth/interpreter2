int[] changeTab (int a, int b){
   int tab[a];
   tab[0] = b;
   return tab;
}   

int main(){
  int[] a = changeTab(2,7);
  print(a[0]);

  return 0;
}
