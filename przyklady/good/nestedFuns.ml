int plustimestwo (int a, int b){ 
  return double(a) + b;
}

int double(int a){
  return a * 2;
}

int main() {
  print(plustimestwo(2, 2));
  return 0;
}
