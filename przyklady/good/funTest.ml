void printFun (){
    print("Function is being printed");
    return;
}

int argFun(int x, int y) {
  return x + y;
}

int main(){
  printFun();
  print(argFun(123, 321));
  return 0;
}
