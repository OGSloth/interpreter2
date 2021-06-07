int fib(int k) {
  if(k == 0)
    return 1;
  else {
    if(k == 1)
        return 1;
    else
        return (fib(k - 1) + fib(k - 2));
  }
}

int main() {
    print(fib (10));
    return 0;
}
