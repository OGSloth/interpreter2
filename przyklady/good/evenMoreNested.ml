string appendNL(string a){
  return a + "\n";
}

void printArr(string[] arr, int size) {
  for(i = 0 to size) {
     printNL(arr[i]);
  }
  return;
}

int main() {
  string myWord[6];
  myWord[0] = "m";
  myWord[1] = "y";
  myWord[2] = "W";
  myWord[3] = "o";
  myWord[4] = "r";
  myWord[5] = "d";
  printArr(myWord, 6);
  return 0;
}


void printNL(string a){
  print(appendNL(a));
  return;
}


