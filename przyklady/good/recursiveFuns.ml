int a(int c){
    print("Co jest3");
    return c / 2;
}

int b(int c) {
    print("Co jest4");
    return a(c - 1);
}


int s(int l){
    int sum = 0;
    print("Co jest");
    for(i = 0 to l){
        print("Co jest2");
        sum = sum + a(i) + b(i);
    }
    return sum;
}


int main() {
    print(s(5));
    return 0;
}


