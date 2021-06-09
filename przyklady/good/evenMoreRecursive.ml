int a(int c){
    print("a - call");
    if(c <= 0)
	return 0;

    return s(c - 2);
}

int b(int c) {
    print("b - call");
    if(c <= 0)
	return 0;
    return s(c - 1);
}


int s(int l){
    print("s - call");
    int sum = 0;
    if(l <= 0)
	return 0;
    for(i = 0 to l){
        sum = sum + a(i) + b(i);
    }
    return sum;
}


int main() {
    print(s(5));
    return 0;
}


