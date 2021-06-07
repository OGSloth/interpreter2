int main() {
    int x = 0;
    print("Break!");
    while (x < 10) {
        x = x + 1;
        if(x == 5)
            break;
        print(x);
    }
    x = 0;
    print("Continue");
    while (x < 10) {
        x = x + 1;
        if(x == 5)
            continue;
        print(x);
    }
    return 0;
}
