int goodFunction(){
	print("Good function works fine!");
	return 7;
}

int badFunction() {
	print("Bad function will show error on assignment");
	return 5;
}

int main() {
	int x = goodFunction();
	string s = badFunction();
	print("This should not be printed");
	return 0;
}
