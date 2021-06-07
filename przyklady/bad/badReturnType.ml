int goodFunction(){
	print("Good function works fine!");
	return 0;
}

int badFunction() {
	print("Bad function will show error");
	return "Error";
}

int main() {
	goodFunction();
	badFunction();
	print("This should not be printed");
	return 0;
}
