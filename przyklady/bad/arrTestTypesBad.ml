/*
	Sum up border arrays of ones
	
*/

int main(){
    int n = 7;
    int k = 6;
    int arr[n, k];
    arr[0, 0] = 1;
    for(i = 0 to n)
	arr[i, 0] = 1;
    for(j = 0 to k)
	arr[0, j] = 1;

    for(i = 1 to n) {
	for(j = 1 to k){
		arr[i, j] = arr[i - 1, j] + arr[i, j - 1] + arr[i - 1, j - 1];
	} 
    }
    arr[0, 0] = false;
    print(arr[n - 1, k - 1]);
    return 0;
}
