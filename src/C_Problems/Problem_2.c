// 4613732 - Completed 20.8.2013
#include <stdlib.h>
#include <stdio.h>

int main(){
    int fib(int);
    int sum = 0;
    for (int n = 3;1;n++){
	int m = fib(n);
	if (m >= 4000000)
	    break;
	else
	    sum = (m % 2 == 0) ? sum + m : sum;
    }
    printf("%d",sum);
}

int fib(int n){
    if (n==1 || n==2)
	return 1;
    else
	return fib(n-1) + fib(n-2);
}
