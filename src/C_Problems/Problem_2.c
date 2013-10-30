// 4613732 - Completed 20.8.2013
#include <stdlib.h>
#include <stdio.h>

#include "../C_Utils/sequence.h"

int main(){
    int sum = 0;
    for (int n = 3;1;n++){
	int m = fib(n);
	if (m >= 4000000)
	    break;
	else
	    sum = (m % 2 == 0) ? sum + m : sum;
    }
    printf("%d\n",sum);
}
