// 233168 - Completed 20.8.2013
#include <stdio.h>

int main(){
    int sum = 0,n;
    for (0; n < 1000; n++){
	sum = (n % 3 == 0 || n % 5 == 0) ? sum + n : sum;
    }
    printf("%d\n",sum);
    return 0;
}
