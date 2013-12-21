// 232792560 - Completed 8.21.2013.
#include <stdlib.h>
#include <stdio.h>

#include "../C_Utils/number.h"

int main(){
    long lcm(long,long);
    long f = 1;
    long t;
    for (long n = 1;n <= 20;n++){
	t = lcm(f,n);
	f = t;
    }
    printf("%ld\n",t);
}
