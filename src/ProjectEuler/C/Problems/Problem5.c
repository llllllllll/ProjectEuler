// 232792560 - Completed 8.21.2013.
#include <stdlib.h>
#include <stdio.h>

#include "../C_Utils/number.h"

int main(){
    long f = 1;
    long t,n;
    for (n = 1;n <= 20;n++){
	t = lcm(f,n);
	f = t;
    }
    printf("%ld\n",t);
    return 0;
}
