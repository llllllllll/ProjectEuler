// NOT YET COMPLETED.
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

int is_prime_factor(long,long);

int main(){
    long ans = 0;
    long max = (long) sqrt(600851475143);
    for (int n = 2; n <= max; n++){
	ans = (is_prime_factor(n,600851475143)) ? n : ans;
    }
    printf("%ld\n",ans);
    return 0;
}

int is_prime_factor(long n,long num){
    if (n % 2 == 0) return 0;
    if (num % n != 0) return 0;
    long max = (long) sqrt(600851475143);
    for (int m = 2;m <= max;m++){
	if (n % m == 0) return 0;
    }
    return 1;
}
