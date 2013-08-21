// 232792560 - Completed 8.21.2013.
#include <stdlib.h>
#include <iostream>
#include <math.h>

int main(){
    long lcm(long,long);
    long f = 1;
    long t;
    for (long n = 1;n <= 20;n++){
	t =lcm(f,n);
	f = t;
    }
    std::cout << t;
}

long lcm(long a,long b){
    long gcd(long,long);
    return a*b/gcd(a,b);
}   

long gcd(long a,long b) {
    if (b == 0) {
	return a;
    }
    else {
	return gcd(b,a%b);
    }
}
