// Joe Jevnik
// 30.10.2013
// Implementation of the number utilities.

#include <stdlib.h>

#include "number.h"

// least common multiple.
long lcm(long a,long b){
    return a * b / gcd(a,b);
}

// greatest common divisor.
long gcd(long a,long b) {
    if (b == 0) {
	return a;
    }
    else {
	return gcd(b,a % b);
    }
}

// n choose r, combinatorics, stores the value in res.
void nCr(mpz_t res,int n,int r){
    mpz_t num,den,den_1,den_2;
    mpz_inits(num,den,den_1,den_2,NULL);
    mpz_fac_ui(num,n);
    mpz_fac_ui(den_1,r);
    mpz_fac_ui(den_2,n-r);
    mpz_mul(den,den_1,den_2);
    mpz_cdiv_q(res,num,den);
    mpz_clears(num,den,den_1,den_2,NULL);
}
