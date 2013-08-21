// 137846528820 - Completed 8.21.2013
#include <stdlib.h>
#include <iostream>
#include <gmp.h>

int main(){
    void nCr(int,int);
    nCr(40,20);
}

void nCr(int n,int r){
    mpz_t res,num,den,den_1,den_2;
    mpz_init(res);
    mpz_init(num);
    mpz_init(den);
    mpz_init(den_1);
    mpz_init(den_2);
    mpz_fac_ui(num,n);
    mpz_fac_ui(den_1,r);
    mpz_fac_ui(den_2,n-r);
    mpz_mul(den,den_1,den_2);
    mpz_cdiv_q(res,num,den);
    gmp_printf ("%Zd",res);
}
     
