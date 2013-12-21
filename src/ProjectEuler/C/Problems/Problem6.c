// 25164150 - Completed 8.23.2013
#include <stdlib.h>
#include <gmp.h>
#include <stdio.h>

int main(){
    mpz_t sum_sq,sq_sum,ans;
    mpz_inits(sum_sq,sq_sum,ans,NULL);
    for (int n = 0;n <= 100;n++){
	mpz_add_ui(sum_sq,sum_sq,n);
    }
    mpz_mul(sum_sq,sum_sq,sum_sq);
    for (int n = 0;n <= 100;n++){
	mpz_add_ui(sq_sum,sq_sum,n*n);
    }
    mpz_sub(ans,sum_sq,sq_sum);
    gmp_printf("%Zd",ans);
    mpz_clears(sq_sum,sum_sq,ans,NULL);
}
