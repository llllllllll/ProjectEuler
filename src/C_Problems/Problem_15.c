// 137846528820 - Completed 8.21.2013
#include <stdlib.h>
#include <gmp.h>

#include "../C_Utils/number.h"

int main(){
    mpz_t res;
    mpz_init(res);
    nCr(res,40,20);
    gmp_printf("%Zd",res);
    mpz_clear(res);
}
