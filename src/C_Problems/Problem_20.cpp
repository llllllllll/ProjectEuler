// 648 - Completed 8.23.2013
#include <stdlib.h>
#include <iostream>
#include <string.h>
#include <gmp.h>

int main(){
    mpz_t fac;
    char *fac_str;
    int sum = 0;
    mpz_init(fac);
    mpz_fac_ui(fac,100);
    fac_str = mpz_get_str(*"",10,fac);
    for (int n = 0;n < strlen(fac_str);n++){
	sum += fac_str[n] - '0';
    }
    mpz_clear(fac);
    std::cout << sum;
}
