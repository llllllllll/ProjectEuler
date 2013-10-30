// Joe Jevnik
// 30.10.2013
// Header of the number utilities.

#ifndef NUMBER_H
#define NUMBER_H
#include <gmp.h>

// least common multiple.
long lcm(long,long);
// greatest common divisor.
long gcd(long,long);
// combinatorics, saves the value in the first mpz_t.
void ncr(mpz_t,int,int);

#endif /* NUMBER_H */
