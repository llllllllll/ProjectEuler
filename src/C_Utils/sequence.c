// Joe Jevnik
// 30.10.2013
// Implementation of the sequence utility.

#include "sequence.h"

// Returns the n'th fib number.
int fib(int n){
    if (n==1 || n==2)
	return 1;
    else
	return fib(n-1) + fib(n-2);
}
