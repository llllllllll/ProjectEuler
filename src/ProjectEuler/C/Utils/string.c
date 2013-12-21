// Joe Jevnik
// 30.10.2013
// Implentation of the string utilities.

#include <stdlib.h>
#include <string.h>

#include "string.h"

// returns the reverse of str.
// mallocs the new string.
char *reverse(char *str){
    int len = strlen(str);
    char *res = malloc(len * sizeof(char));
    for (int n = 0;n < len;n++){
	res[n] = str[len - n - 1];
    }
    return res;
}
