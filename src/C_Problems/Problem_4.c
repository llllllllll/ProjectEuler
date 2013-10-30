// NOT YET COMPLETED.
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../C_Utils/string.h"


int main(){
    char *t = reverse("testing");
    printf("%s\n",t);
    free(t);
}
