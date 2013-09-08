// NOT YET COMPLETED.
#include <stdlib.h>
#include <iostream>
#include <string.h>





char *reverse(char *in,char *out){
    if (strlen(in) == 0)
	return out;
    return reverse(&in[1],(strcat(out,&in[0],out)));
}

int main(){
    std::cout << reverse("thisisatest","");
}
