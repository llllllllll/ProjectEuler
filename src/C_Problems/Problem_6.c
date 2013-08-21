// NOT YET COMPLETED.
#include <stdlib.h>
#include <iostream>

int main(){
    int sum_sq,sq_sum;
    for (sq_sum = 1;sum_sq < 100;sq_sum++){}
    sq_sum *= sq_sum;
    for (int n = 1;n <= 100;n++){
	sum_sq += n*n;
    }
    std::cout << sq_sum - sum_sq;
}
