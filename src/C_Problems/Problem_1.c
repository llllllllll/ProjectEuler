// 233168 - Completed 20.8.2013
#include <stdlib.h>
#include <iostream>

int main(){
    int sum = 0;
    for (int n = 0; n < 1000; n++){
	sum = (n % 3 == 0 || n % 5 == 0) ? sum + n : sum;
    }
    std::cout << sum;
}
	 