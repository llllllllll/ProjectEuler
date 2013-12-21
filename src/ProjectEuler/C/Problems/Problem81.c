// 427337 - Completed 4.10.2013
// Thank you to Proffesor Mixer for helping me reason about this problem.
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


int main(){
    void parse_fl(long[][80],FILE*), fill_cost_mat(long[][80]);
    FILE *txt = fopen("../txt/matrix.txt","r");
    long g[80][80];
    parse_fl(g,txt);
    fill_cost_mat(g);
    //printf("%d\n",g[79][79]);
    fclose(txt);
}



void fill_cost_mat(long g[][80]){
    for (int n = 1;n < 80;n++){
	g[n][0] += g[n-1][0];
    }
    for (int m = 1;m < 80;m++){
	g[0][m] += g[0][m-1];
    }
    for (int n = 1;n < 80;n++){
	for (int m = 1;m < 80;m++){
	    g[n][m] += (g[n-1][m] < g[n][m-1]) ? g[n-1][m] : g[n][m-1];
	}
    }
}

void parse_fl(long g[][80], FILE *txt){
    int r = 0,c = 0;
    while (!feof(txt)){
	if (c == 80){
	    c = 0;
	    ++r;
	}
	fscanf(txt,"%ld",&g[r][c++]);
	while(fgetc(txt) != ',' && !feof(txt));
	printf("%ld\n",g[r][c]);
    }
}
