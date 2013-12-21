// 7273 - Completed 8.9.2013
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


int main(){
    void step_up(int[][100],int), parse_ln(int[][100],char*,int);
    int r = 0, g[100][100];
    char *ln, lb[1024];
    FILE *txt = fopen("../txt/triangle.txt","r");
    for (int n = 0;n < 100;n++){
	for (int m = 0;m < 100;m++){
	    g[n][m] = 0;
	}
    }
    while(fgets(lb,1024,txt)){
	ln = strdup(lb);
	parse_ln(g,ln,r++);
	ln = strdup(lb);
    }
    while (r > 0){
	step_up(g,r);
	r--;
    }
    printf("%d\n",g[0][0]);
    fclose(txt);
}

void step_up(int g[][100],int r){
    int tmp[r];
    for (int n = 0;n < r + 1;n++){
	tmp[n] = (g[r][n] > g[r][n+1]) ? g[r][n] : g[r][n+1];
    }
    for (int n = 0;n < r + 1;n++){
	g[r-1][n] = g[r-1][n] + tmp[n];
    }
}

void parse_ln(int g[][100],char *ln,int r){
    int c = 0, e = 0;
    char *tmp = malloc(3 * sizeof(char));
    for (int n = 0;n < strlen(ln);n++){
	if (c == 2){
	    c = 0;
	    //printf("(%d,%d):%d\n",r,e,atoi(tmp));
	    //printf("  %s\n",tmp);
	    g[r][e++] = atoi(tmp);
	    free(tmp);
	    tmp = malloc(3 * sizeof(char));
	}else{
	    tmp[c++] = ln[n];
	}
    }
    free(tmp);
}
