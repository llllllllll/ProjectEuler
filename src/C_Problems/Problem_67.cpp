// 7273 - Completed 8.9.2013
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <gmp.h>
#include <string.h>
using namespace std;

int g[100][100];

int main(){
    void step_up(int[][100],int), parse_ln(string,int);
    int r = 0;
    string ln;
    ifstream txt("txt/triangle.txt");
    while (getline(txt,ln)){
        parse_ln(ln,r++);
    }
    while (r > 0){
	step_up(g,r);
	r--;
    }
    txt.close();
    cout << g[0][0];
}

void step_up(int g[][100],int r){
    int temp[r];
    for (int n = 0;n < r + 1;n++){
	temp[n] = (g[r][n] > g[r][n+1]) ? g[r][n] : g[r][n+1];
    }
    for (int n = 0;n < r + 1;n++){
	g[r-1][n] = g[r-1][n] + temp[n];
    }
}
    
void parse_ln(string ln,int r){
    char *cstr = new char[ln.length() + 1];
    strcpy(cstr, ln.c_str());
    char *sp = strtok(cstr," ");
    g[r][0] = ((sp[0] - '0') * 10) + (sp[1] - '0');
    for (int n = 1;n <= r;n++){
        char *sp = strtok(NULL," ");
	g[r][n] = ((sp[0] - '0') * 10) + (sp[1] - '0');
    }
    delete cstr;
}
