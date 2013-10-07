// 427337 - Completed 4.10.2013
// Thank you to Proffesor Mixer for helping me reason about this problem.
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string.h>

using namespace std;

int g[80][80];

int main(){
    void parse_ln(string ln, int r), fill_cost_mat();
    ifstream txt("txt/matrix.txt");
    string ln;
    int r = 0;
    while (getline(txt,ln)){
	parse_ln(ln,r++);
    }
    fill_cost_mat();
    cout << g[79][79];
    txt.close();
}

void parse_ln(string ln,int r){
    char *cstr = new char[ln.length() + 1];
    strcpy(cstr, ln.c_str());
    char *sp = strtok(cstr,",");
    g[r][0] = atoi(sp);
    for (int n = 1;n < 80;n++){
        char *sp = strtok(NULL,",");
	g[r][n] = atoi(sp);
    }
    delete cstr;
}

void fill_cost_mat(){
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
