// 171 - Completed 9.22.2013
#include <stdlib.h>
#include <stdio.h>

// Data structure to hold a Day.
struct day {
    int day;
    int weekday;
    int month;
    int year;
};

typedef struct day day_t;

void next_day(), print();
int is_counted();

// Moves the Day to the next day given checks about the month and leapyear.
void next_day(day_t *d){
    int month30(int), month31(int), is_leap_year(int);
    if (d->month == 12 && d->day == 31){
	d->day = 1;
	d->month = 1;
	++d->year;
	return;
    }
    if (d->month == 2 && d->day == 29){
	d->day = 1;
	++d->month;
	return;
    }
    if (d->month == 2 && d->day == 28){
	if (is_leap_year(d->year)){
	    ++d->day;
	    return;
	} else {
	    d->day = 1;
	    ++d->month;
	    return;
	}
    }
    if (month30(d->month) && d->day == 30){
	d->day = 1;
	++d->month;
	return;
    }
    if (month31(d->month) && d->day == 31){
	d->day = 1;
	++d->month;
	return;
    }
    ++d->day;
    d->weekday = ++d->weekday % 7;
}

// Makes the Day start at the date given in the problem.
void set_start(day_t *d){
    d->day = 1;
    d->weekday = 2;
    d->month = 1;
    d->year = 1901;
}

// Checks if the date should be counted by the rules of the problem.
int is_counted(day_t *d){
    return d->weekday == 0 && d->day == 1;
}


int month30(int month){
    return month == 4 || month == 6 || month == 9 || month == 11;
}

int month31(int month){
    return month != 2 && !month30(month);
}

int is_leap_year(int year){
    return (year % 400 == 0 || year % 100 != 0) && year % 4 == 0;
}

int main(){
    day_t d;
    set_start(&d);
    int count = 0;
    while (d.year < 2001){
	count += (is_counted(&d)) ? 1 : 0;
	next_day(&d);
    }
    printf("%d\n",count);
}
