// 171 - 9.22.2013
#include <stdlib.h>
#include <iostream>

// Data structure to hold a Day.
class Day {
public:
    Day();
    void next_day(), print();
    bool is_counted();
    int day;
    int weekday;
    int month;
    int year;
};

// Moves the Day to the next day given checks about the month and leapyear.
void Day::next_day(){
    bool month30(int month), month31(int month), is_leap_year(int year);
    if (month == 12 && day == 31){
	day = 1;
	month = 1;
	++year;
	return;
    }
    if (month == 2 && day == 29){
	day = 1;
	++month;
	return;
    }
    if (month == 2 && day == 28){
	if (is_leap_year(year)){
	    ++day;
	    return;
	} else {
	    day = 1;
	    ++month;
	    return;
	}
    }
    if (month30(month) && day == 30){
	day = 1;
	++month;
	return;
    }
    if (month31(month) && day == 31){
	day = 1;
	++month;
	return;
    }
    ++day;
    weekday = ++weekday % 7;
}

// Makes the Day start at the date given in the problem.
Day::Day(){
    day = 1;
    weekday = 2;
    month = 1;
    year = 1901;
}

// Checks if the date should be counted by the rules of the problem.
bool Day::is_counted(){
    return weekday == 0 && day == 1; 
}

// Debugging use.
void Day::print(){
    using namespace std;
    cout << "Day: " << day << endl << "Month: " << month << endl << "Year: "
	 << year << endl;
}

bool month30(int month){
    return month == 4 || month == 6 || month == 9 || month == 11;
}

bool month31(int month){
    return month != 2 && !month30(month);
}

bool is_leap_year(int year){
    return (year % 400 == 0 || year % 100 != 0) && year % 4 == 0;
}

int main(){
    Day d;
    int count = 0;
    while (d.year < 2001){
	count += (d.is_counted()) ? 1 : 0;
	d.next_day();
    }
    std::cout << count << std::endl;
}
