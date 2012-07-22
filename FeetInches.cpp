//Implementation for FeetInches class
#include <cstdlib>
#include "FeetInches.h"
#include <iostream>

int main()

{
	int feet,inches;	//to hold inputs
	
	//three Feetinches objects
	FeetInches first,second,third;
	
	//get a distance input
	std::cout << "enter a distance in feet and inches: ";
	std::cin >> feet >> inches;
	
	//store the distance in the first class instance
	first.setFeet(feet);
	first.setInches(inches);
	
	//get a second distance
	std::cout << "enter another distance in feet and inches: ";
	std::cin >> feet >> inches;
	
	//store the distance in the second class instance
	second.setFeet(feet);
	second.setInches(inches);
	
	
	//compare the two objects
	if (first != second)
		std::cout << "first != second.\n";
	if(first <= second)
		std::cout << "first <= second.\n";
	if(first >= second)
		std::cout << "first >= second.\n";
	
	


	return 0;
}



//simplify function adjusts feet and inches number so that they conform to standard feet/inches expressions
void FeetInches::simplify()
{
	if (inches >= 12)
	{
		feet+= (inches / 12);
		inches = inches % 12;
	}
	else if (inches < 0)
	{
		feet -= ((abs(inches)/12) + 1);
		inches = 12 - (abs(inches)%12);
	}
}

//overloaded relational operators
bool FeetInches::operator <= (const FeetInches &right)
{
	bool status;
	
	if (feet < right.feet)
		status = true;
	else if (feet == right.feet && (inches < right.inches || inches == right.inches))
			status = true;
	else 
		status = false;
		
	return status;
}

//overloaded > = relational operator
bool FeetInches::operator >= (const FeetInches &right)
{
	bool status;
	
	if (feet > right.feet)
		status = true;
	else if (feet == right.feet && (inches > right.inches || inches == right.inches))
			status = true;
	else 
		status = false;
		
	return status;
}

bool FeetInches::operator != (const FeetInches &right)
{
	bool status;
	
	if (feet != right.feet || inches != right.inches)
		status = true;
	else
		status = false;
		
	return status;
}

//overloaded binary operators
FeetInches FeetInches::operator + (const FeetInches &right)
{
	FeetInches temp;
	
	temp.inches = inches + right.inches;
	temp.feet = feet + right.feet;
	temp.simplify();
	return  temp;
}


FeetInches FeetInches::operator - (const FeetInches &right)
{
	FeetInches temp;
	
	temp.inches = inches - right.inches;
	temp.feet = feet - right.feet;
	temp.simplify();
	return  temp;
}