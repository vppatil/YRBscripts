#ifndef FEETINCHES_H
#define FEETINCHES_H

//demo code from book

class FeetInches
{
	private:
		int feet; 	//To hold a number of feet
		int inches; //To hold a number of inches
		void simplify();	//Defined in feetinches.cpp
	
	public:
		//Constructor
		FeetInches(int f=0,int i =0)
			{
				feet = f;
				inches = i;
				simplify();
			}
			
		//Mutator functions
		void setFeet(int f)
			{
				feet = f;
			}
		
		void setInches(int i)
			{
				inches=i;
			}
			
		//Accessor functions
		int getFeet() const
			{
				return feet;
			}
		
		int getInches() const
			{
				return inches;
			}
			
		//overloaded operators
		FeetInches operator + (const FeetInches &); //Overloaded +
		FeetInches operator - (const FeetInches &); //Overloaded -
		FeetInches operator <= (const FeetInches &); //Overloaded <=
		FeetInches operator >= (const FeetInches &); //Overloaded >=
		FeetInches operator != (const FeetInches &); //Overloaded !=


		
	};
	
	#endif