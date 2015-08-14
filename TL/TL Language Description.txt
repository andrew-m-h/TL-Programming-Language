TL Language Desctiption. 
Written by Andrew M. Hall 2015

TL is a dynamically typed, interpreted, turing complete programming language.

Syntax:
TL borrows heavily from python, c/c++ & Visual Basic.
All lines end in a ';' (including code blocks) and the language is case sensitive.
e.g.
a = 4;
b = 5;
print a + b;

Selection:
TL supports three methods of branching execution:
	1) if then
	2) if else
	3) select

if then:
TL supports a "C like" if statment.
if (condition) {
	program
};

e.g.
andrew = 13;
if ((andrew % 2) == 0) {
	print "andrew is even!\n";
};

if else:
TL supports a "C like" if else syntax.
if (condition) {
	program
} else {
	program
};

e.g.
andrew = 13;
if ((andrew % 2) == 0) {
	print "andrew is even!\n";
} else {
	print "andrew is odd:(\n"
};

select:
TL supports a "VB like" multiway selection.
select
	case (condition) {
		program
	}
	case (condition) {
		program
	}
	.
	.
	case (True) {
		default program
	};

e.g.
andrew = 49;
select
	case (andrew < 30) {
		print "andrew is young!\n";
	}
	case (andrew < 60) {
		print "andrew is middle aged\n";
	}
	case (True) {
		print "andrew is old\n";
	};


Loops:
TL supports three types of Loop; While, For & For_Each.
while:
while iterates a program as long as a condition holds true.
while (condition) {
	program
};

e.g.
andrew = 2;
while (andrew < 100){
	print andrew;
	andrew = andrew + 2;
};

for:
for takes 4 arguments, variable, start, stop, step. variable is rebound in each iteration of the loop.
for (var, start, stop, step) {
	program (using var)
};

e.g.
for (andrew, 2, 100, 2){
	print andrew;
};

for_each:
for_each take 3 arguments, varibale, iterable and a program.
for_each var in iterable {
	program (using i)
};

e.g.
andrew = [1,2,3,4,5,6,7,8,9,10];
for_each i in andrew{
	print (str i) + "\n";
};

Data Types:
TL supports 6 Datatypes, each implicitly declared.
1) Int, an arbitrairly large integral value
2) float, a floating point value stord using the IEEE double standard
3) bool, True or False.
4) string, a string of characters, represented using ""
5) list, a list of objects. Represended using []
6) Std::IntMap, an intMap implamentation using the standard library

Functions:
TL supports side effect free functions, defined using the syntax:
def function_name(parameters,...){
	stuff to do;
	return some_value;
};

e.g.
def is_prime(p){
	ret = True;
	counter = 2;
	while (ret and (counter <= int(p/2))){
		if ((p % counter) == 0){
			ret = False;
		};
		counter += 1;
	};
	return ret;
};

for(i, 2, 100, 1){
	if (is_prime(i)){
		printLn i;
	};	
};

Subroutine:
for ease of modularity, TL supports the idea of side effecting, non-recursive, subroutines.
These allow access to global variables, changing external states and general side effecting shennegins.

Within subtoutines, one must strictly specify where variables lie in the program scope. Local variables must be initialised with
the keyword 'new', and global variables must be accessed using the keyword 'global'.
Subroutines are called using the keyword 'invoke'.


subroutine sub_name(parameters,...){
	return something;	
};

e.g.
name = "andrew";
age = 19;

subroutine print_details(){
	printLn global name;
	printLn global age;
	new k = 14;
	while (k > 0){
		printLn k;
		k -= 1;
	};
};

invoke print_details();

Type converstion:
Types can be converted freely between each other by using inbuild keywords.
str :: a -> String
int :: a -> Int 
float :: a -> Float
bool :: a -> Bool

Numeric Calculations:
The TL interpreter reads strictly Right to Left when interpreting lines. This can be manipulated by using parens.
The inbuild numeric operators are:
+, -, *, -, % and ^. All operating as in python, with "^" doing exponentiation instead of bitwise xor.

Boolean Calculations:
the inbuild boolean operators are:
!, or, and, xor, == and /=.

IO.
IO in TL is done by using the print, printLn & getline keywords.
print expression prints the expression to the screen, by doing implicit string transformation.

getline gets a line of input from the user.
m = getline; sets m to the string that the user typed in.

e.g.
def factorial (n) {
	acc = 1;
	for (count,1,n,1){
		acc *= count;
	};
	return acc;
};

print "insert n:\n";
n = int (getline);
printLn factorial(n);

Standard Libary:
TL utilises the GHC Standard Library through an easily extensible interface libaray. These functions allow the use of IO functions such as file manipulation and efficient List manipulation. An IntMap type is even completly implamented as an external interface to the Standard Library.
To access these functions, use:
Std::function_name(parameters).

standard library modules include.
	StdIO --for using haskell's wide range of IO functions
	StdList --for easy manipulation of TL lists, these functions often improve runtime
	StdIntMap --a complete implamentation of an intMap using the standard libarary
