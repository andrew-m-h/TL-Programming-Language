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