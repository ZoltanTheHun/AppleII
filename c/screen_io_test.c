#include <stdio.h>
#include <stdlib.h>
#include <conio.h>

void main(void){
	char st[200];
	clrscr();
	printf("ZPONG\n");
	printf("READ/WRITE TEST\n");
	cscanf ("%[^\r]s", st);
	printf("\nInput goes to output:\n%s",st);
}