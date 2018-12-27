#include <stdio.h>
#include <conio.h>
typedef unsigned char byte;
typedef unsigned word;

#define SET_GR *(byte*)0xC050=0
#define SET_TEXT *(byte*)0xC051=0
#define SET_FULLSCREEN *(byte*)0xC052=0
#define SET_PAGE1 *(byte*)0xC054=0
#define SET_PAGE2 *(byte*)0xC055=0
#define HGR *(byte*)0xC057=0
#define CLEAR_HGR *(byte*)0xC056=0


byte* hiPage1=(byte*)0x2000;
byte* hiPage2=(byte*)0x4000;
byte* baseScreenAddress=(byte*)0x2000;

/*
    Base addresses as per Apple IIe reference manual.
	Not actual lines, more like rows of the text mode.
	Row 9 is before row 1 and so on... 
	It is easier to use a table to address them than 
	calculate them on the fly
*/
byte* screenRow[24]={
    (byte*)0x2000,
	(byte*)0x2080,
	(byte*)0x2100,
	(byte*)0x2180,
	(byte*)0x2200,
	(byte*)0x2280,
	(byte*)0x2300,
	(byte*)0x2380,
	(byte*)0x2028,
	(byte*)0x20A8,
	(byte*)0x2128,
	(byte*)0x21A8,
	(byte*)0x2228,
	(byte*)0x22A8,
	(byte*)0x2328,
	(byte*)0x23A8,
	(byte*)0x2050,
	(byte*)0x20D0,
	(byte*)0x2150,
	(byte*)0x21D0,
	(byte*)0x2250,
	(byte*)0x22D0,
	(byte*)0x2350,
	(byte*)0x23D0
};

/*
    Test program to clear a high resolution screen row by row (1 row = 8 lines)
	then to switch back to text mode.
*/
void main(void){
	char c;
	int i=0;
	int j=0;
	printf("\n");
	printf("Testing test input/output\n");
	c = getchar();
	printf("\n");
	printf("Char read: \n");
	putchar(c);
	printf("\n");
	printf("Press any key to continue. \n");
	getchar();
	/* First switch from text mode to graphics mode then to high res */
	SET_GR;
	HGR;
	SET_FULLSCREEN;
	SET_PAGE1;
	for(j=0;j<24;j++){
		baseScreenAddress=screenRow[j];
		for(i=0;i<40;i++){
			/* 
			   Apple II has this crazy screen structure that line 0,8,16... 
			   follow each other so line 1,2,... is 1K,2K,... off.
			*/
			*(baseScreenAddress+i)=(byte)0xFF;
			*(baseScreenAddress+i+1024)=(byte)0xFF;
			*(baseScreenAddress+i+2048)=(byte)0xFF;
			*(baseScreenAddress+i+3072)=(byte)0xFF;
			*(baseScreenAddress+i+4096)=(byte)0xFF;
			*(baseScreenAddress+i+5120)=(byte)0xFF;
			*(baseScreenAddress+i+6144)=(byte)0xFF;
			*(baseScreenAddress+i+7168)=(byte)0xFF;
		}
	}
	//while(1);
	SET_TEXT;
}