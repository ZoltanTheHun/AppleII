10 HOME
15 HGR2
16 HGR
17 S=0
18 Y=0
19 DIM XL(3):XL(1)=0:XL(2)=0:XL(3)=20
498 GOTO 900
499 REM "DRAW RECTANGLE"
500 FOR I=0 TO 5:HPLOT 20+I,Y TO 20+I,Y+25:NEXT:RETURN
899 REM "INIT"
900 Y=XL(3):HCOLOR=2:GOSUB 500
999 REM "FLIP SCREEN"
1000 POKE 49236+S,0:POKE 230,32+((1-S)*32):S=1-S
1100 HCOLOR=0:Y=XL(S+1):GOSUB 500:XL(3)=XL(3)+1:Y=XL(3):XL(S+1)=Y:HCOLOR=2:GOSUB 500
1110 POKE 49236,0
1120 POKE 49237,0
2000 GOTO 1000
10000 END