10 HOME
15 HGR2
20 GOTO 500
30 FOR I=0 TO 5:HPLOT 20+I,Y TO 20+I,Y+25:NEXT:RETURN
500 Y=20
520 FOR J=0 TO 100:HCOLOR=0:GOSUB 30:Y=Y+1:HCOLOR=2:GOSUB 30:NEXT
10000 END