10 HGR:HCOLOR=3
15 Y=100
17 HPLOT 0,Y TO 279,Y
20 FOR I = 0 TO 279
22 HCOLOR=1
25 HPLOT I,25*SIN(4*I*0.017)+Y
27 HCOLOR=5
28 DAMPING=(1-I*0.025)*25
29 IF DAMPING > 30 THEN DAMPING = 30
30 HPLOT I,25*SIN(4*I*0.017)+Y
40 HCOLOR=2:HPLOT I,((I/279)*25)*SIN(4*I*0.017)+Y
50 NEXT I
60 END