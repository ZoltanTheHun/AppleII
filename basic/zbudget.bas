10 PRINT "STARTING ZBUDGET 0.01"
15 ONERR GOTO 11000
20 GOTO 10000
100 REM "LOAD DB ROUTINE"
110 PRINT CHR$(4) "OPEN ZBUDGETDB"
120 PRINT CHR$(4) "READ ZBUDGETDB"
130 INPUT CP
131 INPUT IN
140 PRINT CHR$(4) "CLOSE ZBUDGETDB"
150 RETURN
200 REM "WRITE DB ROUTINE"
210 PRINT CHR$(4) "OPEN ZBUDGETDB"
220 PRINT CHR$(4) "WRITE ZBUDGETDB"
230 PRINT CP
231 PRINT IN
240 PRINT CHR$(4) "CLOSE ZBUDGETDB"
250 RETURN
300 REM "NEW INPUT ROUTINE"
310 PRINT "PLEASE ENTER CAPITAL"
320 INPUT "";CP
330 PRINT "PLEASE ENTER INTEREST"
340 INPUT "";IN
350 RETURN
400 REM "DISPLAY FINANCIALS"
409 POKE 37,1: CALL -922: CALL -998: POKE 36,0:PRINT "CAPITAL   !INTEREST           !PAID"
420 POKE 37,2: CALL -922: CALL -998: POKE 36,0:PRINT CP
460 POKE 37,2: CALL -922: CALL -998: POKE 36,10:PRINT "!"IN
470 POKE 37,2: CALL -922: CALL -998: POKE 36,30:PRINT "!"CP+CP*IN
480 RETURN
10000 GOSUB 100
11000 HOME
11001 PRINT "ZBUDGET FINANCIALS"
11002 GOSUB 400
11010 PRINT "PLEASE SELECT FROM OPTIONS BELOW"
11020 PRINT ""
11040 PRINT "1) STORE FINANCIALS"
11050 PRINT "2) NEW FINANCIALS"
11080 PRINT "3) EXIT"
11090 INPUT OP
11120 IF OP = 1 THEN GOSUB 200
11130 IF OP = 2 THEN GOSUB 300
11140 IF OP = 3 THEN GOSUB 20010
20000 GOTO 11000
20010 END
