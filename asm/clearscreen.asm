;	WUDSN IDE example MADS source file for Apple II binary file format (".b")
;
;	Find more information on Apple II coding and APIs in the
;	Apple Assembly Line at http://www.txbobsc.com/aal
;	Apple II References at https://ia700602.us.archive.org/19/items/applerefjan78/appleIIrefjan78_text.pdf
;
;	@com.wudsn.ide.asm.hardware=APPLE2

	crout = $fd8e		; Line break out
	cout = $fded		; Character out
	cin  = $fd0c		; Character in
	hgr  = $f3de
	hgr2 = $f3d4		; Switch to HGR2
	txtm = $fb39		; Switch to text mode
	dispAT = $f836		; Display @ signs

	opt h-f+

	org $c00-4			;After the screen memory
	.word main		
	.word .len main	
	
	.proc main		
	jmp start		
cls				
	lda #$00			;initialize $2000 as the screenbase
	sta $00fa		
	lda #$20		
	sta $00fb		
newln				
	ldy #$00			;start clearing a block (block = 8 lines)
	lda #$00		
clear				
	sta ($fa),y			;clear line in a block
	iny			
	cpy #$0				;going through 256 bytes
	bne clear		
	clc			
	lda $fb				;goto next line
	adc #$1 		
	sta $fb			
	cmp #$40			;repeat until past last line ($3C)
	bne newln		
	rts    			
start   			
	jsr cls			
    jsr hgr			
	jsr cin			
	jsr txtm		
	jmp $03d0			;Return to DOS
	.endp