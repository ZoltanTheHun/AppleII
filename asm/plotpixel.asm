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

;CLS - CLEAR SCREEN FUNCTION	
cls				
	lda #$00			;initialize $2000 as the screenbase
	sta $fa		
	lda #$20		
	sta $fb		
newln				
	ldy #$00			;start clearing a block (block = 8 lines)
	lda #$00			;color is black
clear				
	sta ($fa),y			;clear line in a block
	iny			
	cpy #$0				;going through 256 bytes
	bne clear		
	clc					;goto next block
	lda $fb				
	adc #$1 		
	sta $fb			
	cmp #$40			;repeat until end of page1
	bne newln		
	rts
	
	
;DRAW SHORT LINE - DRAW A LINE 8 PIXEL WIDE
drawShortLine 
	lda drawX			;load x choord (bases lines are occure every 8 line soooooo....
	lsr 				;divide 
	lsr 				;by 
	lsr					;8
	asl 				;and multiply by 2 because there are 2 bytes per location
        tax				;this is moved to index
        lda screen,x	;end the two bytes are picked from screen line index
        sta $fa
        inx
        lda screen,x			
        sta $fb			;store the the base line ($2000,$2080,$2100...)				
	lda drawX			;and load x choord again to determine 1 line in the block of 1-8 lines				
	and #$07			;for this we need 3 bits only (as this represent maximum 8 lines)
	beq draw			;if this is the base line (0) then let's draw
setx					;otherwise increase the the line number maximum 8 times
	pha					;store our remaining line number
	lda $fb				;each line in a block is $400 address away,
	clc					;
	adc #$04			;so we go to the next line
	sta $fb				;
	pla					;we restore our remaining line numbers	
	sec					;
	sbc #$01			;remaining line numbers decreased by one
	bne setx			;if reach 0 go ahead
draw	
	clc
	lda $fa				;initialize screenbase to either $2000 or $2080
	adc drawY 
	sta $fa		
	lda #$ff			;color is white
	sta ($fa),y
	rts
	
;MAIN PROGRAM	
start   			
	jsr cls				; Clear the screen
	jsr drawShortLine	; Draw a box
	jsr hgr				; Switch to graphics
	jsr cin				; Wait for input
	jsr txtm			; Switch back to text mode
	jmp $03d0			; Return to DOS


;SCREEN LOCATIONS				
screenBase .by $00 $80
drawX      .by $15			; Screen X choord
drawY      .by $01			; Screen Y choord
screen	   .by $00 $20 		; list of baselines
    .by $80 $20 
	.by $00 $21
	.by $80 $21
	.by $00 $22
	.by $80 $22
	.by $00 $23
	.by $80 $23
	.by $28 $20
	.by $A8 $20
	.by $28 $21
	.by $A8 $22
	.by $22 $28
	.by $A8 $22
	.by $28 $23
	.by $A8 $23
	.by $50 $20
	.by $D0 $20
	.by $50 $21
	.by $D0 $21
	.by $50 $22
	.by $D0 $22
	.by $50 $23
	.by $D0 $23
fineLine   .by $00
	.endp