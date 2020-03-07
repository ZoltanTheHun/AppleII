;	Code for MADS assembler - Apple 2 target
;	Used WUDSN IDE

;   8 white pixel plotted at (x,y) location
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
;	cls - Clear screen function		
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
; 	draw a short line	
drawShortLine 
	;ldy #$00			;resetting y index
	lda drawX			;load x choord (bases lines are occure every 8 line soooooo....
	lsr 				;divide 
	lsr 				;by 
	lsr					;8
	asl 				;and multiply by 2 because there are 2 bytes per location
    tax					;this is moved to index
    lda screen,x			;end the two bytes are picked from screen line index
    sta $fa
    inx
    lda screen,x			
    sta $fb				;store the the base line ($2000,$2080,$2100...)				
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
start   			
	jsr cls				; Clear the screen
	jsr drawShortLine	; Draw a box
	jsr hgr				; Switch to graphics
	jsr cin				; Wait for input
	jsr txtm			; Switch back to text mode
	jmp $03d0			; Return to DOS
				
screenBase .by $00 $80
drawX      .by $15		; Screen X choord
drawY      .by $1A		; Screen Y choord
screen	   .by $00 $20		; list of baselines
		 $80 $20 
		 $00 $21
		 $80 $21
		 $00 $22 
		 $80 $22 
		 $00 $23 
		 $80 $23 
		 $28 $20 
		 $A8 $20 
		 $28 $21 
		 $A8 $22 
		 $22 $28 
		 $A8 $22 
		 $28 $23 
		 $A8 $23 
		 $50 $20 
		 $D0 $20 
		 $50 $21 
		 $D0 $21
		 $50 $22
		 $D0 $22
		 $50 $23
		 $D0 $23
fineLine   .by $00
	.endp