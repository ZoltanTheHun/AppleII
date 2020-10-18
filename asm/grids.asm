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
	tmpLow = $fa		; 0 page location for temporary storage
	tmpHigh = $fb		; 0 page location for temporary storage

	opt h-f+

	org $c00-4			;After the screen memory
	.word main		
	.word .len main	
	
	.proc main		
	jmp start	

;CLS - CLEAR SCREEN FUNCTION	
cls				
	lda screen+0			;initialize $2000 as the screenbase
	sta tmpLow	
	lda screen+1		
	sta tmpHigh	
newln				
	ldy #$00			;start clearing a block (block = 8 lines)
	lda #$00			;color is black
clear				
	sta (tmpLow),y			;clear line in a block
	iny			
	cpy #$0				;going through 256 bytes
	bne clear		
	clc				;goto next block
	lda tmpHigh				
	adc #$1 		
	sta tmpHigh			
	cmp #$40			;repeat until end of page1
	bne newln		
	rts
	
;test color drawing in x axis
testplot
	lda #$AA                ; load purple
	sta color               ; change color
        clc                     ; store next screen address into tmp low and high
	lda screen+0 
	sta tmpLow
	lda screen+1
	sta tmpHigh
	ldy #$00
drawGrid	
        ldx gridSel      
	lda grid,x              ; load character of grid(x) 
	sta charSel
	jsr drawCharacter
        iny                     ; y register controls the x choordinate of the selected grid row
	inc gridSel
	ldx gridSel
	cpx #$28                 ; $28 = 40 different grids on a single line, because 7 bit = 3,5 pixel 280 res / 7 bit = 40
	bne drawGrid
	rts

; this function draws an 8x8 character on the screen	
drawCharacter 
	lda tmpHigh             ; drawing each line of character updates the high byte of screen address
	pha                     ; therefore we need to save it
        ldx charSel
        jsr drawNextLine
        inx
        jsr drawNextLine
        inx
        jsr drawNextLine
        inx
        jsr drawNextLine
        inx
        jsr drawNextLine
        inx
        jsr drawNextLine
        inx
        jsr drawNextLine
        inx
        jsr drawNextLine
        pla	                 ; once drawing finished
        sta tmpHigh              ; the high address of screen is restored
        rts
; drawNextLine copies 1 byte from (chara + x) to 1 line of a grid location, then increases the line number
drawNextLine
	lda chara,x
	sta (tmpLow),y
	inc tmpHigh
	inc tmpHigh
	inc tmpHigh
	inc tmpHigh
	rts
	
;MAIN PROGRAM	
start   			
	jsr cls			; Clear the screen
	jsr hgr			; Switch to graphics
	jsr testplot
	jsr cin			; Wait for input
	jsr txtm		; Switch back to text mode
	jmp $03d0		; Return to DOS


;SCREEN LOCATIONS	
lIndx   .by $00                 ; loop	index		
color	.by $00			; $00 black, $01 purple, $02 green $03 white  
color2  .by $00
locx	.by $00			; Screen X choord
locy	.by $00			; Screen Y choord
actScr  .by $00                 ; Signals if screen1 (0) or screen2 (1) is active for drawing


; In high res mode, the complete screen is 280x192, and from each byte, the 1st is reserved for the NTSC shift selector.
; If 7 bit sets the color, to encode 280 pixels, this requires (280 pixel / 7 bit ) 40 bytes. To encode 192 lines, it will take 192 line x 40 bytes = 7680 bytes = screen memory 
; This means that with double buffering 15360 bytes (15k) is used for screen memory. As screen memory is not continous, and contains holes, actual size is I think 16k (todo: verify)
; We plan to do 7 bit x 8 line grid based rendering, so we need only 24 rows (192 lines / 8 lines). 40 bytes x 24 bytes = 960 bytes.
; Because 960 bytes are not addressable by a single register, we will divide our grid to 240 bytes of units (960 / 4).
grid    .by $00 $08 $00 $08 $00 $08 $00 $08 $00 $08 $00 $08 $00 $08 $00 $08 $00 $10 $00 $18 $00 $08 $00 $10 $00 $18 $00 $10 $00 $18 $00 $08 $00 $10 $00 $10 $00 $10 $20 $28 ; a single line of screen in the grid, 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
grid2   .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
grid3   .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
grid4   .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
gridSel .by $00
charSel .by $00                 ; character selector

; ideally we will have 256 different characters, that would take 256*8 byte = 2048 byte = 2 kbyte memory
chara   .by $AA $AA $AA $AA $AA $AA $AA $AA
chara1  .by $D5 $D5 $D5 $D5 $D5 $D5 $D5 $D5
chara2  .by $2A $2A $2A $2A $2A $2A $2A $2A
chara3  .by $55 $55 $55 $55 $55 $55 $55 $55
chara4  .by $AA $D5 $AA $D5 $AA $D5 $AA $D5
chara5  .by $55 $00 $55 $00 $55 $00 $55 $00
chara6  .by $55 $55 $55 $55 $55 $55 $55 $55
chara7  .by $55 $55 $55 $55 $55 $55 $55 $55
screen	.by $00 $20 		; list of baselines, each line will be the next 8 lines
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
screen2 .by $00 $40
fineLine   .by $00
	.endp