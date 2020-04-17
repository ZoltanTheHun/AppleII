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
	lda screen			;initialize $2000 as the screenbase
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
	
;PLOTPIXEL - PUT A PIXEL ON THE SCREEN
plotPixel
	clc
	lda screen+0
	sta tmpLow
	lda screen+1
	sta tmpHigh
	lda color
	ldy locy		; get Y choord
	ldx #$fc		; define a color mask (1111 1100) 	
sety				; find pixel
	cpy #$0
	beq draw
	asl			; 1 pixel 2 bits => shift 2
	asl
	pha			; save color
	txa			; move X to A to roll the maks
	sec			; to make sure 1 is rolled from carry
	rol
	rol
	tax			; move mask back to x
	pla			; restore color
	dey 
	jmp sety
draw
	ldy #$00
	pha			; save color before applying color mask
	txa			; get color mask
	and (tmpLow),y		; apply mask on target 4 pixel
	sta (tmpLow),y		; overwrite pixel with results
	pla			; restore color
	ora (tmpLow),y		; create final 4 pixel in register
	sta (tmpLow),y		; save colors to memory
	rts
	
;plot a couple of pixels on the screen
testColorDrawing	
	lda #$01		; load purple
	sta color		; change color
	jsr plotPixel		; Draw a pixel
	lda #$02		; load green
	sta color		; change color
	lda #$01
	sta locy
	jsr plotPixel
	lda #$03		; load white   Note: green and white next to each other will be white 1110 -> 2 high bits next to each other are white
	sta color		; change color
	lda #$02 
	sta locy
	jsr plotPixel
	rts

;MAIN PROGRAM	
start   			
	jsr cls			; Clear the screen
	jsr hgr			; Switch to graphics
	jsr testColorDrawing    ;
	jsr cin			; Wait for input
	jsr txtm		; Switch back to text mode
	jmp $03d0		; Return to DOS


;SCREEN LOCATIONS				
color	.by $00			; $00 black, $01 purple, $02 green $03 white  
locx	.by $00			; Screen X choord
locy	.by $00			; Screen Y choord
screen	.by $00 $20 		; list of baselines
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