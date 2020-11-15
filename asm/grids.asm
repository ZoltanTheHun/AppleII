;	WUDSN IDE example MADS source file for Apple II binary file format (".b")
;
;	Find more information on Apple II coding and APIs in the
;	Apple Assembly Line at http://www.txbobsc.com/aal
;	Apple II References at https://ia700602.us.archive.org/19/items/applerefjan78/appleIIrefjan78_text.pdf
;
;	@com.wudsn.ide.asm.hardware=APPLE2

	crout    = $fd8e	; Line break out
	cout     = $fded	; Character out
	cin      = $fd0c	; Character in
	hgr      = $f3de
	hgr2     = $f3d4	; Switch to HGR2
	txtm     = $fb39	; Switch to text mode
	dispAT   = $f836	; Display @ signs
	tmpLow   = $fa		; 0 page location for temporary storage
	tmpHigh  = $fb		; 0 page location for temporary storage
	charLow  = $fc
	charHigh = $fd
	tmpy     = $fe
	tmpy2    = $ff
	tmpx     = $eb

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
	ldx #$00   
	stx tmpx                    
drawQuadrant1
	lda screen,x 
	sta tmpLow
	inx
	lda screen,x
	sta tmpHigh
	txa
	pha                     ; saving block number
	ldy #$00
	;ldx #$00
	ldx tmpx
drawGrid
	txa
	pha    
	lda grid,x              ; load character of grid(x) 
	sta charSel
	jsr drawCharacter
	iny                     ; y register controls the x choordinate of the selected grid row
	pla
	tax
	inx
	sbc tmpx                 ;
	cmp #$26                 ; $28 = 40 different grids on a single line, because 7 bit = 3,5 pixel 280 res / 7 bit = 40
	bne drawGrid
	stx tmpx
	pla                      ; restoring block number
	tax
	inx	
	cpx #$0C                 ;$30 = 48, there are 44/2  8px grid lines on screen
	bne drawQuadrant1 
	lda #$0
	sta tmpx
drawQuadrant2
	lda screen,x 
	sta tmpLow
	inx
	lda screen,x
	sta tmpHigh
	txa
	pha                     ; saving block number
	ldy #$00
	;ldx #$00
	ldx tmpx
drawGrid2
	txa
	pha    
	lda grid2,x              ; load character of grid(x) 
	sta charSel
	jsr drawCharacter
	iny                     ; y register controls the x choordinate of the selected grid row
	pla
	tax
	inx
	sbc tmpx                 ;
	cmp #$26                 ; $28 = 40 different grids on a single line, because 7 bit = 3,5 pixel 280 res / 7 bit = 40
	bne drawGrid2
	stx tmpx
	pla                      ; restoring block number
	tax
	inx	
	cpx #$18                 ;$30 = 48, there are 44/2  8px grid lines on screen
	bne drawQuadrant2
	lda #$0
	sta tmpx
drawQuadrant3
	lda screen,x 
	sta tmpLow
	inx
	lda screen,x
	sta tmpHigh
	txa
	pha                     ; saving block number
	ldy #$00
	;ldx #$00
	ldx tmpx
drawGrid3
	txa
	pha    
	lda grid3,x              ; load character of grid(x) 
	sta charSel
	jsr drawCharacter
	iny                     ; y register controls the x choordinate of the selected grid row
	pla
	tax
	inx
	sbc tmpx                 ;
	cmp #$26                 ; $28 = 40 different grids on a single line, because 7 bit = 3,5 pixel 280 res / 7 bit = 40
	bne drawGrid3
	stx tmpx
	pla                      ; restoring block number
	tax
	inx	
	cpx #$24                 ;$30 = 48, there are 44/2  8px grid lines on screen
	bne drawQuadrant3
	lda #$0
	sta tmpx
drawQuadrant4
	lda screen,x 
	sta tmpLow
	inx
	lda screen,x
	sta tmpHigh
	txa
	pha                     ; saving block number
	ldy #$00
	;ldx #$00
	ldx tmpx
drawGrid4
	txa
	pha    
	lda grid4,x              ; load character of grid(x) 
	sta charSel
	jsr drawCharacter
	iny                     ; y register controls the x choordinate of the selected grid row
	pla
	tax
	inx
	sbc tmpx                 ;
	cmp #$26                 ; $28 = 40 different grids on a single line, because 7 bit = 3,5 pixel 280 res / 7 bit = 40
	bne drawGrid4
	stx tmpx
	pla                      ; restoring block number
	tax
	inx	
	cpx #$30                 ;$30 = 48, there are 44/2  8px grid lines on screen
	bne drawQuadrant4
	rts

; this function draws an 8x8 character on the screen	
drawCharacter 
	lda tmpHigh             ; drawing each line of character updates the high byte of screen address
	pha                     ; therefore we need to save it
	jsr loadCharacterBank   ; load the correct character bank based on the xxxx 0000 part of the grid value
	sty tmpY                ; save y register, that represents the x choord in the screen grid
	lda charSel             ; now we need to select the character from the bank
	and #$0F                ; let's remove the bank selector bit by keeping 0000 xxxx
	asl                     ; character values are 0 to 7 that we multiply with 8 to get the byte offset (this part needs to be revised, we can store 32 characters in 1 block, 32*8 = 256)
	asl
	asl
	tay                     ; now the character selector will be the bank + y offset
	jsr drawNextLine
	iny                     ; offset increased each time to fetch next 8 bit of character for the next character line
	jsr drawNextLine
	iny
	jsr drawNextLine
	iny
	jsr drawNextLine
	iny
	jsr drawNextLine
	iny
	jsr drawNextLine
	iny
	jsr drawNextLine
	iny
	jsr drawNextLine
	ldy tmpY
	pla	                 ; once drawing finished
	sta tmpHigh              ; the high address of screen is restored
	rts
	
loadCharacterBank
	lda charSel
	and #$F0                 ;bit mask 1111 0000, we need 4 high bits to decide which bank to use
bank0
	cmp #$00                 
	bne bank1
	lda <chara
	sta charLow
	lda >chara
	sta charHigh
	rts
bank1
	cmp #$10                 
	bne bank2
	lda <charb
	sta charLow
	lda >charb
	sta charHigh
	rts
bank2
	cmp #$20
	bne bank3
	lda <charc
	sta charLow
	lda >charc
	sta charHigh
	rts
bank3
	cmp #$30
	bne bank4
	lda <chard
	sta charLow
	lda >chard
	sta charHigh
	rts
bank4
	cmp #$40
	bne bank5
	lda <chare
	sta charLow
	lda >chare
	sta charHigh
	rts
bank5
	cmp #$50
	bne bank6
	lda <charf
	sta charLow
	lda >charf
	sta charHigh
	rts
bank6
	cmp #$60
	bne bank7
	lda <charg
	sta charLow
	lda >charg
	sta charHigh
	rts
bank7   cmp #$70
        lda <charh
	sta charLow
	lda >charh
	sta charHigh
        rts

; drawNextLine copies 1 byte from block + y offset to 1 8 bit line segment of a grid location, then increases the line number
drawNextLine
	lda (charLow),y        ; load char bank + y offset to A register
	sty tmpy2              ; save offset to be able to use in next line
	ldy tmpY               ; load grid offset on x axis
	sta (tmpLow),y         ; store the character at the screen address + y offset
	ldy tmpy2              ; restore the character bank offset
	inc tmpHigh            ; next line on a 8 bit grid is $40 away
	inc tmpHigh
	inc tmpHigh
	inc tmpHigh
	rts
	
;MAIN PROGRAM	
start   		
	jsr cls			; Clear the screen
	jsr hgr			; Switch to graphics
tst	jsr testplot
	inc grid
	inc chara
	inc chara+1
	inc chara+2
	inc chara+3
	inc chara+4
	inc chara+5
	inc chara+6
	inc chara+7
	jmp tst
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
grid    .by $20 $01 $00 $01 $00 $01 $00 $01 $00 $01 $00 $01 $00 $01 $00 $01 $00 $02 $00 $03 $00 $01 $00 $02 $00 $03 $00 $02 $00 $03 $00 $01 $00 $02 $00 $02 $00 $02 $20 $04    
        ;.by $00 $08 $00 $08 $00 $08 $00 $08 $00 $08 $00 $08 $00 $08 $00 $08 $00 $10 $00 $18 $00 $08 $00 $10 $00 $18 $00 $10 $00 $18 $00 $08 $00 $10 $00 $10 $00 $10 $20 $28 ; a single line of screen in the grid, 
        .by $08 $20 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $02
grid2   .by $05 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $01
grid3   .by $03 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $05
grid4   .by $06 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $02
gridSel .by $00
charSel .by $00                 ; character selector


;chard   .by $00 $00 $00 $00 $00 $00 $00 $00 ;the displayed character
; ideally we will have 256 different characters, that would take 256*8 byte = 2048 byte = 2 kbyte memory
chara   .by $00 $00 $00 $00 $00 $00 $00 $00
        .by $D5 $D5 $D5 $D5 $D5 $D5 $D5 $D5
        .by $2A $2A $2A $2A $2A $2A $2A $2A
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $AA $D5 $AA $D5 $AA $D5 $AA $D5
        .by $55 $00 $55 $00 $55 $00 $55 $00
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $55 $55 $55 $55 $55 $55 $55 $55
charb   .by $00 $00 $00 $00 $00 $00 $00 $00
        .by $AA $D5 $AA $D5 $AA $D5 $AA $D5
        .by $2A $2A $2A $2A $2A $2A $2A $2A
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $AA $D5 $AA $D5 $AA $D5 $AA $D5
        .by $55 $00 $55 $00 $55 $00 $55 $00
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $55 $55 $55 $55 $55 $55 $55 $55
charc   .by $D5 $AA $AA $AA $AA $AA $AA $AA
        .by $D5 $D5 $D5 $D5 $D5 $D5 $D5 $D5
        .by $2A $2A $2A $2A $2A $2A $2A $2A
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $AA $D5 $AA $D5 $AA $D5 $AA $D5
        .by $55 $00 $55 $00 $55 $00 $55 $00
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $55 $55 $55 $55 $55 $55 $55 $55
chard   .by $AA $AA $AA $AA $AA $AA $AA $AA
        .by $D5 $D5 $D5 $D5 $D5 $D5 $D5 $D5
        .by $2A $2A $2A $2A $2A $2A $2A $2A
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $AA $D5 $AA $D5 $AA $D5 $AA $D5
        .by $55 $00 $55 $00 $55 $00 $55 $00
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $55 $55 $55 $55 $55 $55 $55 $55
chare   .by $AA $AA $AA $AA $AA $AA $AA $AA
        .by $D5 $D5 $D5 $D5 $D5 $D5 $D5 $D5
        .by $2A $2A $2A $2A $2A $2A $2A $2A
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $AA $D5 $AA $D5 $AA $D5 $AA $D5
        .by $55 $00 $55 $00 $55 $00 $55 $00
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $55 $55 $55 $55 $55 $55 $55 $55
charf   .by $AA $AA $AA $AA $AA $AA $AA $AA
        .by $D5 $D5 $D5 $D5 $D5 $D5 $D5 $D5
        .by $2A $2A $2A $2A $2A $2A $2A $2A
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $AA $D5 $AA $D5 $AA $D5 $AA $D5
        .by $55 $00 $55 $00 $55 $00 $55 $00
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $55 $55 $55 $55 $55 $55 $55 $55
charg   .by $AA $AA $AA $AA $AA $AA $AA $AA
        .by $D5 $D5 $D5 $D5 $D5 $D5 $D5 $D5
        .by $2A $2A $2A $2A $2A $2A $2A $2A
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $AA $D5 $AA $D5 $AA $D5 $AA $D5
        .by $55 $00 $55 $00 $55 $00 $55 $00
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $55 $55 $55 $55 $55 $55 $55 $55
charh   .by $00 $00 $00 $00 $AA $AA $AA $AA
        .by $D5 $D5 $D5 $D5 $D5 $D5 $D5 $D5
        .by $2A $2A $2A $2A $2A $2A $2A $2A
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $AA $D5 $AA $D5 $AA $D5 $AA $D5
        .by $55 $00 $55 $00 $55 $00 $55 $00
        .by $55 $55 $55 $55 $55 $55 $55 $55
        .by $55 $55 $55 $55 $55 $55 $55 $55
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
	.by $A8 $21
	.by $A8 $22
	.by $28 $22
	.by $A8 $23
	.by $28 $23
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
	