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
drawScreen1   
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
	
	
;test color drawing in x axis
drawScreen2   
	ldx #$00   
	stx tmpx   
drawQuadrant1_2
	lda screen2,x 
	sta tmpLow
	inx
	lda screen2,x
	sta tmpHigh
	txa
	pha                     ; saving block number
	ldy #$00
	ldx tmpx
drawGrid_2
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
	bne drawGrid_2
	stx tmpx
	pla                      ; restoring block number
	tax
	inx	
	cpx #$0C                 ;$30 = 48, there are 44/2  8px grid lines on screen
	bne drawQuadrant1_2 
	lda #$0
	sta tmpx
drawQuadrant2_2
	lda screen2,x 
	sta tmpLow
	inx
	lda screen2,x
	sta tmpHigh
	txa
	pha                     ; saving block number
	ldy #$00
	ldx tmpx
drawGrid2_2
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
	bne drawGrid2_2
	stx tmpx
	pla                      ; restoring block number
	tax
	inx	
	cpx #$18                 ;$30 = 48, there are 44/2  8px grid lines on screen
	bne drawQuadrant2_2
	lda #$0
	sta tmpx
drawQuadrant3_2
	lda screen2,x 
	sta tmpLow
	inx
	lda screen2,x
	sta tmpHigh
	txa
	pha                     ; saving block number
	ldy #$00
	ldx tmpx
drawGrid3_2
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
	bne drawGrid3_2
	stx tmpx
	pla                      ; restoring block number
	tax
	inx	
	cpx #$24                 ;$30 = 48, there are 44/2  8px grid lines on screen
	bne drawQuadrant3_2
	lda #$0
	sta tmpx
drawQuadrant4_2
	lda screen2,x 
	sta tmpLow
	inx
	lda screen2,x
	sta tmpHigh
	txa
	pha                     ; saving block number
	ldy #$00
	ldx tmpx
drawGrid4_2
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
	bne drawGrid4_2
	stx tmpx
	pla                      ; restoring block number
	tax
	inx	
	cpx #$30                 ;$30 = 48, there are 44/2  8px grid lines on screen
	bne drawQuadrant4_2
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

;
; loadCharacterBank - Routine to load the location of 8 character patterns (a 64 byte long bank) from the available 64 character patterns	
;
; var charSel       - The id of the desired pattern, a new bank starts for every 8th pattern
; prc bank0-7       - Bank loader routine for a 64 byte long 
;
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
bank7
	cmp #$70
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
	
;
; moveSprite - Routine that can move a 8 bit wide sprite in the first quadrant of the screen
; This routine is using 
; Sprite is defined as $00 $00 $00 that is: X, Y, character pattern ID
; tmpLow - the storage of the address of the sprite
moveSprite
	; load $00 $00 $00 (3 bytes) of sprite information
	ldy #$00		; select the firt byte of the current sprite
	lda (tmpLow),y          ; which will contain x
	sta sptX                ; and save it to sptX
	iny 			; select next byte of the current sprite
	lda (tmpLow),y		; which will contain y
	sta sptY		; and save it to sptY
	iny                     ; select the third byte of the current sprite
	lda (tmpLow),y          ; which will contain the sprite pattern id
	sta sptCh		; and save it to sptCh
	;draw sprite
	lda >grid
	sta tmpHigh
	lda <grid               ; load the start address of the grid 
	; Select the row of the grid where the sprite will be placed
	ldx sptY		; Load Y choordinate
moveSpriteRow			
	cpx #$00  		; while x > 0 increment the row choord
	beq moveSpriteColumn
	clc 			; register A contains the grid'
	adc #$28                ; there are 40 ($28) bytes  in a row
	pha			; if there is a carry, that means that the low address had an overflow
	lda tmpHigh             ; so we need to increase the high address
	adc #$00		; we do this all the time to avoid a branch instruction
	sta tmpHigh
	pla			; restore the low address
	dex			; reduce X and go to next row
	jmp moveSpriteRow  
moveSpriteColumn                ;select columnn of grid
	clc			; prepare for addition
	adc sptX		; grid + x will be the l
	sta tmpLow
	lda tmpHigh             ; similarly to the row selection,
	adc #$00		; if there is an overflow in the low address 
	sta tmpHigh             ; we need to increase the high address
	lda sptCh
	ldy #$00
	sta (tmpLow),y
	rts

;
; swapChara - Routine that creates the changing patterns in the grid screen as a demo effect
;
swapChara	
	inc chara
	inc chara+1
	inc chara+2
	inc chara+3
	inc chara+4
	inc chara+5
	inc chara+6
	inc chara+7
	rts

;
; drawTank - Routine that clears the previous location of the player tank, then draws the tank to the new location
;
drawTank
	; clear back of tank
	lda <tankOld
	sta tmpLow
	lda >tankOld
	sta tmpHigh
	jsr moveSprite
	; clear front of tank
	lda <tankOld1
	sta tmpLow
	lda >tankOld1
	sta tmpHigh
	jsr moveSprite
	; draw back of tank
	lda <tank
	sta tmpLow
	lda >tank
	sta tmpHigh
	jsr moveSprite
	; draw front of tank
	lda <tank1
	sta tmpLow
	lda >tank1
	sta tmpHigh
	jsr moveSprite
	rts
	
;
; moveTank - Routine that updates the location of the player tank
; 
moveTank
	lda tank
	sta tankOld
	inc tank
	lda tank1
	sta tankOld1
	inc tank1
	rts
	
;MAIN PROGRAM	
start   		
	jsr cls			; Clear the screen
	jsr hgr
	; screen 1 drawing starts here	
tst	sta $C055		; show lgr 2 while drawing lgr 1
	jsr drawTank
	jsr moveTank
	jsr drawScreen1
	inc grid
	jsr swapChara
	;jsr cin
	;wait for vertical blank
vbl	lda $C019
	cmp #$80
	bpl vbl
	; screen 2 drawing starts here	
	sta $C054              ;show lgr 1 while drawing lgr 2
	jsr drawTank
	jsr moveTank
	jsr drawScreen2
	inc grid
	jsr swapChara
	;wait for vertical blank
vbl2	lda $C019
	cmp #$80
	bpl vbl2
	;wait for vertical blank
        ;jsr cin
	jmp tst
	jsr cin			; Wait for input
	jsr txtm		; Switch back to text mode
	jmp $03d0		; Return to DOS


;SCREEN LOCATIONS	
lIndx   .by $00			; loop	index		
color	.by $00			; $00 black, $01 purple, $02 green $03 white  
color2  .by $00
locx	.by $00			; Screen X choord
locy	.by $00			; Screen Y choord
actScr  .by $00			; Signals if screen1 (0) or screen2 (1) is active for drawing


; In high res mode, the complete screen is 280x192, and from each byte, the 1st is reserved for the NTSC shift selector.
; If 7 bit sets the color, to encode 280 pixels, this requires (280 pixel / 7 bit ) 40 bytes. To encode 192 lines, it will take 192 line x 40 bytes = 7680 bytes = screen memory 
; This means that with double buffering 15360 bytes (15k) is used for screen memory. As screen memory is not continous, and contains holes, actual size is I think 16k (todo: verify)
; We plan to do 7 bit x 8 line grid based rendering, so we need only 24 rows (192 lines / 8 lines). 40 bytes x 24 bytes = 960 bytes.
; Because 960 bytes are not addressable by a single register, we will divide our grid to 240 bytes of units (960 / 4).
grid    .by $20 $01 $00 $01 $00 $01 $00 $01 $00 $01 $00 $01 $00 $01 $00 $01 $00 $02 $00 $03 $00 $01 $00 $02 $00 $03 $00 $02 $00 $03 $00 $01 $00 $02 $00 $02 $00 $02 $20 $04    
        .by $08 $20 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $67 $66 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $02
grid2   .by $05 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $67 $66 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $67 $66 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $67 $66 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $01
grid3   .by $03 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $67 $66 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $67 $66 $00 $00 $00 $00 
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $05
grid4   .by $06 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $67 $66 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $67 $66 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
        .by $00 $00 $67 $66 $00 $00 $00 $67 $66 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
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
charg   
	.by $AA $AA $AA $AA $AA $AA $AA $AA
	.by $D5 $D5 $D5 $D5 $D5 $D5 $D5 $D5
	.by $2A $2A $2A $2A $2A $2A $2A $2A
	.by $55 $55 $55 $55 $55 $55 $55 $55
	.by $AA $D5 $AA $D5 $AA $D5 $AA $D5
	.by $55 $00 $55 $00 $55 $00 $55 $00
	.by $00 $03 $07 $06 $0C $78 $7F $0C
	.by $00 $00 $F8 $10 $18 $1E $FE $30
charh
	.by $00 $00 $00 $00 $AA $AA $AA $AA
	.by $D5 $D5 $D5 $D5 $D5 $D5 $D5 $D5
	.by $2A $2A $2A $2A $2A $2A $2A $2A
	.by $55 $55 $55 $55 $55 $55 $55 $55
	.by $AA $D5 $AA $D5 $AA $D5 $AA $D5
	.by $55 $00 $55 $00 $55 $00 $55 $00
	.by $00 $00 $00 $00 $00 $00 $00 $00
	.by $00 $00 $00 $00 $00 $00 $00 $00
screen	
	.by $00 $20 		; list of baselines, each line will be the next 8 lines
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
screen2 
	.by $00 $40
	.by $80 $40
	.by $00 $41
	.by $80 $41
	.by $00 $42
	.by $80 $42
	.by $00 $43
	.by $80 $43
	.by $28 $40
	.by $A8 $40
	.by $28 $41
	.by $A8 $41
	.by $A8 $42
	.by $28 $42
	.by $A8 $43
	.by $28 $43
	.by $50 $40
	.by $D0 $40
	.by $50 $41
	.by $D0 $41
	.by $50 $42
	.by $D0 $42
	.by $50 $43
	.by $D0 $43
fineLine   .by $00

sprite1_x .by 00
mask      .by 00
tankOld   .by $00 $17 $00
tank      .by $00 $17 $67
tankOld1  .by $01 $17 $00
tank1     .by $01 $17 $66
sptCh     .by $00 
sptX      .by $00
sptY      .by $00 
	.endp
	