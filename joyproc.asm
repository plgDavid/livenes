;;from blaarg, protects from DMC
INCLUDE read_joy.inc

ScanButtons:
  jsr read_joy_safe  
  sta currdpad ; backup

  ;B7=right,B6=left,B5=down,B4=up,B3=start,B2=select,B1=b,B0=a
  
  and #$1
  beq checkB
  lda prevdpad
  and #$1 
  bne checkB 
  jsr JOYA_NEW_ON
  
checkB:
  lda currdpad
  and #$2
  beq checkSEL
  lda prevdpad
  and #$2
  bne oldB
  jsr JOYB_NEW_ON 
  jmp checkSEL
oldB:
  jsr JOYB_OLD_ON
 
checkSEL:
  lda currdpad
  and #$4
  beq checkSTART
  lda prevdpad
  and #$4
  bne oldSEL
  jsr JOYSEL_NEW_ON 
  jmp checkSTART
oldSEL:
  jsr JOYSEL_OLD_ON  
  
checkSTART:
  lda currdpad
  and #$8
  beq checkUP
  lda prevdpad
  and #$8
  bne oldSTART
  jsr JOYSTART_NEW_ON 
  jmp checkUP
oldSTART:
  jsr JOYSTART_OLD_ON  
 
checkUP:
  lda currdpad
  and #$10
  beq checkDOWN
  lda prevdpad
  and #$10 
  bne oldUP
  jsr JOYUP_NEW_ON 
  jmp checkDOWN
oldUP:
  jsr JOYUP_OLD_ON
  
checkDOWN:
  lda currdpad
  and #$20
  beq checkLEFT
  lda prevdpad
  and #$20 
  bne oldDOWN
  jsr JOYDOWN_NEW_ON 
  jmp checkLEFT 
oldDOWN:  
  jsr JOYDOWN_OLD_ON
  
checkLEFT: 
  lda currdpad
  and #$40
  beq checkRIGHT
  lda prevdpad
  and #$40 
  bne oldLEFT  
  jsr JOYLEFT_NEW_ON 
  jmp checkRIGHT
oldLEFT:
  jsr JOYLEFT_OLD_ON
  
checkRIGHT:   
  lda currdpad
  and #$80
  beq CheckButtonsDone
  lda prevdpad
  and #$80 
  bne oldRIGHT  
  jsr JOYRIGHT_NEW_ON  
  jmp CheckButtonsDone
oldRIGHT:
  jsr JOYRIGHT_OLD_ON
  
CheckButtonsDone:  
  lda currdpad
  sta prevdpad  ;backup
  rts
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;joystick handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JOYA_NEW_ON:
	ldy cur_reg
	lda (cur_shadow_ptr),y
	and selected_bit
	beq BITWASOFF
	;remove bit
	lda (cur_shadow_ptr),y
	eor selected_bit
	sta (cur_shadow_ptr),y

	sty write_pair_reg
	sta write_pair_val	
	jsr write_pair

	jmp JOYA_DRAW
BITWASOFF:
    ;add bit
	lda (cur_shadow_ptr),y
	ora selected_bit
	sta (cur_shadow_ptr),y
	sty write_pair_reg
	sta write_pair_val	
	jsr write_pair
JOYA_DRAW:	
	sty ramdrawreg_arg0
    jsr ramdrawreg;
JOYA_ON_OLD:	
    rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JOYB_NEW_ON:  ; just write current value again (no change)
	ldy cur_reg
	sty write_pair_reg
	lda (cur_shadow_ptr),y
	sta write_pair_val	
	jsr write_pair
JOYB_OLD_ON:
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JOYSEL_NEW_ON:
	jsr swap_register_set
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JOYSEL_OLD_ON:
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JOYSTART_NEW_ON:
	jsr reset
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JOYSTART_OLD_ON:
	rts 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
decrementY:
	lda cursorY
	beq decrementYnegative		
	dec cursorY
	jmp decrementYDone			
decrementYnegative
    lda #$F
	sta cursorY
decrementYDone:	
    rts 

JOYUP_NEW_ON:	
	lda #0
	sta posaccum	
	jsr decrementY
	jmp JOYUP_DONE	
JOYUP_OLD_ON:
    inc posaccum  
    lda posaccum
	and #$8
	beq JOYUP_DONE
	lda #0
	sta posaccum
	jsr decrementY 
JOYUP_DONE:
   rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
decrementX:
	lda cursorX
	beq decrementXnegative		
	dec cursorX
	jmp decrementXDone			
decrementXnegative
    lda #$F
	sta cursorX	
decrementXDone:	
    rts 
	
JOYLEFT_NEW_ON:	
	lda #0
	sta posaccum	
	jsr decrementX
	jmp JOYLEFT_DONE		
JOYLEFT_OLD_ON:		
    inc posaccum  
    lda posaccum
	and #$8
	beq JOYLEFT_DONE
	lda #0
	sta posaccum
	jsr decrementX
JOYLEFT_DONE:
   rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
incrementY:
	inc cursorY 
	lda cursorY		
	cmp selectedCOLH
    bpl clampYNOW
	jmp incrementYend
clampYNOW:
    lda #$0	
	sta cursorY	
incrementYend:	
    rts

JOYDOWN_NEW_ON:
	lda #0
	sta posaccum
	jsr incrementY
	jmp JOYDOWN_DONE	
JOYDOWN_OLD_ON:
    inc posaccum  
    lda posaccum
	and #$8
	beq JOYDOWN_DONE
	lda #0
	sta posaccum
	jsr incrementY
JOYDOWN_DONE:
   rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
incrementX:
	inc cursorX 
	lda cursorX
	and #$F   ; quantise
	sta cursorX	
    rts

JOYRIGHT_NEW_ON:
	lda #0
	sta posaccum
    jsr incrementX
	jmp JOYRIGHT_DONE		
JOYRIGHT_OLD_ON:	
    inc posaccum  
    lda posaccum
	and #$8
	beq JOYRIGHT_DONE
	lda #0
	sta posaccum
	jsr incrementX
JOYRIGHT_DONE:
   rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
coltbl:    ; sprite x positions
    .db $40,$48,$50,$58,$60,$68,$70,$78
	.db $C0,$C8,$D0,$D8,$E0,$E8,$F0,$F8
colbit:    ; highlighed bit value (for potential ORing)
    .db $80,$40,$20,$10,$08,$04,$02,$01
	.db $80,$40,$20,$10,$08,$04,$02,$01
		
update_cursor_sprite:
    lda cursorX
	;translate col to actual spot -> table
	tax ; keep index in X
	lsr
	lsr
	lsr
	sta selectedCOL  ;[0:1]		
	lda coltbl,x
	sta spriteX      ; sprite X position on screen
	lda colbit,x
	sta selected_bit  ; value of the current bit (0x80,0x40,...,0x01)
	
	;translate ROW to actual spot -> table
	lda selectedCOL
	beq regFromCOL0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
regFromCOL1:
	lda cur_maxy_col1
	clc	
    adc #1
	sta selectedCOLH	
	lda cursorY
	cmp cur_maxy_col1 ; last Y index in this column  
    bpl clampY1
	ldx cursorY
    jmp noclampY1
clampY1:
    ldx cur_maxy_col1; last Y index in this column  
	stx cursorY
noclampY1:	
	txa
	tay
	lda cur_reg
	sta prev_reg	
	lda (cur_row_to_reg_col1),y
	sta cur_reg		
    lda cur_set
	bne rowtbl1_store_alternate
rowtbl1_store_main:
	lda RP2A03_rowtbl1,x
	sta spriteY	
    rts
rowtbl1_store_alternate:
	lda ALTERNATE_rowtbl1,x
	sta spriteY	
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
regFromCOL0
	lda cur_maxy_col0
	clc	
    adc #1
	sta selectedCOLH
	lda cursorY
	cmp cur_maxy_col0  ; last Y index in this column  
    bpl clampY0
	ldx cursorY
    jmp noclampY0
clampY0:
    ldx cur_maxy_col0; last Y index in this column  
	stx cursorY
noclampY0:	
	txa
	tay	
	lda cur_reg
	sta prev_reg
	lda (cur_row_to_reg_col0),y
	sta cur_reg
    lda cur_set
	bne rowtbl0_store_alternate
rowtbl0_store_main:
	lda RP2A03_rowtbl0,x
	sta spriteY	
    rts
rowtbl0_store_alternate:
	lda ALTERNATE_rowtbl0,x
	sta spriteY	
    rts
   