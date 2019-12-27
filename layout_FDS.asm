FDS_num_regs = #$0E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FDS_reg_bits:
	  .db "..SSSSSS";4040 Wavetable RAM (first)
      .db "MDVVVVVV";4080 Volume Envelope
      .db "FFFFFFFF";4082 Frequency low
      .db "ME..FFFF";4083 Frequency high 
     
	  .db "MDSSSSSS";4084 Mod envelope      
      .db ".BBBBBBB";4085 Mod counter 
      .db "FFFFFFFF";4086 Mod frequency low 
      .db "E...FFFF";4087 Mod frequency high
      
      .db ".....MMM";4088 Mod table write
      .db "W.....VV";4089 Wave write / master volume 
      .db "SSSSSSSS";408A Envelope speed
  
      .db "..VVVVVV";4090 Volume gain 
      .db "..VVVVVV";4092 Mod gain	  
      .db "76543210";WAVE Choice    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
FDS_regs:
	.dw	 $4040,$4080,$4082,$4083
	.dw	 $4084,$4085,$4086,$4087 
	.dw	 $4088,$4089,$408A      
	.dw  $4090,$4092,FDS_WAVE    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
FDS_reg_user_defaults: ; fds bios defaults actually
     .db $00,$80,$00,$00    
     .db $00,$00,$00,$00    
     .db $00,$00,$E8        
     .db $00,$00,$00         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;high/low bytes of start offset of bit 7 of each of the registers on screen
FDS_regHtbl:
    .db $2C,$2D,$2D,$2D
	.db $2C,$2D,$2D,$2D
	.db $2D,$2D,$2E    
	.db $2D,$2D,$2E      	
FDS_regLtbl:
    .db $E8,$08,$28,$48
	.db $F8,$18,$38,$58  
	.db $C8,$E8,$08   
	.db $D8,$F8,$18   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FDS_ch0:	.db "                                "     ;$2C00    
			.db "                                "     ;$2C20        
			.db "  "
			.db $1,$2,$3,$4,$5,$6,$7,$8,$9,$A ; logorow0
			.db "                    "     ;$2C40        
			.db "  "
			.db $11,$12,$13,$14,$15,$16,$17,$18,$19,$1A; logorow1 
			.db "livenes FDS  06/2016"     ;$2C60      
			.db "                                "     ;$2C80            
			.db "        Car             Mod     "     ;$2CA0    
			.db "                                "     ;$2CC0            
			.db "   4040            4084         ",0   ;$2CE0          
FDS_ch1:	.db "   4080            4085         "     ;$2D00    
			.db "   4082            4086         "     ;$2D20
			.db "   4083            4087         "     ;$2D40
			.db "                                "     ;$2D60     
			.db "        Misc            Gain    "     ;$2D80      
			.db "                                "     ;$2DA0
			.db "   4088            4090         "     ;$2DC0
			.db "   4089            4092         ",0   ;$2DE0        
FDS_ch2:	.db "   408A            WAVE         "     ;$2E00
			.db "                                "     ;$2E20      
			.db "                                "     ;$2E40        
			.db "                                "     ;$2E60      
			.db "                                "     ;$2E80
			.db "                                "     ;$2EA0
			.db "                                "     ;$2EC0
			.db "                                ",0   ;$2EE0
FDS_ch3:	.db "                                "     ;$2F00    
			.db "                                "     ;$2F20        
			.db "                                "     ;$2F40         
			.db "                                "     ;$2F60    
			.db "                                "     ;$2F80    
			.db "                                ",0   ;$2FA0      
;DO NOT THRASH ATTRIBUTES TABLE @ 2FC0!!		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FDS_row_to_reg_col0: ; col=0 row=X
    .db $00,$01,$02,$03     
	.db $08,$09,$0A         
	.db 0,0,0,0,0,0,0,0,0   ; (dummys)	 
 
FDS_row_to_reg_col1: ; col=1 row=X
    .db $04,$05,$06,$07     
	.db $0B,$0C,$0D        
	.db 0,0,0,0,0,0,0,0,0   ; (dummys)	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FDS_maxy_col0 = #$07
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_rowtbl0:    ; sprite y positions
    .db $38,$40,$48,$50,$70,$78,$80,$A0
	.db $A8,$B0,$B8,$C0,$C8,$D0,$D8,$E0	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FDS_maxy_col1 = #$07
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_rowtbl1:    ; sprite y positions
    .db $38,$40,$48,$50,$70,$78,$80,$A0
	.db $A8,$B0,$B8,$C0,$C8,$D0,$D8,$E0			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_ch_print0:
    ;name table 0 is $2000-$23FF
    lda #$2C
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
	ldx #$FF
    ldy #0
ALTERNATE_ch_print0_loop:
    lda FDS_ch0,y
    sta $2007 ;PPUDATA
    iny
    dex
    bne ALTERNATE_ch_print0_loop
    rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_ch_print1:
    lda #$2D
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
	ldx #$FF
    ldy #0
ALTERNATE_ch_print1_loop:
    lda FDS_ch1,y
    sta $2007 ;PPUDATA
    iny
    dex
    bne ALTERNATE_ch_print1_loop
    rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_ch_print2:	
    lda #$2E
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
	ldx #$FF
    ldy #0
ALTERNATE_ch_print2_loop:
    lda FDS_ch2,y
    sta $2007 ;PPUDATA
    iny
    dex
    bne ALTERNATE_ch_print2_loop
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_ch_print3:
    lda #$2F
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
	ldx #$C0
    ldy #0
ALTERNATE_ch_print3_loop:
    lda FDS_ch3,y
    sta $2007 ;PPUDATA
    iny
    dex
    bne ALTERNATE_ch_print3_loop
    rts 	 	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_set_cur:
	lda #0 
	sta cursorY
	
	lda #1   ; yes ALTERNATE!
	sta cur_set 
	
	lda #FDS_num_regs	
	sta cur_num_regs	
	
	lda #FDS_maxy_col0	
	sta cur_maxy_col0
	
	lda #FDS_maxy_col1	
	sta cur_maxy_col1	
	
    ; creating pointer adresses for current register set
    lda #FDS_regs&255
    sta ptr_src
    lda #FDS_regs/256
    sta ptr_src+1	
    lda #cur_reg_table&255
    sta ptr_dst
    lda #0; zero page!
    sta ptr_dst+1       
	lda cur_num_regs
	asl ;a=a*2
	tay
    jsr memcpy	
	
	;bunch of table pointers	
    lda #ALTERNATE_shadow
    sta cur_shadow_ptr
    lda #0
    sta cur_shadow_ptr+1	
	
    lda #FDS_reg_bits&255
    sta cur_reg_bits_ptr
    lda #FDS_reg_bits/256
    sta cur_reg_bits_ptr+1	
	
    lda #FDS_regHtbl&255
    sta cur_regHtbl
    lda #FDS_regHtbl/256
    sta cur_regHtbl+1	

    lda #FDS_regLtbl&255
    sta cur_regLtbl
    lda #FDS_regLtbl/256
    sta cur_regLtbl+1	

    lda #FDS_row_to_reg_col0&255
    sta cur_row_to_reg_col0
    lda #FDS_row_to_reg_col0/256
    sta cur_row_to_reg_col0+1	
	
	lda #FDS_row_to_reg_col1&255
    sta cur_row_to_reg_col1
    lda #FDS_row_to_reg_col1/256
    sta cur_row_to_reg_col1+1	

	rts		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_fill_shadow_with_defaults:
	;ONLY DONE ONCE
    lda #FDS_reg_user_defaults&255
    sta ptr_src
    lda #FDS_reg_user_defaults/256
    sta ptr_src+1
    ;destination
    lda #ALTERNATE_shadow&255
    sta ptr_dst
    lda #ALTERNATE_shadow/256
    sta ptr_dst+1    
    ldy #FDS_num_regs 
    jsr memcpy	
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_extra_init:
	jsr fds_saw_load
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_bank_init:
	pha
	
	lda	#$C0
	sta	$0100
	
	;;To use your own IRQ routine, you must manually write $c0 to $101
	lda	#$80
	sta	$0101
	
	;$102 is set to $35 so that the ($DFFC) vector is used and the BIOS is skipped.
	lda	#$35
	sta	$0102 
	
	;$103 indicate reset type : 
	;$AC = first boot of the game, 
	;$53 = the game was soft-reseted by the user
	lda	#$53
	sta	$0103
	
	pla
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
fds_metroid:
	.db $00,$08,$0E,$13,$17,$1B,$20,$24,$27,$29,$2B,$27,$22,$1E,$1B,$18
	.db $1D,$24,$2C,$30,$31,$30,$2D,$2B,$29,$28,$27,$26,$25,$24,$23,$22
	.db $21,$20,$19,$11,$0B,$12,$19,$12,$23,$20,$2F,$34,$37,$3A,$3E,$3F
	.db $3E,$3A,$34,$30,$2D,$27,$23,$1E,$1B,$16,$13,$0F,$0B,$08,$04,$00
fds_metroid_load:
	;backup 4089
	lda $4089
	pha
	ora #$80
	sta $4089
	
    lda #fds_metroid&255
    sta ptr_src
    lda #fds_metroid/256
    sta ptr_src+1
    lda #$40
    sta ptr_dst
    lda #$40
    sta ptr_dst+1    
    ldy #$40
    jsr memcpy	
	
	pla
	sta $4089
	
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
fds_saw:
	.db $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
	.db $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F	
	.db $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F		
	.db $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F	
fds_saw_load:
	;backup 4089
	lda $4089
	pha
	ora #$80
	sta $4089
	
    lda #fds_saw&255
    sta ptr_src
    lda #fds_saw/256
    sta ptr_src+1
    lda #$40
    sta ptr_dst
    lda #$40
    sta ptr_dst+1    
    ldy #$40
    jsr memcpy	
	
	pla
	sta $4089
	
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
fds_sine_u8:
	.db $80,$8D,$99,$A5,$B1,$BC,$C7,$D1,$DB,$E3,$EA,$F1,$F6,$FA,$FE,$FF
	.db $FF,$FF,$FE,$FA,$F6,$F1,$EA,$E3,$DB,$D1,$C7,$BC,$B1,$A5,$99,$8D
	.db $80,$73,$67,$5B,$4F,$44,$39,$2F,$25,$1D,$16,$0F,$0A,$06,$02,$01
	.db $00,$01,$02,$06,$0A,$0F,$16,$1D,$25,$2F,$39,$44,$4F,$5B,$67,$73
