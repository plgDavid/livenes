S5B_num_regs  = #$10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
S5B_reg_bits:
      .db "ffffffff";0
      .db "....FFFF";1
      .db "ffffffff";2
      .db "....FFFF";3
      .db "ffffffff";4
      .db "....FFFF";5
      .db "...NNNNN";6
      
	  .db "BA321321";7
      .db "...M3210";8
      .db "...M3210";9
      .db "...M3210";A
      .db "eeeeeeee";B
      .db "EEEEEEEE";C
      .db "....CATH";D
	  
 	  .db "AAAAAAAA";E
      .db "BBBBBBBB";F
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
S5B_regs:; not really
	.dw	 $6000,$6001,$6002,$6003,$6004,$6005,$6006; 
	.dw	 $6007,$6008,$6009,$600A,$600B,$600C,$600D; 
	.dw	 $600E,$600F
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
S5B_reg_user_defaults:
     .db $00,$00,$00,$00,$00,$00,$00;
     .db $00,$00,$00,$00,$00,$00,$00;
     .db $00,$00
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;high/low bytes of start offset of bit 7 of each of the registers on screen
S5B_regHtbl:
    .db $2C,$2D,$2D,$2D,$2D,$2D,$2D ; 
	.db $2C,$2D,$2D,$2D,$2D,$2D,$2D ; 
	.db $2E,$2E,$2E,$2E,$2E,$2E,$2E ;
	.db $2E,$2E,$2E,$2E             ; 
	
S5B_regLtbl:
    .db $E8,$08,$28,$48,$68,$88,$A8 ;
	.db $F8,$18,$38,$58,$78,$98,$B8 ; 
	.db $28,$48,$68,$88,$A8,$C8,$E8 ;
	.db $38,$58,$78,$98             ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
S5B_ch0: .db "                                "     ;$2C00    
			.db "                                "     ;$2C20        
			.db "  "
			.db $1,$2,$3,$4,$5,$6,$7,$8,$9,$A ; logorow0
			.db "                    "     ;$2C40        
			.db "  "
			.db $11,$12,$13,$14,$15,$16,$17,$18,$19,$1A; logorow1 
			.db "livenes S5B  07/2021"     ;$2C60      
			.db "                                "     ;$2C80            
			.db "        Freq            Ctrl    "     ;$2CA0    
			.db "                                "     ;$2CC0            
			.db "   REG0            REG7         ",0   ;$2CE0          
S5B_ch1:.db     "   REG1            REG8         "     ;$2D00    
			.db "   REG2            REG9         "     ;$2D20
			.db "   REG3            REGA         "     ;$2D40
			.db "   REG4            REGB         "     ;$2D60     
			.db "   REG5            REGC         "     ;$2D80      
			.db "   REG6            REGD         "     ;$2DA0
			.db "                                "     ;$2DC0
			.db "        Ports                   ",0   ;$2DE0        
S5B_ch2:.db     "                                "     ;$2E00
			.db "   REGE                         "     ;$2E20      
			.db "   REGF                         "     ;$2E40        
			.db "                                "     ;$2E60      
			.db "                                "     ;$2E80
			.db "                                "     ;$2EA0
			.db "                                "     ;$2EC0
			.db "                                ",0   ;$2EE0
S5B_ch3:.db     "                                "     ;$2F00    
			.db "                                "     ;$2F20        
			.db "                                "     ;$2F40         
			.db "                                "     ;$2F60    
			.db "                                "     ;$2F80    
			.db "                                ",0   ;$2FA0      
;DO NOT THRASH ATTRIBUTES TABLE @ 2FC0!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
S5B_row_to_reg_col0: ; col=0 row=X
    .db $00,$01,$02,$03,$04,$05,$06     ; Freq
    .db $0E,$0F,0,0,0,0,0,0,0           ; Ports
S5B_row_to_reg_col1: ; col=1 row=X
    .db $07,$08,$09,$0A,$0B,$0C,$0D     ; Ctrl    
	.db 0,0,0,0,0,0,0,0,0               ;(dummys)		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
S5B_maxy_col0 = #$08
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_rowtbl0:    ; sprite y positions
    .db $38,$40,$48,$50,$58,$60,$68
    .db $88,$90,$98,$A0,$A8,$B0,$B8
	.db 0,0 ;(dummys)	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
S5B_maxy_col1 = #$06
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_rowtbl1:    ; sprite y positions
    .db $38,$40,$48,$50,$58,$60,$68
    .db $88,$90,$98,$A0,$A8,$B0,$B8
	.db 0,0 ;(dummys)
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
    lda S5B_ch0,y
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
    lda S5B_ch1,y
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
    lda S5B_ch2,y
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
    lda S5B_ch3,y
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

	lda #1  ; yes ALTERNATE!
	sta cur_set 
	
	lda #S5B_num_regs	
	sta cur_num_regs	
	
	lda #S5B_maxy_col0	
	sta cur_maxy_col0
	
	lda #S5B_maxy_col1	
	sta cur_maxy_col1
	
    ; creating pointer addresses for current register set
    lda #S5B_regs&255
    sta ptr_src
    lda #S5B_regs/256
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
	
    lda #S5B_reg_bits&255
    sta cur_reg_bits_ptr
    lda #S5B_reg_bits/256
    sta cur_reg_bits_ptr+1	
	
    lda #S5B_regHtbl&255
    sta cur_regHtbl
    lda #S5B_regHtbl/256
    sta cur_regHtbl+1	

    lda #S5B_regLtbl&255
    sta cur_regLtbl
    lda #S5B_regLtbl/256
    sta cur_regLtbl+1	

    lda #S5B_row_to_reg_col0&255
    sta cur_row_to_reg_col0
    lda #S5B_row_to_reg_col0/256
    sta cur_row_to_reg_col0+1	
	
	lda #S5B_row_to_reg_col1&255
    sta cur_row_to_reg_col1
    lda #S5B_row_to_reg_col1/256
    sta cur_row_to_reg_col1+1	

	rts		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_fill_shadow_with_defaults:
	;ONLY DONE ONCE
    lda #S5B_reg_user_defaults&255
    sta ptr_src
    lda #S5B_reg_user_defaults/256
    sta ptr_src+1
    ;destination
    lda #ALTERNATE_shadow&255
    sta ptr_dst
    lda #ALTERNATE_shadow/256
    sta ptr_dst+1    
    ldy #S5B_num_regs 
    jsr memcpy	
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_extra_init:
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_bank_init:
	;filling up CHR as if it didn't have a mapper
	lda #0
	sta $8000
	sta $A000
	lda #1
	sta $8000
	sta $A000
	lda #2
	sta $8000
	sta $A000
	lda #3
	sta $8000
	sta $A000
	lda #4
	sta $8000
	sta $A000
	lda #5
	sta $8000
	sta $A000
	lda #6
	sta $8000
	sta $A000
	lda #7
	sta $8000
	sta $A000
	rts		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_write:
	lda write_pair_reg
	sta $C000
	lda write_pair_val
	sta $E000	
	rts
	