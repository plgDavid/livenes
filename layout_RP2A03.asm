RP2A03_num_regs = #$14 ; 16 including 4015 (read)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			
RP2A03_reg_bits:
      .db "DDLCVVVV";4000  
      .db "EPPPNSSS";4001  
      .db "TTTTTTTT";4002  
      .db "LLLLLTTT";4003  
      
      .db "DDLCVVVV";4004  
      .db "EPPPNSSS";4005  
      .db "TTTTTTTT";4006  
      .db "LLLLLTTT";4007  
      
      .db "CRRRRRRR";4008  
      .db "TTTTTTTT";400A  
      .db "LLLLLTTT";400B     
	  
      .db "--LCVVVV";400C 
      .db "L---PPPP";400E  
      .db "LLLLL---";400F  
      
      .db "IL--RRRR";4010  
      .db "-DDDDDDD";4011  
      .db "AAAAAAAA";4012  
      .db "LLLLLLLL";4013  
	  
      .db "---DNT21";4015 
      .db "MI------";4017	  
      .db "--------";Dummy
      .db "IF-DNT21";4015 (read)	  	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
RP2A03_regs:
	.dw	 $4000,$4001,$4002,$4003 ; pulse1
	.dw	 $4004,$4005,$4006,$4007 ; pulse2
	.dw	 $4008,$400A,$400B       ; triangle
	.dw  $400C,$400E,$400F       ; noise
	.dw  $4010,$4011,$4012,$4013 ; DMC
	.dw  $4015,$4017,$0000,$4015 ; Control (dummy+4015 (read))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
RP2A03_reg_user_defaults:
IFDEF INIT_2A03
     .db $00,$00,$00,$00 ; pulse1
     .db $00,$00,$00,$00 ; pulse2
     .db $00,$00,$00     ; tri
     .db $00,$00,$00     ; noise
     .db $00,$00,$00,$00 ; DMC
	 .db $00,$00         ; Control	
ELSE
     .db $30,$08,$00,$00 ; pulse1
     .db $30,$08,$00,$00 ; pulse2
     .db $80,$00,$00     ; tri
     .db $30,$00,$00     ; noise
     .db $00,$00,$00,$00 ; DMC
	 .db $0F,$40         ; Control	(was 4017=0 but that doesnt match reset init!!
ENDIF	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
;high/low bytes of start offset of bit 7 of each of the registers on screen
RP2A03_regHtbl:
    .db $20,$21,$21,$21 ; pulse1
	.db $20,$21,$21,$21 ; pulse2
	.db $21,$21,$22     ; tri
	.db $21,$21,$22     ; noise
	.db $22,$22,$22,$22 ; DMC
	.db $22,$22,$22,$22 ; Control 
RP2A03_regLtbl:
    .db $E8,$08,$28,$48 ; pulse1
	.db $F8,$18,$38,$58 ; pulse2
	.db $C8,$E8,$08     ; tri
	.db $D8,$F8,$18     ; noise
	.db $88,$A8,$C8,$E8 ; DMC
	.db $98,$B8,$D8,$F8 ; Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RP2A03_ch0:	.db "                                "     ;$2000    
			.db "                                "     ;$2020        
			.db "  "
			.db $1,$2,$3,$4,$5,$6,$7,$8,$9,$A ; logorow0
			.db "                    "     ;$2040        
			.db "  "
			.db $11,$12,$13,$14,$15,$16,$17,$18,$19,$1A; logorow1 
			.db "livenes 2A03 01/2022"     ;$2060      
			.db "                                "     ;$2080            
			.db "        Pulse1          Pulse2  "     ;$20A0    
			.db "                                "     ;$20C0            
			.db "   4000            4004         ",0   ;$20E0          
RP2A03_ch1:	.db "   4001            4005         "     ;$2100    
			.db "   4002            4006         "     ;$2120
			.db "   4003            4007         "     ;$2140
			.db "                                "     ;$2160     
			.db "        Triangle        Noise   "     ;$2180        
			.db "                                "     ;$21A0
			.db "   4008            400C         "     ;$21C0
			.db "   400A            400E         ",0   ;$21E0        
RP2A03_ch2:	.db "   400B            400F         "     ;$2200
			.db "                                "     ;$2220      
			.db "        DMC             Control "     ;$2240        
			.db "                                "     ;$2260      
			.db "   4010            4015         "     ;$2280
			.db "   4011            4017         "     ;$22A0
			.db "   4012                         "     ;$22C0
			.db "   4013            "
			.db $94,$90,$91,$95 ; "4015" + $60 which is the highlighted font offset
			.db " IF-DNT21",0   ;$22E0			
RP2A03_ch3:	.db "                                "     ;$2300    
			.db "                                "     ;$2320        
			.db "                                "     ;$2340         
			.db "                                "     ;$2360    
			.db "                                "     ;$2380    
			.db "                                ",0   ;$23A0      
;DO NOT THRASH ATTRIBUTES TABLE @ 23C0!!			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RP2A03_row_to_reg_col0: ; col=0 row=X
    .db $00,$01,$02,$03     ; pulse1
	.db $08,$09,$0A         ; triangle
	.db $0E,$0F,$10,$11     ; DMC
	.db 0,0,0,0,0           ; (dummys)
RP2A03_row_to_reg_col1: ; col=1 row=X
    .db $04,$05,$06,$07     ; pulse2
	.db $0B,$0C,$0D         ; noise
	.db $12,$13	            ; Control
	.db 0,0,0,0,0,0,0       ; (dummys)	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RP2A03_maxy_col0 = #$0A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
RP2A03_rowtbl0:    ; sprite y positions
    .db $38,$40,$48,$50,$70,$78,$80,$A0
	.db $A8,$B0,$B8,$C0,$C8,$D0,$D8,$E0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RP2A03_maxy_col1 = #$08
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
RP2A03_rowtbl1:    ; sprite y positions
    .db $38,$40,$48,$50,$70,$78,$80,$A0
	.db $A8,$B0,$B8,$C0,$C8,$D0,$D8,$E0	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IFDEF UNROM
ALTERNATE_ch_print0:
ENDIF
RP2A03_ch_print0:
    ;name table 0 is $2000-$23FF
    lda #$20
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
    ldy #0
RP2A03_ch_print0_loop:
    lda RP2A03_ch0,y
    sta $2007 ;PPUDATA
    iny
    bne RP2A03_ch_print0_loop
    rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
IFDEF UNROM
ALTERNATE_ch_print1:
ENDIF	
RP2A03_ch_print1:
    lda #$21
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
    ldy #0
RP2A03_ch_print1_loop:
    lda RP2A03_ch1,y
    sta $2007 ;PPUDATA
    iny
    bne RP2A03_ch_print1_loop
    rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
IFDEF UNROM
ALTERNATE_ch_print2:
ENDIF	
RP2A03_ch_print2:
    lda #$22
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR	
    ldy #0
RP2A03_ch_print2_loop:
    lda RP2A03_ch2,y
    sta $2007 ;PPUDATA
    iny
    bne RP2A03_ch_print2_loop
    rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IFDEF UNROM
ALTERNATE_ch_print3:
ENDIF	
RP2A03_ch_print3:  
    lda #$23
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR		
	ldx #$C0
    ldy #0
RP2A03_ch_print3_loop:
    lda RP2A03_ch3,y
    sta $2007 ;PPUDATA
    iny
    dex
    bne RP2A03_ch_print3_loop
    rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IFDEF UNROM
ALTERNATE_set_cur:
ENDIF
RP2A03_set_cur:	
	lda #0 
	sta cursorY
	
	lda #0     ; main set
	sta cur_set 

	lda #RP2A03_num_regs
	sta cur_num_regs
	
	lda #RP2A03_maxy_col0	
	sta cur_maxy_col0
	
	lda #RP2A03_maxy_col1	
	sta cur_maxy_col1
	
    ; creating pointer adresses for current register set
    lda #RP2A03_regs&255
    sta ptr_src
    lda #RP2A03_regs/256
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
    lda #RP2A03_shadow&255
    sta cur_shadow_ptr
    lda #0
    sta cur_shadow_ptr+1	
	
    lda #RP2A03_reg_bits&255
    sta cur_reg_bits_ptr
    lda #RP2A03_reg_bits/256
    sta cur_reg_bits_ptr+1	
	
    lda #RP2A03_regHtbl&255
    sta cur_regHtbl
    lda #RP2A03_regHtbl/256
    sta cur_regHtbl+1	

    lda #RP2A03_regLtbl&255
    sta cur_regLtbl
    lda #RP2A03_regLtbl/256
    sta cur_regLtbl+1	
	
    lda #RP2A03_row_to_reg_col0&255
    sta cur_row_to_reg_col0
    lda #RP2A03_row_to_reg_col0/256
    sta cur_row_to_reg_col0+1	
	
	lda #RP2A03_row_to_reg_col1&255
    sta cur_row_to_reg_col1
    lda #RP2A03_row_to_reg_col1/256
    sta cur_row_to_reg_col1+1	

	rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
IFDEF UNROM	
ALTERNATE_fill_shadow_with_defaults:
	;ONLY DONE ONCE
    lda #RP2A03_reg_user_defaults&255
    sta ptr_src
    lda #RP2A03_reg_user_defaults/256
    sta ptr_src+1
    ;destination
    lda #ALTERNATE_shadow&255
    sta ptr_dst
    lda #ALTERNATE_shadow/256
    sta ptr_dst+1    
    ldy #RP2A03_num_regs 
    jsr memcpy	
	rts		
ENDIF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
RP2A03_fill_shadow_with_defaults:
	;ONLY DONE ONCE
    lda #RP2A03_reg_user_defaults&255
    sta ptr_src
    lda #RP2A03_reg_user_defaults/256
    sta ptr_src+1
    ;destination
    lda #RP2A03_shadow&255
    sta ptr_dst
    lda #RP2A03_shadow/256
    sta ptr_dst+1    
    ldy #RP2A03_num_regs 
    jsr memcpy	
	rts	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
IFDEF UNROM
ALTERNATE_extra_init:
	rts
ALTERNATE_bank_init:
	rts	
ALTERNATE_rowtbl0:    ; sprite y positions
    .db $38,$40,$48,$50,$70,$78,$80,$A0
	.db $A8,$B0,$B8,$C0,$C8,$D0,$D8,$E0	
ALTERNATE_rowtbl1:    ; sprite y positions
    .db $38,$40,$48,$50,$70,$78,$80,$A0
	.db $A8,$B0,$B8,$C0,$C8,$D0,$D8,$E0	
ALTERNATE_max_y_per_row:
	.db $9,$B	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
ALTERNATE_write:
	jsr write_2a03 
	rts
ENDIF	
