VRC6_num_regs  = #$0D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VRC6_reg_bits:
      .db "MDDDVVVV";9000  
      .db "FFFFFFFF";9001  
      .db "E...FFFF";9002  
      .db ".....ABH";9003  
      
      .db "MDDDVVVV";A000  
      .db "FFFFFFFF";A001  
      .db "E...FFFF";A002  
      
      .db "..AAAAAA";B000  
      .db "FFFFFFFF";B001        
      .db "E...FFFF";B002   
  
      .db "LLLLLLLL";F000  
      .db ".....MEA";F001        
      .db "........";F002   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
VRC6_regs:
    .dw  $9000,$9001,$9002,$9003 ; pulse1
    .dw  $A000,$A001,$A002       ; pulse2
    .dw  $B000,$B001,$B002       ; SAW
    .dw  $F000,$F001,$F002       ; IRQ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
VRC6_reg_user_defaults:
     .db $00,$00,$00,$00 ; pulse1
     .db $00,$00,$00     ; pulse2
     .db $00,$00,$00     ; SAW
     .db $00,$00,$00     ; IRQ     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;high/low bytes of start offset of bit 7 of each of the registers on screen
VRC6_regHtbl:
    .db $2C,$2D,$2D,$2D ; pulse1
    .db $2C,$2D,$2D     ; pulse2
    .db $2D,$2D,$2E     ; saw
    .db $2D,$2D,$2E     ; irq2    
VRC6_regLtbl:
    .db $E8,$08,$28,$48 ; pulse1
    .db $F8,$18,$38     ; pulse2
    .db $C8,$E8,$08     ; saw
    .db $D8,$F8,$18     ; irq2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VRC6_ch0:   .db "                                "     ;$2C00    
            .db "                                "     ;$2C20        
            .db "  "
            .db $1,$2,$3,$4,$5,$6,$7,$8,$9,$A ; logorow0
            .db "                    "     ;$2C40        
            .db "  "
            .db $11,$12,$13,$14,$15,$16,$17,$18,$19,$1A; logorow1 
            .db "livenes VRC6 10/2016"     ;$2C60      
            .db "                                "     ;$2C80            
            .db "        Pulse1          Pulse2  "     ;$2CA0    
            .db "                                "     ;$2CC0            
            .db "   9000            A000         ",0   ;$2CE0          
VRC6_ch1:   .db "   9001            A001         "     ;$2D00    
            .db "   9002            A002         "     ;$2D20
            .db "   9003                         "     ;$2D40
            .db "                                "     ;$2D60     
            .db "        Saw             IRQ     "     ;$2D80      
            .db "                                "     ;$2DA0
            .db "   B000            F000         "     ;$2DC0
            .db "   B001            F001         ",0   ;$2DE0        
VRC6_ch2:   .db "   B002            F002         "     ;$2E00
            .db "                                "     ;$2E20      
            .db "                                "     ;$2E40        
            .db "                                "     ;$2E60      
            .db "                                "     ;$2E80
            .db "                                "     ;$2EA0
            .db "                                "     ;$2EC0
            .db "                                ",0   ;$2EE0
VRC6_ch3:   .db "                                "     ;$2F00    
            .db "                                "     ;$2F20        
            .db "                                "     ;$2F40         
            .db "                                "     ;$2F60    
            .db "                                "     ;$2F80    
            .db "                                ",0   ;$2FA0      
;DO NOT THRASH ATTRIBUTES TABLE @ 2FC0!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VRC6_row_to_reg_col0: ; col=0 row=X
    .db $00,$01,$02,$03     ; PULSE1
    .db $07,$08,$09         ; SAW
    .db 0,0,0,0,0,0,0,0,0   ; (dummys)
        
VRC6_row_to_reg_col1: ; col=1 row=X
    .db $04,$05,$06         ; PULSE1
    .db $0A,$0B,$0C         ; IRQ
    .db 0,0,0,0,0,0,0,0,0,0 ; (dummys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VRC6_maxy_col0 = #$06
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_rowtbl0:    ; sprite y positions
    .db $38,$40,$48,$50,$70,$78,$80
    .db 0,0,0,0,0,0,0,0,0,0,0 ;(dummys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VRC6_maxy_col1 = #$05    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_rowtbl1:    ; sprite y positions
    .db $38,$40,$48,$70,$78,$80
    .db 0,0,0,0,0,0,0,0,0,0 ;(dummys)    
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
    lda VRC6_ch0,y
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
    lda VRC6_ch1,y
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
    lda VRC6_ch2,y
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
    lda VRC6_ch3,y
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
    
    lda #VRC6_num_regs    
    sta cur_num_regs    
    
    lda #VRC6_maxy_col0    
    sta cur_maxy_col0
    
    lda #VRC6_maxy_col1    
    sta cur_maxy_col1    
    
    ; creating pointer adresses for current register set
    lda #VRC6_regs&255
    sta ptr_src
    lda #VRC6_regs/256
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
    
    lda #VRC6_reg_bits&255
    sta cur_reg_bits_ptr
    lda #VRC6_reg_bits/256
    sta cur_reg_bits_ptr+1    
    
    lda #VRC6_regHtbl&255
    sta cur_regHtbl
    lda #VRC6_regHtbl/256
    sta cur_regHtbl+1    

    lda #VRC6_regLtbl&255
    sta cur_regLtbl
    lda #VRC6_regLtbl/256
    sta cur_regLtbl+1    

    lda #VRC6_row_to_reg_col0&255
    sta cur_row_to_reg_col0
    lda #VRC6_row_to_reg_col0/256
    sta cur_row_to_reg_col0+1    
    
    lda #VRC6_row_to_reg_col1&255
    sta cur_row_to_reg_col1
    lda #VRC6_row_to_reg_col1/256
    sta cur_row_to_reg_col1+1    

    rts        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
ALTERNATE_fill_shadow_with_defaults:
    ;ONLY DONE ONCE
    lda #VRC6_reg_user_defaults&255
    sta ptr_src
    lda #VRC6_reg_user_defaults/256
    sta ptr_src+1
    ;destination
    lda #ALTERNATE_shadow&255
    sta ptr_dst
    lda #ALTERNATE_shadow/256
    sta ptr_dst+1    
    ldy #VRC6_num_regs 
    jsr memcpy    
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
ALTERNATE_extra_init:
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
ALTERNATE_bank_init:

    ;VRC6 $9003 audio enable register!
    ;http://forums.nesdev.com/viewtopic.php?f=3&t=9207
    ;http://www.famitracker.com/forum/posts.php?id=3809    
    lda  #$00
    sta  $9003    
        
;We need to set proper CHR windows! VRC6 deals with the CHR in 8 sub-windows of 1KB
;we only need the first 4 but they need to point to the right stuff!
    lda #$20 ;; https://wiki.nesdev.com/w/index.php/VRC6#PPU_Banking_Style_.28.24B003.29
    sta $B003  
    
    lda #$0
    sta $D000;CHR $0000-$03FF: 1 KB switchable CHR ROM bank
    lda #$1
    sta $D001;CHR $0400-$07FF: 1 KB switchable CHR ROM bank
    lda #$2
    sta $D002;CHR $0800-$0BFF: 1 KB switchable CHR ROM bank
    lda #$3
    sta $D003;CHR $0C00-$0FFF: 1 KB switchable CHR ROM bank    

    rts
    
