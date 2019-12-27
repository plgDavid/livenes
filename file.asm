INCLUDE vars.inc

IFDEF UNROM ; mapper 002 note CHRAM MUST also be defined!
.org $BFF0 ; urgh $10 bytes before
.db "NES",$1A
.db $01   ;Size of PRG ROM in 16 KB units
.db $00   ;Size of CHR ROM in 8 KB units (Value 0 means the board uses CHR RAM)
.db $20   ;Flags 6 (UNROM)
.db $00   ;Flags 7 (UNROM)
.db $00   ;Size of PRG RAM in 8 KB units (Value 0 infers 8 KB for compatibility)
.db $00   ;Flags 9
.db $00   ;Flags 10 (unofficial)
.db 0,0,0,0,0
ENDIF

IFDEF VRC6 ;Note this is mapper 024 (C3)
.org $BFF0 ; urgh $10 bytes before
.db "NES",$1A
.db $01   ;Size of PRG ROM in 16 KB units
.db $01   ;Size of CHR ROM in 8 KB units (Value 0 means the board uses CHR RAM)
.db $80   ;Flags 6 (VRC6 mapper 24 or $18)
.db $10   ;Flags 7 (VRC6 mapper 24 or $18)
.db $00   ;Size of PRG RAM in 8 KB units (Value 0 infers 8 KB for compatibility)
.db $00   ;Flags 9
.db $00   ;Flags 10 (unofficial)
.db 0,0,0,0,0
.org $C000
nop
.org $E010
;When the cart is first started, the first 16K ROM bank in the cart is loaded into $8000, and the LAST 16K ROM bank is loaded into
;$C000. The last 8K of ROM is permanently "hard-wired" and cannot be swapped.
;VROM should NOT be swapped into PPU $0000 when the cartridge is started or reset, in order to avoid graphics corruption.
INCLUDE layout_VRC6.asm
ENDIF

IFDEF FDS ; mapper "22" note CHRAM MUST also be defined!
;.org $BFF0 ; urgh $10 bytes before
;.db "FDS",$1A
;.db $01  ; number of sides
;.db 0,0,0,0,0,0,0,0,0,0,0        
.org $C000
INCLUDE layout_FDS.asm
ENDIF

;Always present whatever the mapper
INCLUDE layout_RP2A03.asm

;same atributes for ALL screens on all mappers
attributes:
    .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101  ;$23C0    
    .db %11111111, %11111111, %00000000, %00000000, %11111111, %11111111, %00000000, %00000000  ;$23C8    
    .db %11111111, %11111111, %00000000, %00000000, %11111111, %11111111, %00000000, %00000000  ;$23D0    
    .db %11111111, %11111111, %00000000, %00000000, %11111111, %11111111, %00000000, %00000000  ;$23D8
    .db %11111111, %11111111, %00000000, %00000000, %11111111, %11111111, %00000000, %00000000  ;$23E0
    .db %11111111, %11111111, %00000000, %00000000, %11111111, %11111111, %00000000, %00000000  ;$23E8
    .db %11111111, %11111111, %00000000, %00000000, %11111111, %11111111, %00000000, %00000000  ;$23F0
    .db %11111111, %11111111, %00000000, %00000000, %11111111, %11111111, %00000000, %00000000  ;$23F8

;same pallete for all screens on all mappers
palette:
    .db $0F,$2d,$30,$10,  $0F,$30,$11,$31,  $0A,$1A,$2A,$3A,  $07,$11,$27,$37
    .db $0F,$1C,$00,$14,  $31,$02,$10,$3C,  $0F,$1C,$20,$14,  $31,$02,$10,$3C
    
LoadPalettes:
  LDA $2002 ;PPUSTATUS read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006 ;PPUADDR   write the high byte of $3F00 address
  LDA #$00
  STA $2006 ;PPUADDR   write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                        ; 1st time through loop it will load palette+0
                        ; 2nd time through loop it will load palette+1
                        ; 3rd time through loop it will load palette+2
                        ; etc
  STA $2007 ;PPUDATA             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  rts
  
 
;;;;;this routine should be swapped in and out depending on what we want
INCLUDE joyproc.asm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
write_vram_until_zero:
    ldy #0
write_vram_until_zero_loop:
    lda (ptr_src),y
    beq write_vram_until_zero_quit
    sta $2007 ;PPUDATA
    iny
    bne write_vram_until_zero_loop
write_vram_until_zero_quit:
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
;writes x amount of bytes to vram
write_vram_x:
    ldy #0
write_vram_x_loop:
    lda (ptr_src),y
    sta $2007 ;PPUDATA
    iny
    dex
    bne write_vram_x_loop
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
;indirect memory->memory copy    
;ptr_src: zero page indirect source
;ptr_dst: zero page indirect destination
;y   : number of BYTES to copy
memcpy:
    dey
memcpy_loop:  
    lda (ptr_src),y
    sta (ptr_dst),y
    dey
    bpl memcpy_loop
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
;indirect memory->memory copy    
;ptr_src: zero page indirect source
;ptr_dst: zero page indirect destination
;x   : number of PAGES of 256 BYTES to copy
memcpyPages:    
    ldy #$00
memcpyLoop:
    lda (ptr_src),Y
    sta (ptr_dst),Y 
    iny
    bne memcpyLoop
    inc ptr_src+1
    inc ptr_dst+1
    dex
    bne memcpyPages
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

IFDEF CHRRAM
clear_vram:
    lda #$00
    sta $2006  ;PPUADDR
    sta $2006  ;PPUADDR
    tay
    ldx #6
clear_vram_loop:
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    dey
    bne clear_vram_loop
    dex
    bne clear_vram_loop
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
load_font:
    lda #font&255
    sta ptr_src
    lda #font/256
    sta ptr_src+1
    ldx #3
    ldy #0
fontchar_loop:
    lda (ptr_src),y
    sta $2007 ;PPUDATA
    iny
    lda (ptr_src),y
    sta $2007 ;PPUDATA
    iny
    lda (ptr_src),y
    sta $2007 ;PPUDATA
    iny
    lda (ptr_src),y
    sta $2007 ;PPUDATA
    iny
    lda (ptr_src),y
    sta $2007 ;PPUDATA
    iny
    lda (ptr_src),y
    sta $2007 ;PPUDATA
    iny
    lda (ptr_src),y
    sta $2007 ;PPUDATA
    iny
    lda (ptr_src),y
    sta $2007 ;PPUDATA

    lda #0
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    sta $2007 ;PPUDATA
    
    iny
    bne fontchar_loop
    inc ptr_src+1
    dex
    bne fontchar_loop
    rts
ENDIF
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
RP2A03_set_attributes:
    lda #$23   ;$23C0     sized $0040 (Attribute Table #0)
    sta $2006  ;PPUADDR
    lda #$C0
    sta $2006  ;PPUADDR
    lda #attributes&255
    sta ptr_src
    lda #attributes/256
    sta ptr_src+1
    ldx #$40
    jsr write_vram_x
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
ALTERNATE_set_attributes:
    lda #$2F   ;$2FC0     sized $0040 (Attribute Table #0)
    sta $2006  ;PPUADDR
    lda #$C0
    sta $2006  ;PPUADDR
    lda #attributes&255
    sta ptr_src
    lda #attributes/256
    sta ptr_src+1
    ldx #$40
    jsr write_vram_x
    rts


;************************************************************************    
;****************** Main Entry Point!!! *********************************
;************************************************************************    
;http://wiki.nesdev.com/w/index.php/Init_code
reset:
    sei        ; ignore IRQs
    cld        ; disable decimal mode
    ldx #$40
    stx $4017  ; disable APU frame IRQ 
    ldx #$ff
    txs        ; Set up stack
    inx        ; now X = 0  
    stx $2000  ; disable NMI
    stx    PPU2000; cache (also used by FDS)    
    stx $2001  ; disable rendering
    stx    PPU2001; cache (also used by FDS)    
    stx $4010  ; disable DMC IRQs
    
    lda #$0
    sta initdone
    
    ; Set up mapper and jmp to further init code here.
    
IFDEF GALMAPPER 
    ;reset LOW chip through GAL's Q4 (pin18)
    lda #$EF
    sta $8000    
ENDIF

    ; If the user presses Reset during vblank, the PPU may reset
    ; with the vblank flag still true.  This has about a 1 in 13
    ; chance of happening on NTSC or 2 in 9 on PAL.  Clear the
    ; flag now so the @vblankwait1 loop sees an actual vblank.
    bit $2002

    ; First of two waits for vertical blank to make sure that the
    ; PPU has stabilized
@vblankwait1:  
    bit $2002
    bpl @vblankwait1

    ; We now have about 30,000 cycles to burn before the PPU stabilizes.
    ; One thing we can do with this time is put RAM in a known state.
    ; Here we fill it with $00, which matches what (say) a C compiler
    ; expects for BSS.  Conveniently, X is still 0.
    txa
@clrmem:
    sta $000,x
    sta $100,x
    ;sta $200,x     (done below in @clearOAM instead)
    sta $300,x
    sta $400,x
    sta $500,x
    sta $600,x
    sta $700,x  ; Remove this if you're storing reset-persistent data
    ; We skipped $200,x on purpose.  Usually, RAM page 2 is used for the
    ; display list to be copied to OAM.  OAM needs to be initialized to
    ; $EF-$FF, not 0, or you'll get a bunch of garbage sprites at (0, 0).
    inx
    bne @clrmem

    ; clear OAM by hiding all sprites at #$FF
    ldx #0
    lda #$FF
@clearOAM:
    sta $200,x
    inx
    inx
    inx
    inx
    bne @clearOAM
    
    ; Other things you can do between vblank waits are set up audio
    ; or set up other mapper registers.
    
@vblankwait2:
    bit $2002
    bpl @vblankwait2
    
IFDEF GALMAPPER
    ;lift up reset chip through GAL's Q4 (pin18)
    lda #$FF
    sta $8000        
ENDIF
    
    lda #$80
    sta $2000 ;PPUCTRL
    sta cur_nametable
        
    jsr ALTERNATE_bank_init
        
    jsr LoadPalettes 
    
IFDEF CHRRAM        
    jsr clear_vram

    ;load font base
    lda #$02
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
    
    jsr load_font
    
    ;load font offseted (for other bits of color palette trick)
    lda #$08
    sta $2006  ;PPUADDR
    lda #$08
    sta $2006  ;PPUADDR
    jsr load_font
    
    ;Plogue Logo_row0
    lda #$00
    sta $2006  ;PPUADDR
    lda #$10
    sta $2006  ;PPUADDR    
    lda #ploguelogo_row0&255
    sta ptr_src
    lda #ploguelogo_row0/256
    sta ptr_src+1
    ldx #$A0
    jsr write_vram_x

    ;Plogue Logo_row1
    lda #$01
    sta $2006  ;PPUADDR
    lda #$10
    sta $2006  ;PPUADDR    
    lda #ploguelogo_row1&255
    sta ptr_src
    lda #ploguelogo_row1/256
    sta ptr_src+1
    ldx #$A0
    jsr write_vram_x    
ENDIF    
 
    ;RP2A03 audio+graphics init
    jsr RP2A03_fill_shadow_with_defaults   ; only do ONCE        
    jsr RP2A03_ch_print0
    jsr RP2A03_ch_print1
    jsr RP2A03_ch_print2
    jsr RP2A03_ch_print3    
     jsr RP2A03_set_attributes
    jsr RP2A03_set_cur    
    jsr cur_write_all_shadows    
    jsr cur_draw_all_regs     
    jsr ppudraw_all_regs    
    
    ;RP2A03(alt) or Mapper
    jsr ALTERNATE_fill_shadow_with_defaults; only do ONCE    
    jsr ALTERNATE_ch_print0
    jsr ALTERNATE_ch_print1
    jsr ALTERNATE_ch_print2
    jsr ALTERNATE_ch_print3    
     jsr ALTERNATE_set_attributes    
    jsr ALTERNATE_set_cur
    jsr cur_write_all_shadows        
    jsr ALTERNATE_extra_init
    jsr cur_draw_all_regs     
    jsr ppudraw_all_regs    

    ;no emph, start sprites, backgroung and show in borders
    lda #%00011110
    sta $2001
    sta    PPU2001    ;cache (also used by FDS)    
    
    ;scrolling OFF again
    lda #0
    sta $2005
    sta    PPU2005H ;cache (also used by FDS)    
    sta $2005
    sta    PPU2005V ;cache (also used by FDS)    
    
    lda #1
    sta initdone;
    sta cur_set;yes there was a double click bug before
    
main_loop:    
    jsr WaitFrame
    jsr ScanButtons        
    jsr update_cursor_sprite
    jmp main_loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
;; WaitFrame - waits for VBlank, returns after NMI handler is done
WaitFrame:
inc sleeping
@loop:
  lda sleeping
  bne @loop
rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
;trashes a and x
write_pair:
    
    lda write_pair_reg
    asl
    tax
    
    lda write_pair_val
    sta (cur_reg_table,x)
    
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
; this is always done, whether or not we have an extra mapper!
cur_write_all_shadows    
    ldy #0    
shadowloop:  
    lda (cur_shadow_ptr),y
    
    sty write_pair_reg    
    sta write_pair_val
    jsr write_pair
    
    iny
    cpy cur_num_regs
    bne shadowloop

    rts    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
;Draws register in cur_ramdrawreg into a temporary screen location until PPU NMI
;Actually displays it using ppudrawreg trashes A and Y
ramdrawreg:    
    ;putting the current register value in a variable:
    ;temp0 = soundbank0[x]
    ldy ramdrawreg_arg0    
    lda (cur_shadow_ptr),y
    sta tempA
    ; get proper string adress by multiplying x by 8 (using 3 shifts)    
    tya
    asl
    asl
    asl 
    tay; x contains the memory offset to reg_bits
    
    ;for (uint8_t y=0;y<8;y++){
    ldx #$0
ramdrawreg_loop:
    ;if(soundbank0[x]&0x80)
    ;   chartowrite+=0x80; //bright font    
    lda tempA 
    and #$80 ;testing high bit
    beq ramdrawreg_bitnotset;
    lda (cur_reg_bits_ptr),y
    adc #$60 ; adding 60 which is the highlighed font offset
    sta screentemp,y
    jmp ramdrawreg_next
ramdrawreg_bitnotset:    ; copy directly
    lda (cur_reg_bits_ptr),y
    sta screentemp,y
ramdrawreg_next:    
    ;get next bit    
    lda tempA
    asl
    sta tempA

    inx ; x++
    iny ; y++
    cpx #$8
    bne ramdrawreg_loop    
    ldx temp2
    rts    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
;drawing all registers, call this on init or when changing bank
cur_draw_all_regs:
    ldx cur_num_regs
cur_drawallregs_loop:        
    
    stx ramdrawreg_arg0    
    jsr ramdrawreg ; thrashes ALL regs!
    ldx ramdrawreg_arg0
    
    dex
    bne cur_drawallregs_loop
    ;and... the last one (entry 0)        
    stx ramdrawreg_arg0        
    jsr ramdrawreg    ; thrashes ALL regs!
    rts    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
;;needed register MUST be in y
ppudrawreg:
    lda (cur_regHtbl),y
    sta $2006  ;PPUADDR
    lda (cur_regLtbl),y
    sta $2006  ;PPUADDR    

    tya
    asl
    asl
    asl 
    tax; x contains the memory offset to screentemp    

    lda screentemp,x
    sta $2007 ;PPUDATA
    lda screentemp+1,x
    sta $2007 ;PPUDATA
    lda screentemp+2,x
    sta $2007 ;PPUDATA
    lda screentemp+3,x
    sta $2007 ;PPUDATA
    lda screentemp+4,x
    sta $2007 ;PPUDATA
    lda screentemp+5,x
    sta $2007 ;PPUDATA
    lda screentemp+6,x
    sta $2007 ;PPUDATA
    lda screentemp+7,x
    sta $2007 ;PPUDATA
    rts    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ppudraw_all_regs
    ldy cur_num_regs
    dey
ppudraw_all_regs_loop:    
    jsr ppudrawreg  
    dey
    bne ppudraw_all_regs_loop
    jsr ppudrawreg  ;last time    
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
swap_register_set:    
    lda cur_set
    beq do_set_alternate    
    jsr RP2A03_set_cur        
    lda #0
    sta cur_set
    lda #$80
    sta cur_nametable
    jmp swap_end
do_set_alternate:
    jsr ALTERNATE_set_cur        
    lda #$1
    sta cur_set
    lda #$83
    sta cur_nametable    
swap_end:
    jsr cur_draw_all_regs; will be done after whole screen redraw
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;http://wiki.nesdev.com/w/index.php/The_frame_and_NMIs
;The CPU can load graphics data into the PPU only during this rest period. 
;From NMI to the end of the pre-render scanline, the NTSC NES PPU stays off the bus for 2273 cycles
nmihandler:
    php
    pha
    txa
    pha
    tya
    pha    ; backed up registers
  
    bit $2002 ; clear VBl flag, reset $2005/$2006 toggle

    ;jmp nmidone
    lda initdone
    beq notready
 
    lda cur_nametable
    sta $2000
    
    ldy cur_reg
    jsr ppudrawreg 

IFDEF FDS
    ;why again in case MAIN didnt set it in time??
    jsr    ALTERNATE_bank_init
ENDIF
        
    lda    PPU2005H
    sta    $2005
    lda    PPU2005V
    sta    $2005
    
notready:
    
    ;out sprite0 handling ... needs to be timed for correct flashes
    lda spriteY  ; load  Y value
    sta oam      ; store Y value
    lda #$BF     ; define sprite's tile number
    sta oam+1    ; store tile number

    lda spriteColor     ; Attributes
    lsr
    lsr
    lsr 
    lsr
    sta oam+2    ; store palete and stuff
    lda spriteColor     ; Attributes
    adc #1
    and #$3F
    sta spriteColor
        
    lda spriteX  ; load  X value
    sta oam+3    ; store X value    
    
    ;sprite DMAs (for ALL sprites)
    lda #0      ; do sprite DMA
    sta $2003   ; conditional via the 'needdma' flag
    lda #>oam
    sta $4014
      
nmidone:      
    lda #0       ;; clear the sleeping flag so that WaitFrame will exit
    sta sleeping ;; note that you should not 'dec' here, as sleeping might
                 ;; already be zero (will be the case during slowdown)
   
    pla; restore regs and exit
    tay
    pla
    tax
    pla
    plp    
    rti
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
irqhandler:
    rti
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
IFDEF CHRRAM  
INCLUDE bitmaps.asm
ENDIF


IFNDEF FDS    
;include some test DMC's (2016). DPCM samples must:
;Started with $C000 + (A * 64) SO
;1)begin in the memory range $C000-FFFF 
;2)start on $40 bounds (C000,C040,(...),FFB0)
;3)For $E000, fill $4012 with $80,$81, etc
;3)For $F000, fill $4012 with $C0,$C1, etc
;.org $F000
;INCLUDE dmcs.asm

IFDEF GALSTM32
    .org $FF00
    .db "FCSTM32 live   ",0 ; name
    .db $FC,$AA,$EE,$01; fake CRC32
    .db 34 ;https://wiki.nesdev.com/w/index.php/BNROM
    .db 4 ;prg_blocks
    .db 0 ;chr_blocks    
    .db 0 ;flags    
    .db 0,0,0,0,0,0,0,0 ;32bit pointers (arm)
    .db "F051   ",0; just use the datasheet and figure it out mate!                
    .db $4D,$4F,$1B,$00  ; 1.789773MHz (uint32_t) weird xtal on proto    
    .dw $0001 ; channel mask (only right)        
    .dw $5000 ; audio   adress
    .dw $8000 ; banking adress    
    .dw $8000 ; reset adress
    .db $FF   ; reset inactive
    .db $EF   ; reset active    
ENDIF
    
IFDEF GALSID
    .org $FF00
    .db "FCSID v2 live  ",0 ; name
    .db $FC,$AA,$EE,$01; fake CRC32
    .db 34 ;https://wiki.nesdev.com/w/index.php/BNROM
    .db 4 ;prg_blocks
    .db 0 ;chr_blocks    
    .db 0 ;flags    
    .db 0,0,0,0,0,0,0,0 ;32bit pointers (arm)
    .db "SID    ",0; just use the datasheet and figure it out mate!            
    .db $4D,$4F,$1B,$00  ; 1.789773MHz (has to be driven by M2, no choice!)
    .dw $0002 ; channel mask (only right)        
    .dw $5000 ; audio   adress
    .dw $8000 ; banking adress    
    .dw $8000 ; reset adress
    .db $FF   ; reset inactive
    .db $EF   ; reset active
ENDIF

IFDEF GALDCSG
    .org $FF00
    .db "FCDCSG r0 live ",0 ; name
    .db $FC,$AA,$EE,$01; fake CRC32
    .db 34 ;https://wiki.nesdev.com/w/index.php/BNROM
    .db 4 ;prg_blocks
    .db 0 ;chr_blocks    
    .db 0 ;flags    
    .db 0,0,0,0,0,0,0,0 ;32bit pointers (arm)
    .db "DCSG   ",0; just use the datasheet and figure it out mate!            
    .db $4D,$4F,$1B,$00  ; 1.789773MHz 
    .dw $0001 ; channel mask
    .dw $5000 ; audio   adress
    .dw $8000 ; banking adress    
    .dw $0000 ; reset adress (none no use!)
    .db $00   ; reset inactive
    .db $00   ; reset active
ENDIF

IFDEF GALOPN2
    .org $FF00
    ;FCROM START//////////////////////////////////
    .db "FCOPN2 r0 live ",0
    .db $FC,$AA,$EE,$01; fake CRC32
    .db 34 ;https://wiki.nesdev.com/w/index.php/BNROM
    .db 4 ;prg_blocks
    .db 0 ;chr_blocks    
    .db 0 ;flags    
    .db 0,0,0,0,0,0,0,0 ;32bit pointers (arm)
    .db "OPN2   ",0; just use the datasheet and figure it out mate!            
   ;.db $00,$80,$70,$00  ; 7.3728MHz (uint32_t) weird xtal on proto
   ;.db $00,$30,$75,$00  ; 7.6800MHz (uint32_t) weird xtal on proto
    .db $00,$12,$7A,$00  ; 8.0000MHz (uint32_t) weird xtal on proto
    .dw $0002 ; channel mask (only right)        
    .dw $5000 ; audio   adress
    .dw $8000 ; banking adress    
    .dw $8000 ; reset adress
    .db $FF   ; reset inactive
    .db $EF   ; reset active
ENDIF

IFDEF GALOPLL
    .org $FF00
    .db "FCOPLLr0 live  ",0
    .db $FC,$AA,$EE,$01; fake CRC32
    .db 34 ;https://wiki.nesdev.com/w/index.php/BNROM
    .db 4 ;prg_blocks
    .db 0 ;chr_blocks    
    .db 0 ;flags    
    .db 0,0,0,0,0,0,0,0 ;32bit pointers (arm)
    .db "OPLL   ",0; just use the datasheet and figure it out mate!                
    .db $99,$9E,$36,$00  ; 3.579545MHz (uint32_t) 
    .dw $0002 ; channel mask (only right)        
    .dw $5000 ; audio   adress
    .dw $8000 ; banking adress    
    .dw $8000 ; reset adress
    .db $FF   ; reset inactive
    .db $EF   ; reset active
ENDIF
            
IFDEF GALOPL3
    .org $FF00
    .db "FCOPL3r0 live  ",0
    .db $FC,$AA,$EE,$01; fake CRC32
    .db 34 ;https://wiki.nesdev.com/w/index.php/BNROM
    .db 4 ;prg_blocks
    .db 0 ;chr_blocks    
    .db 0 ;flags    
    .db 0,0,0,0,0,0,0,0 ;32bit pointers (arm)
    .db "OPL3   ",0; just use the datasheet and figure it out mate!            
    .db $65,$7A,$DA,$00  ; 14.318181MHz
    .dw $0002 ; channel mask (only right)        
    .dw $5000 ; audio   adress
    .dw $8000 ; banking adress    
    .dw $8000 ; reset adress
    .db $FF   ; reset inactive
    .db $EF   ; reset active
ENDIF    
        
    .org $FFFA
    .dw nmihandler,    reset, irqhandler
ELSE
    .pad $DFF6    
    .dw nmihandler
    .dw nmihandler
    .dw nmihandler
    .dw reset
    .dw irqhandler    
    .pad $1BEA0
ENDIF

; for mappers with CHRs...
IFNDEF CHRRAM  
.incbin "bitmaps.chr"
ENDIF
