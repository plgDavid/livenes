;//$00..$0F is used in FDS as temporary memory for the BIOS. The main program can use it as temporary memory too.

currdpad       = $10 ;current  status of DPAD  
prevdpad       = $11 ;previous status of DPAD
cur_reg        = $12 ;
selected_bit   = $13 ;the cursor highlights which bit 
posaccum       = $14 ;when gliding
joytmp1        = $15 ;needed for read_joy.inc

cur_reg_bits_ptr=$16
;//$17 
cur_regHtbl=$18
;//$19
cur_regLtbl=$1A
;//$1B
cur_row_to_reg_col0=$1C
;//$1D
cur_row_to_reg_col1=$1E
;//$1F

;// register shadows
RP2A03_shadow   = $20 ; ALWAYS there
ALTERNATE_shadow= $40 ; VRC6/MMC5/FDS/SB5/N163 and second RP2A03 uses that

cur_shadow_ptr      = $60
;//$61
;// fill this before calling ramdrawreg
ramdrawreg_arg0  = $62
tempA            = $63
write_pair_reg = $64
write_pair_val = $65

; memcpy
ptr_src        = $70
ptr_dst        = $72

temp0          = $74 ;temp function storage
temp1          = $75 ;temp function storage
temp2          = $76 
spriteColor    = $77
spriteX        = $78 ;a X position for our sprite (for IRQ)
spriteY        = $79 ;a Y position for our sprite (for IRQ)
initdone       = $7A ; 
sleeping       = $7B ;nonzero if main thread is waiting for VBlank
selectedCOL    = $7C ; 
selectedCOLH   = $7D ;used to clip the cursor in Y dependent on column's unique height
cursorX        = $7E ;cursor in imaginary 16x16 grid
cursorY        = $7F ; 

cur_reg_table   = $80 ;zero page pointers to all needed registers
;//do NOT use anything until C0! (SID uses a WHOLE LOT of em)

cur_maxy_col0  = $C0
cur_maxy_col1  = $C1

FDS_WAVE       = $CB ; fake register

cur_nametable  = $CC
prev_reg       = $CD
cur_set        = $CE ; 0=RP2A03, 1=Alternate (mapper or second RP2A03)
cur_num_regs   = $CF

;//FDS BIOS needs those to be filled? we can do that even in no FDS I guess
FDS4025 = $FA
;//FDS MIRRORS
PPU2005V = $FC
PPU2005H = $FD
PPU2001  = $FE
PPU2000  = $FF

;//ram copy of what needs to be writen by PPU
;//8bits*20registers = 160. Will use [$300:$39F]
oam        = $0200    ; shadow oam
screentemp = $0300