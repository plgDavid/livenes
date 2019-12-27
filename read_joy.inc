; Reads controller
; Out: A=buttons pressed, where bit 0 is A button
read_joy:
       ;; Strobe controller
       lda #1
       sta $4016
       lda #0
       sta $4016
        
       ;; Read all 8 buttons
       ldx #8
loop:
       pha
        
       ;; Read next button state and mask off low 2 bits.
       ;; Compare with $01, which will set carry flag if
       ;; either or both bits are set.
       lda $4016
       and #$03
       cmp #$01
        
       ;; Now, rotate the carry flag into the top of A,
       ;; land shift all the other buttons to the right
       pla
       ror a
        
       dex
       bne loop
        
       rts
; temp is a zero-page variable
 
; Reads controller. Reliable when DMC is playing.
; Out: A=buttons held, A button in bit 0
read_joy_safe:
       ;; Get first reading
       jsr read_joy
        
mismatch:
       ;; Save previous reading
       sta joytmp1
        
       ;; Read again and compare. If they differ,
       ;; read again.
       jsr read_joy
       cmp joytmp1
       bne mismatch
        
       rts