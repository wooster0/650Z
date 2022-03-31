; .export _main

; _main:
; ; Initialization
; lda #$6
; sta $aa
; ldy #0

; sta ($aa), Y ; Access address $aa in memory (which will be 5) and add Y (2) to that.
;              ; Result: A will be 7.
; jsr $FFF9

; lda #4
; ora #2
; ldx #1

; lda #%1111
; and #%0110

; lda #%1111
; eor #%1010

; lda #127
; adc #1
; adc #1
; adc #1

; lda #4
; cmp #3

lda #0
adc #$ff

inx


; sta $abcd,x

; ldx #10
; ldy #15

; ; Expected result:
; ; A = 5, X = 10, Y = 15
