;
; Author: Michel Iwaniec <michel_iwaniec@yahoo.com>
;
; Uses the nmi_sync library to ensure minimal CPU / PPU jitter between frames, authored by Shay Green
;
; Build with:
;   ca65 main.s -v -g -l nes_color_transitions.lst
;   ld65 -o nes_color_transitions.nes -C config.cfg -v -m nes_color_transitions.map -vm --dbgfile nes_color_transitions.dbg main.o
;
; This NES program uses mid-frame palette re-writes to displays a pattern of vertical bars of 12 different colors on top of a backdrop
; that changes between the 52 unique PPU colors on the NES.
;
; The intent of the program is to allow studying the rise/fall times between different colors with a high-sample-rate oscilloscope / upscaler.
;
; As a special-case, the first 12 scanlines replace the hue colors in the vertical bars with the following
; grayscale values: $0D,$1D,$0F,$2D,$00,$10,$3D,$20,$30,$30,$30,$30
; 
; This allows capturing grayscale-to-grayscale transitions, which the colored bars section will omit.
; The last 4 grayscale values (all $30) are redundant and only appear to allow sharing the raster loop with the colored-bars version.
;
; While this program allows capturing "all" color transitions, this is still limited to just one pre-set emphasis setting.
; However the emphasis setting for the entire frame can be changed with the following joypad buttons:
; * Hold SELECT:    Enable mono-mode
; * Hold START:     Enable RED emphasis
; * Hold B:         Enable GREEN emphasis
; * Hold A:         Enable BLUE emphasis
;
; The thickness of the bars is chosen to be 2 pixels, to allow measurements of a stable value in case of slow rise and fall times.
; However, to align with the 16x16 attribute grid the number of backdrop pixels varies between 3 and 4 pixels.
; More specifically, the palette entries displayed on each scanline are as follows:
;
; 3x$00, 2x$01, 3x$00, 2x$02, 3x$00, 2x$03, 4x$00, 2x$05, 3x$00, 2x$06, 3x$00, 2x$07, 2x$09, 3x$00, 2x$0A, 3x$00, 2x$0B, 2x$0D, 3x$00, 2x$0E, 3x$00, 2x$0F, 17x$00 (...repeat...)
;
; TODO:
; * PAL support. This provides challenges for the goal of capturing it in 1 frame, given there's 6 possible subcarrier phases instead of just 3
;
; --- Structure of the program's palette update ---
;
; In more detail, the colors in a frame are generated as follows:
; * The palette is setup so that c1..c3 of each sub-palette consist of 9 grayscale values: $0D,$1D,$0F,$2D,$00,$10,$3D,$20,$30,$30,$30,$30
; * For each of 8 scanlines, the common backdrop color is changed to cycle through 8 grayscales: $20,$3D,$10,$00,$2D,$0F,$1D,$0D
; * 1 full scanline is then spent to re-write palette so that the hues $01 to $0C occupy c1..c3 of the BG sub-palettes (12 hues total)
; * The 12 vertical bars get repeatedly displayed over the scanline for a total of 3 sections, in order to allow capturing the 3 possible subcarrier phases on NTSC
; * After the end of each scanline the hblank period is used to re-write the common backdrop color, cycling it through gs value $xD, colors $xC to $x1, and gs value $x0
; * The above process is repeated 4 times with the luminance for the common backdrop color being decreased after each iteration
; * After 4 * 14 = 56 scanlines, 1 full scanline is spent rewriting colors c1..c3 in each sub-palette with a brighter luminance
; * After 8 + 4 * 57 = 236 scanlines have passed, the remaining 4 scanlines are used to read joypad input and update emphasis values for the next frame
;
.include "nmi_sync.s"
.include "delaymacro.s"

sprites                     = $200          ; OAM page
INIT_FINE_DELAY = 1                         ; Initial value for HBlank fine-tune delay. Allowed range: 0 <= INIT_FINE_DELAY <= 3
SCAN_ACC_ADD    = 171                       ; NTSC: 0.666 fractional cycles / scanline -> 256 * 0.666 ~= 171
PSNT_ADDRESS    = $2C00                     ; PPU memory address for PSNT
PSNT_DST_ROW    = 24                        ; Destination row in PSNT

.zeropage
paletteCache:                   .res 25         ; Entire palette, excluding mirror registers
ScrollX:                        .res 1          ; Background scroll X
ScrollY:                        .res 1          ; Background scroll Y
ScrollY_t:                      .res 1          ; Background scroll Y (temporary variable incremented by raster code)
scanAcc:                        .res 1          ; Scanline accumulator used to emulate fractional cycles
numLines:                       .res 1
tmp:                            .res 2
fineDelay:                      .res 1          ; HBlank fine-tuning delay
frameCounter:                   .res 1
tmpB:                           .res 2
delayMacroVar:                  .res 1
x2001_enableOrDisableMask:      .res 1          ; $2001 render mask for reg X in HBlank code
palRewriteIndex:                .res 1          ; Palette index to rewrite mid-frame
palRewriteValue:                .res 1          ; Palette value to rewrite mid-frame
palRewriteScanline:             .res 1          ; Desired scanline to execute palette rewrite
palRewriteScanlineAdjusted:     .res 1          ; Adjusted scanline, to make sure fine-y = 3 or 7
r2001:                          .res 1          ; $2001 value
psntDstAddr:                    .res 2          ; Stores destination of where row is copied into "palette shadow nametable"
psntDstAddrAT:                  .res 2          ; Same for row's attribute data
psntSrcRow:                     .res 1          ; Row index in source (ROM) nametable
joy:
joy0:                           .res 1
joy1:                           .res 1
joyP:
joyP0:                          .res 1
joyP1:                          .res 1
backdropColorTableIndex:        .res 1
zeroByte:                       .res 1
backdropTablePtr:               .res 2

JOY_A       = %00000001
JOY_B       = %00000010
JOY_SELECT  = %00000100
JOY_START   = %00001000
JOY_UP      = %00010000
JOY_DOWN    = %00100000
JOY_LEFT    = %01000000
JOY_RIGHT   = %10000000

;
; Delay by a fractional amount of cycles using the scanline accumulator byte
;
; Takes 11.666 cycles (NTSC)
;
.MACRO DELAY_ACC
    lda scanAcc
    clc
    adc #SCAN_ACC_ADD
    bcs :+
:
    sta scanAcc
.ENDMACRO

;
; Writes palette cache to PPU
;
.MACRO WRITE_PALETTE_CACHE
    lda #$3F
    sta $2006
    lda #$00
    sta $2006
    ldx paletteCache
.repeat 8,i
    stx $2007
    lda paletteCache+1+3*i+0
    sta $2007
    lda paletteCache+1+3*i+1
    sta $2007
    lda paletteCache+1+3*i+2
    sta $2007
.endrep
.ENDMACRO

;
; Sets X / Y of a single sprite
;
.MACRO SET_SPRITE spriteIndex, spriteX, spriteY
    lda spriteX
    sta sprites+spriteIndex*4+3
    lda spriteY
    sta sprites+spriteIndex*4+0
.ENDMACRO

; Writes all 12 hues to palette, with preset brightness
; Takes 96 cycles
.MACRO WRITE_HUES_TO_PALETTE br
    lda #$3F
    sta $2006
    lda #$01
    ldx #($01 + br)
    sta $2001 ; Blanked but MONO-bit ON
    sta $2006
    ;
    stx $2007
    lda #($02 + br)
    sta $2007
    lda #($03 + br)
    sta $2007
    bit $2007
    ;
    lda #($04 + br)
    sta $2007
    lda #($05 + br)
    sta $2007
    lda #($06 + br)
    sta $2007
    bit $2007
    ;
    lda #($07 + br)
    sta $2007
    lda #($08 + br)
    sta $2007
    lda #($09 + br)
    sta $2007
    bit $2007
    ;
    lda #($0A + br)
    sta $2007
    lda #($0B + br)
    sta $2007
    lda #($0C + br)
    sta $2007
.ENDMACRO

.CODE

TileData:
; 3xUBC -> 2xBP1
; 2xBP1 -> 3xUBC (back)
; 3xUBC -> 2xBP2
; 2xBP2 -> 3xUBC (back)
; 3xUBC -> 2xBP3
; 2xBP3 -> 1xUBC (back)
; 1xUBC (sustained)
bg_chr:
bg_chr_tile_left:
.repeat 8
;      UUU11UUU
.byte %00011000
.endrep
.repeat 8
.byte %00000000
.endrep
bg_chr_tile_right:
.repeat 8
;      22UUU33U
.byte %00000110
.endrep
.repeat 8
.byte %11000110
.endrep
; bg_chr_blank
.repeat 16
.byte %00000000
.endrep

NameTableData:
.repeat 30
.byte 0,1   ; BGP0
.byte 0,1   ; BGP1
.byte 0,1   ; BGP2
.byte 0,1   ; BGP3
.byte 2,2   ; ***PADDING***
.byte 0,1   ; BGP0
.byte 0,1   ; BGP1
.byte 0,1   ; BGP2
.byte 0,1   ; BGP3
.byte 2,2   ; ***PADDING***
.byte 0,1   ; BGP0
.byte 0,1   ; BGP1
.byte 0,1   ; BGP2
.byte 0,1   ; BGP3
.byte 2,2   ; ***PADDING***   
.byte 2,2   ; ***PADDING***
.endrep
; Attributes
.repeat 8
.byte %00000100, %00001110, %00000000, %00001001, %00000011, %00000100, %00001110, %00000000
.endrep

.align $100
backdropTable:
    .byte $00
    .byte $01
    .byte $02
    .byte $03
    .byte $04
    .byte $05
    .byte $06
    .byte $07
    .byte $08
    .byte $09
    .byte $0A
    .byte $0B
    .byte $0C
    .byte $0D
    ;
    .byte $10
    .byte $11
    .byte $12
    .byte $13
    .byte $14
    .byte $15
    .byte $16
    .byte $17
    .byte $18
    .byte $19
    .byte $1A
    .byte $1B
    .byte $1C
    .byte $1D
    ;
    .byte $20
    .byte $21
    .byte $22
    .byte $23
    .byte $24
    .byte $25
    .byte $26
    .byte $27
    .byte $28
    .byte $29
    .byte $2A
    .byte $2B
    .byte $2C
    .byte $2D
    ;
    .byte $30
    .byte $31
    .byte $32
    .byte $33
    .byte $34
    .byte $35
    .byte $36
    .byte $37
    .byte $38
    .byte $39
    .byte $3A
    .byte $3B
    .byte $3C
    .byte $3D
backdropTable_endof:
backdropTable_SIZEOF = backdropTable_endof - backdropTable
    
backdropTableGrays:
.byte $0D
.byte $1D
.byte $0F
.byte $2D
.byte $00
.byte $10
.byte $3D
.byte $20
backdropTableGrays_endof:
backdropTableGrays_SIZEOF = backdropTableGrays_endof - backdropTableGrays

reset:
    sei
    ldx #0
    txa
:
    sta $000,x
    sta $100,x
    sta $200,x
    sta $300,x
    sta $400,x
    sta $500,x
    sta $600,x
    sta $700,x
    inx
    bne :-
    dex
    txs
    lda #$C0
    sta $4017
:
    lda $2002
    bpl :-

:
    lda $2002
    bpl :-
    ; Setup delay
    lda #INIT_FINE_DELAY
    sta fineDelay
    lda #$00
    sta ScrollX
    ; Clear nametables
    jsr ClearNameTables
    ; Initialize PPU and palette
    jsr UploadCHR
    jsr UploadNameTable
    lda #128
    jsr SetPalette
    WRITE_PALETTE_CACHE
    jsr InitOAM
    ; Enable sprites / BG in leftmost column
    lda #$1E
    sta r2001
    ; Start off with changing BG0 to a bright green
    lda #$2A
    sta palRewriteValue
    lda #$00
    sta palRewriteIndex
    ; Initialise PPU addresses for PSNT
    lda #<(PSNT_ADDRESS + 32 * PSNT_DST_ROW)
    sta psntDstAddr
    lda #>(PSNT_ADDRESS + 32 * PSNT_DST_ROW)
    sta psntDstAddr+1
    lda #<(PSNT_ADDRESS + $3C0 + (PSNT_DST_ROW / 4)*8)
    sta psntDstAddrAT
    lda #>(PSNT_ADDRESS + $3C0 + (PSNT_DST_ROW / 4)*8)
    sta psntDstAddrAT+1
    lda #0
    sta ScrollY
    ; Synchronize to PPU and enable NMI
    jsr init_nmi_sync
    lda #$90
    sta $2000
    
MainLoop:
    jsr wait_nmi
    jmp MainLoop

.align $100 ; branches must not cross page
nmi:
    pha
    
    ; Do this sometime before you DMA sprites
    jsr begin_nmi_sync
    
    ; DMA then enable sprites. Instructions before
    ; STA $4014 (excluding begin_nmi_sync) must take
    ; an even number of cycles. The only required
    ; instruction here is STA $4014.
    bit <0          ; to make cycle count even
    lda #0
    sta $2003
    lda #>sprites
    sta $4014
    lda #$08
    sta $2001
    
    lda #$90
    sta $2000
    WRITE_PALETTE_CACHE
    DELAY 372
    lda #0
    sta $2006
    sta $2006
    lda ScrollX
    sta $2005
    lda ScrollY
    sta $2005
    sta ScrollY_t
       
    DELAY 1065-240
    jsr UpdateState

    jsr end_nmi_sync
    
    ; We're now synchronized exactly to 2286 cycles
    ; after beginning of frame.    
    DELAY 55-28-1

    DELAY 4
    
    ; Load pointer for grays backdrop changes
    lda #<backdropTableGrays
    sta backdropTablePtr
    lda #>backdropTableGrays
    sta backdropTablePtr+1
    ldy #backdropTableGrays_SIZEOF-1
    jsr DoHueTransitions

    ; Load pointer for color backdrop changes
    lda #<backdropTable
    sta backdropTablePtr
       
    ldy #backdropTable_SIZEOF-1
    WRITE_HUES_TO_PALETTE $00
    jsr DoHueTransitions
    ;
    DELAY 4
    ;
    ldy #backdropTable_SIZEOF-1
    WRITE_HUES_TO_PALETTE $10
    jsr DoHueTransitions
    ;
    DELAY 3
    ;
    ldy #backdropTable_SIZEOF-1
    WRITE_HUES_TO_PALETTE $20
    jsr DoHueTransitions
    ;
    DELAY 3
    ;
    ;DELAY_ACC
    ldy #backdropTable_SIZEOF-1
    WRITE_HUES_TO_PALETTE $30
    jsr DoHueTransitions

    pla
    rti

;
; Main raster code
;
.align $100
DoHueTransitions:
@rasterLoop:
    ; Rewrite backdrop
    lda (backdropTablePtr),y
    ldx #$3F
    stx $2006
    ldx #$00
    stx $2001
    stx $2006
    nop
    sta $2007
    nop

    ; Re-enable screen
    lda r2001
    sta $2001    
    ; Reset scroll
    stx $2006
    stx $2006

    DELAY 4

    ; Delay until 1st phase almost finished
    DELAY 23

    ; Write X-scroll for 2nd phase
    ldx #0
    stx $2005
    bit $2002    
    ; Delay until 2nd phase almost finished
    DELAY 4
    DELAY_ACC
    ; Write X-scroll for 3rd phase
    dey
    stx $2005
    stx $2005
    
    bpl :+
    rts
:
    
    jmp @rasterLoop
    
    rts

UpdateEmphasisBits:
    lda r2001
    and #$1E
    sta r2001
    ; Red emphasis
    lda joy0
    and #JOY_START
    asl
    asl
    ora r2001
    sta r2001
    ; Green emphasis
    lda joy0
    and #JOY_B
    asl
    asl
    asl
    asl
    asl
    ora r2001
    sta r2001
    ; Blue emphasis
    lda joy0
    and #JOY_A
    lsr
    ror
    ora r2001
    sta r2001
    ; Monochrome mode
    lda joy0
    and #JOY_SELECT
    lsr
    lsr
    ora r2001
    sta r2001
    rts

UpdateState:
    jsr ReadJoypads
    jsr UpdateEmphasisBits
    jsr SetScanlineAccumulator
    inc frameCounter
    rts

.align $100
;
; Delays by fineDelay cycles + 
;
DelayByXPlus36:
    cpx #0
    bne :+
    ; 0 + 36 = 36
    DELAY 12
    rts
:
    dex
    bne :+
    ; 1 + 36 = 37
    DELAY 8
    rts
:
    dex
    bne :+
    ; 2 + 36 = 38
    DELAY 4
    rts
:
    dex
    bne :+
    ; 3 + 36 = 39
    rts
:
; Should never happen unless constant out-of-range - do endless loop if so
:
    jmp :-

; Freeze program if this somehow gets triggered, rather
; than silently messing up timing
irq:    jmp irq

.align 256
R2006TabX:
.repeat 256,i
.byte ((i & $FF)>>3) & $1F
.endrep

R2006BTabY:
.repeat 256,i
.byte ((i>>3)<<5) & $FF
.endrep

ClearNameTables:
    lda #$20
    sta $2006
    lda #$00
    sta $2006
@bankSwitchNT1:
    lda #$80
    sta @bankSwitchNT1+1
    jsr @clear4Nametables
@bankSwitchNT0:
    lda #$00
    sta @bankSwitchNT0+1
@clear4Nametables:
    ldy #16
    ldx #0
    lda #0
:
    sta $2007
    inx
    bne :-
    dey
    bne :-
    rts

UploadCHR:
    @ChrPtr = tmp
    lda #$10
    sta $2006
    lda #$00
    sta $2006
    lda #>TileData
    sta @ChrPtr+1
    lda #<TileData
    sta @ChrPtr
    ldx #16
    ldy #0
:
    lda (@ChrPtr),y
    iny
    sta $2007
    bne :-
    inc @ChrPtr+1
    dex
    bne :-
    rts

UploadSpriteCHR:
    @ChrPtr = tmp
    lda #$00
    sta $2006
    lda #$00
    sta $2006
    lda #>SpriteTileData
    sta @ChrPtr+1
    lda #<SpriteTileData
    sta @ChrPtr
    ;
    ldx #64
@tileLoop:
    ldy #0
@byteLoopP0:
    lda (@ChrPtr),y
    sta $2007
    iny
    cpy #8
    bne @byteLoopP0
    ldy #0
@byteLoopP1:
    lda (@ChrPtr),y
    and #$F0
    sta tmpB
    lda #$FF
    eor tmpB
    sta $2007
    iny
    cpy #8
    bne @byteLoopP1
    lda @ChrPtr
    clc
    adc #16
    sta @ChrPtr
    lda @ChrPtr+1
    adc #0
    sta @ChrPtr+1
    dex
    bne @tileLoop
    rts

UploadNameTable:
    @ChrPtr = tmp
    lda #$20
    sta $2006
    lda #$00
    sta $2006
    lda #>NameTableData
    sta @ChrPtr+1
    lda #<NameTableData
    sta @ChrPtr
    ldx #4
    ldy #0
:
    lda (@ChrPtr),y
    iny
    sta $2007
    bne :-
    inc @ChrPtr+1
    dex
    bne :-
    rts
    
InitOAM:
    ldx #0
:
    lda #$F0
    sta sprites,x
    lda #0
    sta sprites+1,x
    sta sprites+2,x
    sta sprites+3,x
    inx
    inx
    inx
    inx
    bne :-
    rts

SetPalette:
    ldy #0
:
    lda @palette,y
    sta paletteCache,y
    iny
    cpy #25
    bne :-    
    rts
    
@palette:
.byte $20 ; Note: This needs to match the last entry in backdropTableGrays
;
.byte $0D,$1D,$0F
.byte $2D,$00,$10
.byte $3D,$20,$30
.byte $30,$30,$30
;
.byte $0F,$0F,$0F
.byte $0F,$0F,$0F
.byte $0F,$0F,$0F
.byte $0F,$0F,$0F

.align $100
ReadJoypads:
    ldx     #1
    stx     $4016
    dex
    stx     $4016
.repeat 8
    lda     $4016
    lsr
    ror     joy
.endrep
    rts

;
; Initialize scanline accumulator slightly different for odd/even frame
;
; TODO: Detect reset phase and adjust initialization
;
.align $100
SetScanlineAccumulator:
    lda frameCounter
    and #1
    bne :+
    lda #(256-SCAN_ACC_ADD) ;#0
    sta a:scanAcc
    rts
:
    lda #0 ;#(256-SCAN_ACC_ADD)
    sta scanAcc
    rts

;
; Table to use for de-glitching fine-X writes performed during rendering
;
.align $100
xFineHiByteTab:
.repeat 256,i
    .byte (i & 7) | $20
.endrep

SpriteTileData:
.incbin "numbers.chr", $0000, $400

.segment "HEADER"
    .byte "NES",26, 1,0, $E0 + %00001000, $10 ; 16K PRG, 8K CHR, Mapper30
    .byte 0,0,0,0,0,0,0,0

.segment "VECTORS"
    .word 0,0,0, nmi, reset, irq

