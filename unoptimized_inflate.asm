    org    $808000

; Uncompress DEFLATE stream STArting from the address stored in inputPointer
; to the memory STArting from the address stored in outputPointer

RESET:
        SEI

        ; Exit emulation mode
        CLC
        XCE
        
        ; Clear interrupts, DMA, HDMA
        STZ $4200
        STZ $420B
        STZ $420C

        ; Jump to FastROM area. Works because ORG specifies bank $80
        JML +

+        REP #$20

        ; Set STAck pointer to $00:0FFF
        LDA #$0FFF
        TCS
        
        ; Set data bank to $80 (FastROM area)
        PHK
        PLB
        
        ; Set direct page register to $0000
.zero   PEA $0000
        PLD

        ; Enable FastROM through hardware register
        LDX #$01
        STX $420D

        SEP #$20
        REP #$10

        ; Clear RAM $7E:0000-$7F:FFFF
        LDX #$8008
        STX $4300
        LDX.w #.zero+1
        STX $4302
        LDA.b #.zero+1>>16
        STA $4304
        LDX #$0000
        STX $4305
        STX $2181
        STZ $2183
        LDA #$01
        STA $420B
        LDA #$01
        STA $2183
        STA $420B
        SEP #$10
        
        ; Initialize every single hardware register ($21xx & $42xx)
        PHD
        PEA $2100
        PLD
        LDA #$80
        STA $00
        STZ $01
        STZ $02
        STZ $03
        STZ $04
        STZ $05
        STZ $06
        STZ $07
        STZ $08
        STZ $09
        STZ $0A
        STZ $0B
        STZ $0C
        STZ $0D
        STZ $0D
        STZ $0E
        STZ $0E
        STZ $0F
        STZ $0F
        STZ $10
        STZ $10
        STZ $11
        STZ $11
        STZ $12
        STZ $12
        STZ $13
        STZ $13
        STZ $14
        STZ $14
        STZ $15
        STZ $16
        STZ $17
        STZ $18
        STZ $19
        STZ $1A
        STZ $1B
        STZ $1B
        STZ $1C
        STZ $1C
        STZ $1D
        STZ $1D
        STZ $1E
        STZ $1E
        STZ $1F
        STZ $1F
        STZ $20
        STZ $20
        STZ $21
        STZ $22
        STZ $22
        STZ $23
        STZ $24
        STZ $25
        STZ $26
        STZ $27
        STZ $28
        STZ $29
        STZ $2A
        STZ $2B
        STZ $2C
        STZ $2D
        STZ $2E
        STZ $2F
        STZ $30
        STZ $31
        LDA #$80
        STA $32
        LDA #$40
        STA $32
        LDA #$20
        STA $32
        STZ $33
        STZ $40
        STZ $41
        STZ $42
        STZ $43
        PEA $4200
        PLD
        STZ $01
        STZ $02
        STZ $03
        STZ $04
        STZ $05
        STZ $06
        STZ $07
        STZ $08
        STZ $09
        STZ $0A
        STZ $0B
        STZ $0C
        PLD

    REP #$20
    LDA.w #example_data
    STA data.source
    LDY.b #example_data>>16
    STY data.source+2
    LDA #$2000
    STA data.dest
    LDY.b #$7E
    STY data.dest+2
    STY data.dest_window+2
    SEP #$30
    PHK
    PLB
    stz $5003
    
    rep #$30
    lda #$8000    ;$7E8000
    sta $2181
    lda #example_data	
    sta $4302		
    lda #1640
    sta $4305		
    lda #$8000		
    sta $4300		
    sep #$20		
    lda #example_data>>16
    sta $4304		
    lda #$01		
    sta $420B		
        ldx #$8000	;$7E8000
    stx $2181
    rep #$30
    JSR inflate
    stz $5005
-    bra -

print "start ",pc

struct tree $0000
	.code_lengths: skip 16*2
	.translate: skip 288*2
endstruct

struct trees $0200
	.dynamic:
		.dynamic_ltree: skip sizeof(tree)
		.dynamic_dtree: skip sizeof(tree)
endstruct

print "trees.dynamic_ltree", hex(trees.dynamic_ltree)
print "trees.dynamic_dtree", hex(trees.dynamic_dtree)

struct tables $800
	.code_tree: skip sizeof(tree)
	.lengths: skip 288+32*2
	.offsets: skip 32
endstruct

print "tables.code_tree", hex(tables.code_tree)
print "tables.lengths", hex(tables.lengths)
print "tables.offsets", hex(tables.offsets)

struct data $F0
	.source: skip 3
	.dest: skip 3
	.dest_window: skip 3
	.bit_buffer: skip 2	;consider not being 16 bit?
	.bit_count: skip 2	;reserve 16 bit so access size doesn't matter
	.last: skip 2
endstruct

static_trees:
	static_ltree:
		static_ltree_code_length:
			dw 0, 0, 0, 0, 0, 0, 0, 24
			dw 152, 112, 0, 0, 0, 0, 0, 0
		static_ltree_translate:
			dw 256, 257, 258, 259, 260, 261 
			dw 262, 263, 264, 265, 266, 267 
			dw 268, 269, 270, 271, 272, 273 
			dw 274, 275, 276, 277, 278, 279 
			dw 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 
			dw 10, 11, 12, 13, 14, 15, 16, 17 
			dw 18, 19, 20, 21, 22, 23, 24, 25 
			dw 26, 27, 28, 29, 30, 31, 32, 33 
			dw 34, 35, 36, 37, 38, 39, 40, 41 
			dw 42, 43, 44, 45, 46, 47, 48, 49 
			dw 50, 51, 52, 53, 54, 55, 56, 57 
			dw 58, 59, 60, 61, 62, 63, 64, 65 
			dw 66, 67, 68, 69, 70, 71, 72, 73 
			dw 74, 75, 76, 77, 78, 79, 80, 81 
			dw 82, 83, 84, 85, 86, 87, 88, 89 
			dw 90, 91, 92, 93, 94, 95, 96, 97 
			dw 98, 99, 100, 101, 102, 103, 104
			dw 105, 106, 107, 108, 109, 110, 111
			dw 112, 113, 114, 115, 116, 117, 118
			dw 119, 120, 121, 122, 123, 124, 125
			dw 126, 127, 128, 129, 130, 131, 132
			dw 133, 134, 135, 136, 137, 138, 139
			dw 140, 141, 142, 143, 280, 281, 282
			dw 283, 284, 285, 286, 287, 144, 145
			dw 146, 147, 148, 149, 150, 151, 152
			dw 153, 154, 155, 156, 157, 158, 159
			dw 160, 161, 162, 163, 164, 165, 166
			dw 167, 168, 169, 170, 171, 172, 173
			dw 174, 175, 176, 177, 178, 179, 180
			dw 181, 182, 183, 184, 185, 186, 187
			dw 188, 189, 190, 191, 192, 193, 194
			dw 195, 196, 197, 198, 199, 200, 201
			dw 202, 203, 204, 205, 206, 207, 208
			dw 209, 210, 211, 212, 213, 214, 215
			dw 216, 217, 218, 219, 220, 221, 222
			dw 223, 224, 225, 226, 227, 228, 229
			dw 230, 231, 232, 233, 234, 235, 236
			dw 237, 238, 239, 240, 241, 242, 243
			dw 244, 245, 246, 247, 248, 249, 250
			dw 251, 252, 253, 254, 255

	static_dtree:
		static_dtree_code_length:
			dw 0, 0, 0, 0, 0, 32, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
		static_dtree_translate:		;maybe do an index optimization on this one...
			dw 0, 1, 2, 3, 4, 5, 6, 7, 8
			dw 9, 10, 11, 12, 13, 14, 15, 16 
			dw 17, 18, 19, 20, 21, 22, 23, 24
			dw 25, 26, 27, 28, 29, 30, 31, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0



code_length_codes:		;hell of a name...
	dw 16*2, 17*2, 18*2, 0*2, 8*2, 7*2, 9*2, 6*2
	dw 10*2, 5*2, 11*2, 4*2, 12*2, 3*2, 13*2, 2*2
	dw 14*2, 1*2, 15*2
	
length_bits:
	dw 0, 0, 0, 0, 0, 0, 0, 0
	dw 1, 1, 1, 1, 2, 2, 2, 2
	dw 3, 3, 3, 3, 4, 4, 4, 4
	dw 5, 5, 5, 5, 0, 6
	
length_base:
	dw 3, 4, 5, 6, 7, 8, 9, 10
	dw 11, 13, 15, 17, 19, 23, 27
	dw 31, 35, 43, 51, 59, 67, 83
	dw 99, 115, 131, 163, 195, 227, 258, 323

dist_bits:	
	dw 0, 0, 0, 0, 1, 1, 2, 2
	dw 3, 3, 4, 4, 5, 5, 6, 6
	dw 7, 7, 8, 8, 9, 9, 10
	dw 10, 11, 11, 12, 12, 13, 13
	
dist_base:
	dw 1, 2, 3, 4, 5, 7, 9, 13, 17
	dw 25, 33, 49, 65, 97, 129, 193
	dw 257, 385, 513, 769, 1025, 1537
	dw 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577
	
macro fill_bit_buffer()
	lda.b [data.source]
	inc data.source
	inc data.source
	sta data.bit_buffer
	lda #$0010
	sta data.bit_count
endmacro
	
; Read one bit, return in the C flag
macro get_bit()
	lsr data.bit_buffer
	dec data.bit_count
	bne ?return
	pha			;if the buffer is drained load 16 new bits
	%fill_bit_buffer()
	pla
?return:
endmacro

;todo
macro read_bits()	;this can in general be optimized a lot, but for now its fine
	cpy #$0000	;possibly we can optimize this out
	beq ?skip	;this structure now works well enough that I can optimize it into an AND instruction at some point
		lda #$8000
		?next_bit:
			%get_bit()
			ror
			dey
		bne ?next_bit
		
		clc
		?normalize:
			ror
		bcc ?normalize
	?skip:
		clc
		adc $00
endmacro

macro read_bits_base(bit_count, base)	;need to update this
	ldy <bit_count>	
	beq ?skip	;this structure now works well enough that I can optimize it into an AND instruction at some point
		lda #$8000
		?next_bit:
			%get_bit()
			ror
			dey
		bne ?next_bit
		
		clc
		?normalize:
			ror
		bcc ?normalize
		clc
		adc <base>
	?skip:
endmacro

	
inflate_exit:
	rts
inflate:
	stz data.bit_buffer
	stz data.bit_count
	stz data.last
	%fill_bit_buffer()
	
	inflate_loop:
		lsr data.last
		bcs inflate_exit
		%get_bit()
		rol data.last
		
		ldy #$0002
		stz $00		;optimize by making a second routine
		%read_bits()
		;sta $5010
		
		cmp #$0000	;possible to drop later
		bne +
			jmp inflate_uncompressed
		+
		cmp #$0001	;lsr may be a quicker option
		beq inflate_static_block
		;assume type 3 as type 4 is malformed.
		jsr decode_trees
		ldx #trees.dynamic
		bra inflate_block
		
	inflate_static_block:
		ldx #static_trees
	inflate_block:
		jsr decode_symbol	;probably going to inline this
		cmp #$0100
		beq inflate_loop
		bcs .distance_fill
			sta.b [data.dest]
			inc data.dest
			bra inflate_block
		.distance_fill
			sec
			sbc #$0101
			asl	;probably needed
			tay
			lda length_base,y
			sta $00
			;sta $5010
			lda length_bits,y
			;sta $5010
			tay
			%read_bits()
			;sta $5010
			sta $08
			txa
			clc
			adc #sizeof(tree)
			phx
			tax
			jsr decode_symbol
			;sta $5010
			asl
			tay
			lda dist_base,y
			;sta $5010
			sta $00
			lda dist_bits,y
			;sta $5010
			tay
			%read_bits()
			;sta $5010
			sta $00
			lda data.dest
			sec
			sbc $00
			sta data.dest_window
			ldx $08
			;stx $5010
			.copy	;can be optimized to word copies, or mvn....
				lda.b [data.dest_window],y
				sta $5010
				sta.b [data.dest]
				inc data.dest
				iny
				dex
			bne .copy
			plx
			jmp inflate_block
		
	inflate_uncompressed:		;possibly make this a RAM routine so I can use MVN?
		lda data.bit_count
		cmp #$0008
		bcc +
			dec data.source
			lsr data.bit_count
			bra inflate_uncompressed	;handle worst case of buffer just filled with 16 bits
		+
		
		lda.b [data.source]
		xba
		tay
		iny #4
		phy
		
		-	;can probably be optimized as this is technically a word copy (lazy because maybe mvn later)
			lda.b [data.source],y
			sta.b [data.dest],y
			dey
		bne -
		
		pla
		clc
		adc data.source
		sta data.source
		
		%fill_bit_buffer()
		jmp inflate_loop
			
	decode_symbol:
		txy
		stz $00
		lda #$0000
		-	
			%get_bit()
			rol
			iny
			iny
			pha
			lda tree.code_lengths,y
			clc		;probably always clear, test later
			adc $00
			sta $00
			pla
			sec
			sbc tree.code_lengths,y
			bpl -
		stx $02
		clc
		adc $00
		asl
		adc $02
		tay
		lda tree.translate,y
		sta $5010
		rts
		
;A = count
;X = table
;$04 = offset
build_tree:	;can't wait to test this and have it not work!
	sta $00
	sta $02
	stx $06
	lda #$000F	;eventually make mvn probably
	txy
	-
		stz tree.code_lengths,x
		inx
		inx
		dec
	bpl -
	lda $04
	clc
	adc #tables.lengths
	pha
	tyx
	tay
	-
		phx
		txa
		clc
		adc $0000,y	;y=lengths+off
		tax
		inc tree.code_lengths,x
		plx
		iny
		iny
		dec $00
	bne -
	ldx $06
	stz tree.code_lengths,x
	
	ldy #$0000
	tya
	-
		sta tables.offsets,y
		adc tree.code_lengths,x
		inx
		inx
		iny
		iny
		cpy #$0020
	bne -
	
	pla
	tax
	;here now
	-
		lda $00,x	;x=lengths+off
		beq +
			clc
			adc #tables.offsets
			tay
			lda $0000,y
			inc
			sta $0000,y
			dec	;hack for now optimize later
			asl	;probably needed didn't check
			clc
			adc $06
			tay
			lda $00
			sta tree.translate,y
		+
		inx
		inx
		inc $00
		dec $02
	bne -
	rts
	
decode_trees:
	%read_bits_base(#$0005, #$0101)	;hlit
	sta $08
	%read_bits_base(#$0005, #$0001)	;hdist
	sta $0A
	%read_bits_base(#$0004, #$0004)	;hclen
	sta $0C
	
	ldx #$0026	;maybe OBO (or two, fuck it maybe even three)
	-
		stz tables.lengths,x
		dex
		dex
	bpl -
	
	ldx #code_length_codes
	-
		%read_bits_base(#$0003, #$0000)
		ldy $0000,x
		;sty $5010
		asl
		sta tables.lengths,y
		;sta $5010
		inx
		inx
		dec $0C
	bne -
	
	ldx #tables.code_tree
	stz $04
	lda #$0012
	jsr build_tree
	
	
	;loop of death
	lda $08
	clc
	adc $0A
	sta $0E
	ldx #$0000
	.loop
		phx
		ldx #tables.code_tree
		jsr decode_symbol	;this could be expensive....3k cycles worst case... 
		plx
		cmp #$0010
		bcs +
			asl
			sta tables.lengths,x
			inx
			inx
			dec $0E
			bne .loop		;optimize for what I think will be the common case
			jmp .end
		+
		bne +
			;sym = 16
			%read_bits_base(#$0002, #$0003)
			tay
			lda tables.lengths-2,x
			-
				sta tables.lengths,x
				inx
				inx
				dec $0E		;0E may be worth making a compare rather than dec
				dey
			bne -
			lda $0E
			bne .loop
			jmp .end
		+
		cmp #$0011
		bne +
			;sym = 17
			%read_bits_base(#$0003, #$0003)
			-
				stz tables.lengths,x
				inx
				inx
				dec $0E		;0E may be worth making a compare rather than dec
				dec
			bne -
			lda $0E
			beq .end
			jmp .loop
		+
		;sym = 18
		%read_bits_base(#$0007, #$000B)
		-
			stz tables.lengths,x
			inx
			inx
			dec $0E		;0E may be worth making a compare rather than dec
			dec
		bne -
		lda $0E
		beq .end
		jmp .loop
	.end
	
	;end loop of death
	
	stz $04
	lda $08
	ldx #trees.dynamic_ltree
	jsr build_tree
	lda $08
	asl
	sta $04
	lda $0A
	ldx #trees.dynamic_dtree
	jsr build_tree
	
;A = count
;X = table
;$04 = offset
	
	rts
		
print "end ",pc

example_data:
	;db $73,$04,$00
	incbin GFX00.bin.deflate
youscrewedup:
stp

    ; I mostly copypasted this header ROM from an old project of mine
; I'm not sure if the values are correct or not BUT HEY the rom works
org $00FFB0
        db "FF"                ;maker code.
        db "FFFF"            ;game code.
        db $00,$00,$00,$00,$00,$00,$00    ;fixed value, must be 0
        db $00                ;expansion RAM size. SRAM size. 128kB
        db $00                ;special version, normally 0
        db $00                ;cartridge sub number, normally 0s

        db "INFLATE              "    ;ROM NAME
        db $30                ;MAP MODE. Mode 30 = fastrom
        db $02                ;cartridge type. ROM AND RAM AND SRAM
        db $09                ;3-4 MBit ROM        
        db $00                ;64K RAM        
        db $00                ;Destination code: Japan
        db $33                ;Fixed Value    
        db $00                ;Mask ROM. This ROM is NOT revised.
        dw $B50F            ;Complement Check.
        dw $4AF0            ;Checksum

        ;emulation mode
        dw $FFFF            ;Unused
        dw $FFFF            ;Unused
        dw youscrewedup        ;COP
        dw youscrewedup        ;BRK
        dw youscrewedup        ;ABORT
        dw $FFFF                ;NMI
        dw $FFFF            ;Unused
        dw $FFFF                ;IRQ

        ;native mode
        dw $FFFF            ;Unused
        dw $FFFF            ;Unused
        dw youscrewedup        ;COP
        dw youscrewedup        ;BRK
        dw youscrewedup        ;ABORT
        dw $FFFF            ;NMI
        dw RESET            ;RESET
        dw $FFFF            ;IRQ
