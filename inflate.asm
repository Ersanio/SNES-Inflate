;Coded and optimized by p4plus2
;DMA routine and testing assistance from Ersanio

;IMPORTANT ASSUMPTIONS:
;dp = 0
;interrupts = off

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
        pea $8080
        plb
        plb

    REP #$20
    LDA.w #example_data
    STA.b data.source
	LDY.b #example_data>>16
	STY.b data.source+2
	LDA #$2000
	STA.b data.dest
	LDY.b #$7E
	STY.b data.dest+2
	SEP #$30
	PHK
	PLB
	stz $5003	
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
	.bit_buffer: skip 2
	.last: skip 2
	.stack: skip 2
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

;transform bit read counts into a normalize and bit terminated pair
function t(bit_count) = ($0001<<(bit_count-1))|$8000


code_length_codes:		;hell of a name...
	dw 16*2+tables.lengths, 17*2+tables.lengths, 18*2+tables.lengths, 0*2+tables.lengths
	dw 8*2+tables.lengths, 7*2+tables.lengths, 9*2+tables.lengths, 6*2+tables.lengths
	dw 10*2+tables.lengths, 5*2+tables.lengths, 11*2+tables.lengths, 4*2+tables.lengths
	dw 12*2+tables.lengths, 3*2+tables.lengths, 13*2+tables.lengths, 2*2+tables.lengths
	dw 14*2+tables.lengths, 1*2+tables.lengths, 15*2+tables.lengths
	
length_bits:
	dw 0, 0, 0, 0, 0, 0, 0, 0
	dw t(1), t(1), t(1), t(1), t(2), t(2), t(2), t(2)
	dw t(3), t(3), t(3), t(3), t(4), t(4), t(4), t(4)
	dw t(5), t(5), t(5), t(5), t(0), t(6)
	
length_base:
	dw 3, 4, 5, 6, 7, 8, 9, 10
	dw 11, 13, 15, 17, 19, 23, 27
	dw 31, 35, 43, 51, 59, 67, 83
	dw 99, 115, 131, 163, 195, 227, 258, 323

dist_bits:	
	dw 0, 0, 0, 0, t(1), t(1), t(2), t(2)
	dw t(3), t(3), t(4), t(4), t(5), t(5), t(6), t(6)
	dw t(7), t(7), t(8), t(8), t(9), t(9), t(10)
	dw t(10), t(11), t(11), t(12), t(12), t(13), t(13)
	
dist_base:
	dw 1, 2, 3, 4, 5, 7, 9, 13, 17
	dw 25, 33, 49, 65, 97, 129, 193
	dw 257, 385, 513, 769, 1025, 1537
	dw 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577
	
macro fill_bit_buffer()
	lda.b [data.source]
	inc.b data.source
	inc.b data.source
	sec
	ror
	sta.b data.bit_buffer
endmacro
	
; Read one bit, return in the C flag
macro get_bit()
	lsr.b data.bit_buffer
	bne ?return
	sta $06			;if the buffer is drained load 16 new bits
	%fill_bit_buffer()
	lda $06
?return:
endmacro

macro get_bit_unsafe()	;won't preserve A
	lsr.b data.bit_buffer
	bne ?return
	%fill_bit_buffer()
?return:
endmacro


macro read_bits()	;this can in general be optimized a lot, but for now its fine
	beq ?skip	;this structure now works well enough that I can optimize it into an AND instruction at some point
		?next_bit:
			%get_bit()
			ror
		bcc ?next_bit
		clc
		?normalize:
			ror
		bcs ?skip
			ror
		bcc ?normalize
	?skip:
		clc
		adc $00
endmacro

macro read_bits_base(bit_count, base)	;need to update this
	lda.w #($0001<<(<bit_count>-1))|$8000
	?next_bit:
		%get_bit()
		ror
	bcc ?next_bit
	xba
	clc
	rep 8-<bit_count> : ror
		
	;bcc ?normalize
	adc <base>-1
endmacro

macro read_bits_no_base(bit_count)	;need to update this
	lda.w #($0001<<(<bit_count>-1))|$8000
	?next_bit:
		%get_bit()
		ror
	bcc ?next_bit
	xba
	clc
	rep 8-<bit_count> : ror
endmacro

	
inflate_exit:
	lda.b data.stack
	tcs
	rts
inflate:
	lda #$0001
	sta.b data.bit_buffer
	stz.b data.last
	
	tsc
	sta.b data.stack
	inflate_loop:
		lsr.b data.last
		bcs inflate_exit
		%get_bit_unsafe()
		rol.b data.last
		
		%read_bits_no_base($0002)
		bne +
			jmp inflate_uncompressed
		+
		cmp #$0001	;lsr may be a quicker option
		beq inflate_static_block
		;assume type 3 as type 4 is malformed.
		jmp decode_trees
		
		
	;here be dragons (at this point I think all of the code is dragons though)	
	macro inline_decode_symbol(table)
		ldx #<table>
		tdc
		tcs
		-	
			%get_bit()
			rol
			inx
			inx
			tay
			tsc
			adc.b tree.code_lengths,x
			tcs
			tya
			sec
			sbc.b tree.code_lengths,x
			bpl -
		sta $00
		tsc
		adc $00
		asl
		tay
		lda.w tree.translate+<table>,y
	endmacro
	
	end_block:
		bra inflate_loop
		
	inflate_static_block:
		lda #trees.dynamic
		sta $2181
		lda #static_trees	
		sta $4302		
		lda.w #sizeof(tree)*2
		sta $4305		
		lda #$8000		
		sta $4300		
		sep #$20		
		lda #static_trees>>16
		sta $4304		
		lda #$01		
		sta $420B	
		rep #$20
		bra inflate_block
		
	store_symbol:
		sta.b [data.dest]
		inc.b data.dest
	inflate_block:
		%inline_decode_symbol(trees.dynamic)
		cmp #$0100
		bcc store_symbol
		beq end_block
		asl
		tay
		lda length_base-$0202,y
		sta $00
		lda length_bits-$0202,y
		%read_bits()
		;sta $5010
		sta $08

		%inline_decode_symbol(trees.dynamic_dtree)
		asl
		tay
		lda dist_base,y
		sta $00
		lda dist_bits,y
		%read_bits()
		sta $00
		lda.b data.dest
		tay
		sec
		sbc $00
		tax
		lda $08
		mvn $7E,$7E
		inc
		mvn inflate_block>>16,inflate_block>>16,
		lda $08
		clc
		adc.b data.dest
		sta.b data.dest
		jmp inflate_block
		
inflate_uncompressed:
		lda.b data.bit_buffer
		and #$FF00
		beq +
			dec.b data.source
		+
		
		sep #$10
		lda #$8000
		sta $4300
		lda.b [data.source]
		sta $00
		sta $4305
		lda.b data.source
		clc
		adc #$0004
		sta.b data.source
		sta $4302
		ldx.b data.source+2
		stx $4304

		lda.b data.dest
		sta $2181
		stz $2183
		ldx #$01
		stx $420B
		rep #$11

		lda $00
		adc.b data.source
		sta.b data.source
		
		stx.b data.bit_buffer	; DO NOT CHANGE DMA CHANNEL OR THIS BREAKS
		jmp inflate_loop
		
;A = count
;X = table
;$04 = offset
;build_tree:	;can't wait to test this and have it not work! (it actually did, this comment stays though)
	macro build_tree()
		sta $00
		sta $02
		lda #$0007	;eventually make mvn probably
		stx $06
		txs
		-
			stz.w tree.code_lengths,x
			stz.w tree.code_lengths+2,x
			inx
			inx
			inx
			inx
			dec
		bpl -
		
		ldy $04
		-
			tsc
			adc $0000,y    ;y=lengths+off
			tax
			inc.b tree.code_lengths,x
			iny
			iny
			dec $00
		bne -
		
		tsx
		stz.w tree.code_lengths,x
		
		ldy #$0000-$20
		tdc
		-
			sta.w tables.offsets+$20,y
			adc.b tree.code_lengths,x
			inx
			inx
			iny
			iny
		bne -
		
		ldy $04
		tdc
		tcs
		-
			ldx $0000,y    ;x=lengths+off
			beq +
				lda.w tables.offsets,x
				inc.w tables.offsets,x
				asl
				adc $06
				tax
				tsc
				sta.w tree.translate,x
			+
			iny
			iny
			inc
			tcs
			dec $02
		bne -
	endmacro
	
decode_trees:
	%read_bits_base($0005, #$0101)	;hlit
	sta $08
	%read_bits_base($0005, #$0001)	;hdist
	sta $0A
	%read_bits_base($0004, #$0004)	;hclen
	tay
	
	;keeping this commented out, doesn't seem to be needed but I thought it was....
	;ldx #$0026
	;-
	;	stz.w tables.lengths,x
	;	dex
	;	dex
	;bpl -
	
	lda #code_length_codes-1
	tcs
	-
		%read_bits_no_base($0003)
		plx
		asl
		sta.b $00,x	;tables.lengths+CLC
		dey
	bne -
	
	ldx #tables.code_tree
	lda #tables.lengths
	sta $04
	lda #$0012
	%build_tree()
	
	;loop of death
	lda $08
	adc $0A
	sta $0E
	ldx #tables.lengths
	.loop
		stx $0C
		;jsr decode_symbol	;this could be expensive....3k cycles worst case... 
		%inline_decode_symbol(tables.code_tree)
		ldx $0C
		cmp #$0010
		bcs +
			asl
			sta.b $00,x
			inx
			inx
			dec $0E
			bne .loop		;optimize for what I think will be the common case
			jmp .end
		+
		bne +
			;sym = 16
			%read_bits_base($0002, #$0003)
			tay
			dex
			dex
			lda.b $00,x
			inx
			inx
			-
				sta.b $00,x
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
			%read_bits_base($0003, #$0003)
			ldy $0E
			-
				stz.b $00,x
				inx
				inx
				dey
				dec
			bne -
			tya	;lets us use a smaller instruction in the loop and set the zero flag
			sta $0E
			beq .end
			jmp .loop
		+
		;sym = 18
		%read_bits_base($0007, #$000B)
		ldy $0E
		-
			stz.b $00,x
			inx
			inx
			dey
			dec
		bne -
		tya
		sta $0E
		beq .end
		jmp .loop
	.end
	;end loop of death
	
	lda #tables.lengths
	sta $04
	lda $08
	ldx #trees.dynamic_ltree
	%build_tree()
	lda $08
	asl
	adc #tables.lengths
	sta $04
	lda $0A
	ldx #trees.dynamic_dtree
	%build_tree()
	jmp inflate_block
		
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
