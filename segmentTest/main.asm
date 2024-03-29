;
; segmentTest.asm
;
; Created: 1/11/2024 9:09:10 PM
; Author : bwen6
;


.dseg
	; digit 1 is the leftmost
	digit1: .byte 1
	digit2: .byte 1
	digit3: .byte 1
	digit4: .byte 1

	; char 1 corresponds with digit 1 (leftmost)
	char1: .byte 1
	char2: .byte 1
	char3: .byte 1
	char4: .byte 1

	;length of text
	text_len: .byte	1
	current_letter_begin: .byte 1
	current_letter: .byte 1

.cseg
RESET:
	rjmp start

.org PORTE_PORT_vect
	rjmp porte_isr

start:
	; Configure digit addresses
	ldi ZL, LOW(digit1)
	ldi ZH, HIGH(digit1)
	ldi r16, 0x0E
	st Z+, r16
	ldi r16, 0x16
	st Z+, r16
	ldi r16, 0x1A
	st Z+, r16
	ldi r16, 0x1C
	st Z+, r16


	; Enable LEDs bjt and common anode
	sbi VPORTC_DIR, 0
	cbi VPORTC_OUT, 0

	; Enable digit bjt's

	ldi r16, 0x1E			; set direction of PORTC (Digit select)
	out VPORTC_DIR, r16

	ldi r16, 0xFF			; set direction of PORTA (LEDs)
	sts PORTA_DIR, r16

	ldi r16, 0xFF			; LEDs initially OFF
	out VPORTA_OUT, r16

	; Enable switch as input
	cbi VPORTE_DIR, 0
	cbi VPORTE_DIR, 2

	lds r16, PORTE_PIN0CTRL		;set ISC for PE0 to falling edge and enable pullup
	ori r16, 0x03 | PORT_PULLUPEN_bm
	sts PORTE_PIN0CTRL, r16
	
	lds r16, PORTE_PIN2CTRL		;set ISC for PE2 to falling edge and enable pullup
	ori r16, 0x03 | PORT_PULLUPEN_bm
	sts PORTE_PIN2CTRL, r16

	sei				;enable global interrupts

	rcall shifting_text_init

main:
	nop
	nop
	rcall display_text
	rjmp main

porte_isr:
	cli

	in r16, CPU_SREG
	push r16

	; change implementation on how button is checked
	;in r16, VPORTE_IN
    ;andi r16, (1 << 0)  ; Mask only the bit corresponding to the button
    ;cpi r16, 0

	sbis VPORTE_IN, 0
    rjmp button_pressed_up

	sbis VPORTE_IN, 2
    rjmp button_pressed_down

	button_released_up:
    ; Handle button release
		;rcall delay
		sbis VPORTE_IN, 0 ;check for 1 (indicating that the button was let go)
		rjmp button_released_up

		pop r16
		out CPU_SREG, r16

		lds r16, PORTE_INTFLAGS
		ori r16, PORT_INT0_bm
		sts PORTE_INTFLAGS, r16

		;ldi r16, PORT_INT0_bm ;clear IRQ flag for PE0
		;sts PORTE_INTFLAGS, r16

		sei
		reti

	button_pressed_up:
    ; Debounce by waiting for a short delay or checking the stable state
    ; (You may use a timer or a loop with a delay)
    ; Proceed with button handling after debounce
		wait_for_0:
			sbic VPORTE_IN, 0 ;wait for PE0 being 0
			rjmp wait_for_0
			rcall delay ;delay when you see a 0

			sbic VPORTE_INTFLAGS, 0		;check if PE0 (UP pushbutton) is pressed
			rcall shift_buffer_left

			; REMEMBER TO UNCOMMENT THIS TO TEST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			;sbic VPORTE_INTFLAGS, 2		;check if PE2 (DOWN pushbutton) is pressed
			;rcall shift_buffer_right

			rjmp button_released_up


	button_released_down:
    ; Handle button release
		;rcall delay
		sbis VPORTE_IN, 2 ;check for 1 (indicating that the button was let go)
		rjmp button_released_up

		pop r16
		out CPU_SREG, r16

		lds r16, PORTE_INTFLAGS
		ori r16, PORT_INT2_bm
		sts PORTE_INTFLAGS, r16

		sei
		reti

	button_pressed_down:
    ; Debounce by waiting for a short delay or checking the stable state
    ; (You may use a timer or a loop with a delay)
    ; Proceed with button handling after debounce
		wait_for_0_down:
			sbic VPORTE_IN, 2 ;wait for PE2 being 0
			rjmp wait_for_0_down
			rcall delay ;delay when you see a 0

			; REMEMBER TO UNCOMMENT THIS TO TEST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			sbic VPORTE_INTFLAGS, 2		;check if PE2 (DOWN pushbutton) is pressed
			rcall shift_buffer_right

			rjmp button_released_down


	;ldi r16, PORT_INT0_bm ;clear IRQ flag for PE0
	;sts PORTE_INTFLAGS, r16

	lds r16, PORTE_INTFLAGS		;clear IRQ flag for PE0
	ori r16, PORT_INT0_bm
	sts PORTE_INTFLAGS, r16

	lds r16, PORTE_INTFLAGS		;clear IRQ flag for PE2
	ori r16, PORT_INT2_bm
	sts PORTE_INTFLAGS, r16

	sei
	reti

shifting_text_init:
	;init text length
	ldi ZL, LOW(text<<1)	; Z points to the text we want to input
	ldi ZH, HIGH(text<<1)
	
	; reading how long the text is
	ldi r16, 0
	not_EOL:
		lpm r17, Z+
		inc r16
		cpi r17, '/'
		brne not_EOL

	dec r16			; to fix to proper text length
	sts text_len, r16

	;init current letter positions
	ldi r16, 1
	sts current_letter_begin, r16
	ldi r16, 4
	sts current_letter, r16

	;initialize the buffer
	ldi ZL, LOW(text<<1)	; Z points to the text we want to input
	ldi ZH, HIGH(text<<1)

	ldi YL, LOW(char1)		; Y points to the buffer
	ldi YH, HIGH(char1)
	
	ldi r17, 4
	init_loop:				; we put the first 4 chars in the buffer
		lpm r16, Z+
		st Y+, r16
		dec r17
		brne init_loop

	rcall text2bin_all
	ret

shift_buffer_left:
	cli
	push ZL
	push ZH
	push YL
	push YH
	push r19
	push r18
	push r17
	push r16
	in r16, CPU_SREG
	push r16

	ldi ZL, LOW(text<<1)
	ldi ZH, HIGH(text<<1)
	lds r18, current_letter
	ldi r17, $00
	add ZL, r18
	adc ZH, r17
	
	ldi YL, LOW(char1)	;shift the char variables
	ldi YH, HIGH(char1)

	ldi r17, 3
	shift_loop:
		ldd r16, Y+1
		st Y+, r16
		dec r17
		brne shift_loop

	lds r18, text_len
	lds r19, current_letter
	cp r19, r18		
	brsh rollover

	shift:
		inc r19						;increment letter position
		sts current_letter, r19
		lpm r16, Z+
		st Y, r16
		rcall text2bin_last_char

	pop r16
	out CPU_SREG, r16
	pop r16
	pop r17
	pop r18
	pop r19
	pop YH
	pop YL
	pop ZH
	pop ZL

	sei
	ret

	rollover:
		ldi ZL, LOW(text<<1)	;reset Z pointer if same length of text
		ldi ZH, HIGH(text<<1)
		lpm r16, Z
		st Y, r16
		

		ldi r19, 1
		sts current_letter, r19

		rcall text2bin_last_char

	pop r16
	out CPU_SREG, r16
	pop r16
	pop r17
	pop r18
	pop r19
	pop YH
	pop YL
	pop ZH
	pop ZL

	ret


shift_buffer_right:
	cli
	push ZL
	push ZH
	push YL
	push YH
	push r19
	push r18
	push r17
	push r16
	in r16, CPU_SREG
	push r16

	ldi ZL, LOW(text<<1)
	ldi ZH, HIGH(text<<1)
	lds r18, current_letter_begin
	ldi r17, $00
	add ZL, r18
	adc ZH, r17
	
	ldi YL, LOW(char1 + 3)	;shift the char variables
	ldi YH, HIGH(char1 + 3)

	ldi r17, 3
	shift_loop_down:
		ld r16, -Y
		std Y+1, r16
		dec r17
		brne shift_loop_down

	ldi r18, 1			
	lds r19, current_letter_begin
	cp r18, r19	
	brlo rollunder

	shift_down:
		dec r19						;decrement letter position
		sts current_letter_begin, r19
		lpm r16, Z
		ldi r17, $00
		subi ZL, 1
		sbc ZH, r17
		st Y, r16
		rcall text2bin_last_char

	pop r16
	out CPU_SREG, r16
	pop r16
	pop r17
	pop r18
	pop r19
	pop YH
	pop YL
	pop ZH
	pop ZL

	sei
	ret

	rollunder:
		ldi ZL, LOW(text<<1)	;reset Z pointer if reaches 0 to end of line
		ldi ZH, HIGH(text<<1)

		ldi r16, $00
		lds r17, text_len
		dec r17
		add ZL, r17
		adc ZH, r16	

		lpm r16, Z
		st Y, r16
		

		ldi r19, 1
		sts current_letter_begin, r19

		rcall text2bin_last_char

	pop r16
	out CPU_SREG, r16
	pop r16
	pop r17
	pop r18
	pop r19
	pop YH
	pop YL
	pop ZH
	pop ZL

	sei
	ret


/*	
again:
	ldi ZL, LOW(numbers<<1)
	ldi ZH, HIGH(numbers<<1)

	ldi r17, 36
	inner:
		lpm r16, Z+
		sts PORTA_OUT, r16
		rcall delay
		rcall delay
		rcall delay
		rcall delay
		dec r17
		brne inner

		rjmp again
		*/

text2bin_last_char:
	push r16
	lds r16, char4
	subi r16, 'A'
	sts char4, r16
	pop r16
	ret

text2bin_all:
	push r16

	lds r16, char4
	subi r16, 'A'
	sts char4, r16

	lds r16, char3
	subi r16, 'A'
	sts char3, r16

	lds r16, char2
	subi r16, 'A'
	sts char2, r16

	lds r16, char1
	subi r16, 'A'
	sts char1, r16

	pop r16
	ret

;put your character offsets into char1 - char4
display_text:
	push r20
	push r19
	in r19, CPU_SREG
	push r19
	push r18
	push r17
	push r16
	push ZL
	push ZH

	lds r19, digit4				; select digit
	out VPORTC_OUT, r19

	ldi ZL, LOW(letters<<1)		; set pointer to table beginning
	ldi ZH, HIGH(letters<<1)

	ldi r17, $00
	lds r18, char4				; load offset
	add ZL, r18
	adc ZH, r17					; map to right char
	lpm r16, Z
	sts PORTA_OUT, r16			; display letter

	rcall turn_off_digit		; turn digit OFF

	lds r19, digit3
	out VPORTC_OUT, r19

	ldi ZL, LOW(letters<<1)
	ldi ZH, HIGH(letters<<1)

	lds r18, char3	
	add ZL, r18
	adc ZH, r17
	lpm r16, Z
	sts PORTA_OUT, r16

	rcall turn_off_digit

	lds r19, digit2
	out VPORTC_OUT, r19

	ldi ZL, LOW(letters<<1)
	ldi ZH, HIGH(letters<<1)

	lds r18, char2
	add ZL, r18
	adc ZH, r17
	lpm r16, Z
	sts PORTA_OUT, r16

	rcall turn_off_digit

	lds r19, digit1
	out VPORTC_OUT, r19

	ldi ZL, LOW(letters<<1)
	ldi ZH, HIGH(letters<<1)

	lds r18, char1
	add ZL, r18
	adc ZH, r17
	lpm r16, Z
	sts PORTA_OUT, r16
	
	rcall turn_off_digit

	pop ZH
	pop ZL
	pop r16
	pop r17
	pop r18
	pop r19
	out CPU_SREG, r19
	pop r19
	pop r20

	ret

turn_off_digit:
	push r16
	ldi r16, 0xFF
	out VPORTA_OUT, r16
	pop r16
	ret

delay:
	push r18
	in r18, CPU_SREG
	push r18
	push r17

	ldi r18, 0
delayouter:
	ldi r17, 0
delayinner:
	dec r17
	brne delayinner
	dec r18
	brne delayouter

	pop r17
	pop r18
	out CPU_SREG, r18
	pop r18
	ret


; numbers is the segment values for 0-9
; letters are the segment values for A-Z
; text is the actual text you want to display and uses '/' as end of line
numbers: .db $03,$D7,$61,$C1,$95,$89,$09,$D3,$01,$91
letters: .db $11,$0D,$2B,$45,$29,$39,$0B,$15,$D7,$CB,$19,$2F,$59,$5D,$4D,$31,$91,$7D,$89,$2D,$4F,$A7,$A5,$5F,$85,$63
text: .db 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','/'