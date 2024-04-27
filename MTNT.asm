   
; ***************************************************************************
;   LCD-AVR-8bit.asm  - Use an HD44780U based LCD with an Atmel ATmega processor
;
;   
; ***************************************************************************
;        File:    LCD-AVR-8bit.asm
;        Date:    March 9, 2022
;
;      Target:    ATmega32
;   Assembler:    Atmel AvrAssembler2 (AVR Studio 6)
;      Author:    Donald Weiman
;
;     Summary:    8-bit data interface, busy flag not implemented.
;                 Any LCD pin can be connected to any available I/O port.
;                 Includes a simple write string routine.
;
; ****************************** Program Notes ******************************
;
;           This program uses an 8-bit data interface but does not use the
;             busy flag to determine when the LCD controller is ready.  The
;             LCD RW line (pin 5) is not connected to the uP and it must be
;             connected to GND for the program to function.
;
;           All time delays are longer than those specified in most datasheets
;             in order to accommodate slower than normal LCD modules.  This
;             requirement is well documented but almost always ignored.  The
;             information is in a note at the bottom of the right hand
;             (Execution Time) column of the instruction set.
;
; ***************************************************************************
;
;           The eight data lines as well as the two control lines may be
;             implemented on any available I/O pin of any port.  These are
;             the connections used for this program:
;
;                -----------                   ----------
;               | ATmega32  |                 |   LCD    |
;               |           |                 |          |
;               |        PB7|---------------->|D7        |
;               |        PB6|---------------->|D6        |
;               |        PB5|---------------->|D5        |
;               |        PB4|---------------->|D4        |
;               |        PB3|---------------->|D3        |
;               |        PB2|---------------->|D2        |
;               |        PB1|---------------->|D1        |
;               |        PB0|---------------->|D0        |
;               |           |                 |          |
;               |        PA1|---------------->|E         |
;               |           |         GND --->|RW        |
;               |        PA0|---------------->|RS        |
;                -----------                   ----------
;
; ***************************************************************************


#include "m32def.inc"


.equ    fclk                = 8000000      ; system clock frequency (for delays)

; register usage
.def    temp                = R16           ; temporary storage
.def 	PW_count		    = R19
.def 	mode 				= R22
.def 	flag 				= R23
; LCD interface (should agree with the diagram above)
;   make sure that the LCD RW pin is connected to GND
.equ    lcd_D7_port         = PORTB         ; lcd D7 connection
.equ    lcd_D7_bit          = PORTB7
.equ    lcd_D7_ddr          = DDRB

.equ    lcd_D6_port         = PORTB         ; lcd D6 connection
.equ    lcd_D6_bit          = PORTB6
.equ    lcd_D6_ddr          = DDRB

.equ    lcd_D5_port         = PORTB         ; lcd D5 connection
.equ    lcd_D5_bit          = PORTB5
.equ    lcd_D5_ddr          = DDRB

.equ    lcd_D4_port         = PORTB         ; lcd D4 connection
.equ    lcd_D4_bit          = PORTB4
.equ    lcd_D4_ddr          = DDRB

.equ    lcd_D3_port         = PORTB         ; lcd D3 connection
.equ    lcd_D3_bit          = PORTB3
.equ    lcd_D3_ddr          = DDRB

.equ    lcd_D2_port         = PORTB         ; lcd D2 connection
.equ    lcd_D2_bit          = PORTB2
.equ    lcd_D2_ddr          = DDRB

.equ    lcd_D1_port         = PORTB         ; lcd D1 connection
.equ    lcd_D1_bit          = PORTB1
.equ    lcd_D1_ddr          = DDRB

.equ    lcd_D0_port         = PORTB         ; lcd D0 connection
.equ    lcd_D0_bit          = PORTB0
.equ    lcd_D0_ddr          = DDRB

.equ    lcd_E_port          = PORTA         ; lcd Enable pin
.equ    lcd_E_bit           = PORTA2
.equ    lcd_E_ddr           = DDRA

.equ    lcd_RS_port         = PORTA         ; lcd Register Select pin
.equ    lcd_RS_bit          = PORTA0
.equ    lcd_RS_ddr          = DDRA

; LCD module information
.equ    lcd_LineOne         = 0x00          ; start of line 1
.equ    lcd_LineTwo         = 0x40          ; start of line 2
;.equ   lcd_LineThree        = 0x14          ; start of line 3 (20x4)
;.equ   lcd_lineFour         = 0x54          ; start of line 4 (20x4)
;.equ   lcd_LineThree        = 0x10          ; start of line 3 (16x4)
;.equ   lcd_lineFour         = 0x50          ; start of line 4 (16x4)

; LCD instructions
.equ    lcd_Clear           = 0b00000001    ; replace all characters with ASCII 'space'
.equ    lcd_Home            = 0b00000010    ; return cursor to first position on first line
.equ    lcd_EntryMode       = 0b00000110    ; shift cursor from left to right on read/write
.equ    lcd_DisplayOff      = 0b00001000    ; turn display off
.equ    lcd_DisplayOn       = 0b00001100    ; display on, cursor off, don't blink character
.equ    lcd_FunctionReset   = 0b00110000    ; reset the LCD
.equ    lcd_FunctionSet8bit = 0b00111000    ; 8-bit data, 2-line display, 5 x 7 font
.equ    lcd_SetCursor       = 0b10000000    ; set cursor position

; ****************************** Reset Vector *******************************
.dseg
.org 0x100
current_password: .byte 10

.cseg
.org    0x0000
    jmp     start                           ; jump over Interrupt Vectors, Program ID etc.

.org 0x0002
	 rjmp EXT_INT0
.org 0x0004
	rjmp EXT_INT1

.org    0x30

program_author:
.db         " DOOR LOCK",0

program_version:
.db         "PW:",0,0

wrong:
.db			" WRONG PASSWORD",0
right:
.db			" RIGHT PASSWORD",0
right2:
.db			"  DOOR OPENED",0
new_PW:
.db			" ENTER NEW PASS",0
PW_changed:
.db			" PASSWORD CHANGED",0
set_password:
.db 0
close:
.db			" DOOR NOW CLOSED",0
pass_check:
.db '6',0

preset_password: 
.db '1','2','3','4','5','6',0

; ****************************** Main Program Code **************************
start:
; initialize the stack pointer to the highest RAM address

    ldi     temp,low(RAMEND)
    out     SPL,temp
    ldi     temp,high(RAMEND)
    out     SPH,temp

	LDI  R16,0x00

	OUT  DDRC,R16  ; config PORTC as input
	sbi DDRD, 7    ; config PORTD pin 7 as output
	cbi PORTD, 7	;clear PD7 to 0
	

	sbi DDRD, 3
	sbi DDRD, 4

	; Set INT0 Rising Edge triger, INT 1 falling edge

	LDI R16,0b00001011;  MCUCR = 0b00001011; 
	OUT MCUCR,R16
	LDI R16, 0x00
	OUT MCUCSR, R16		//INT 2 falling edge
	LDI R16,(1<<INT0)|(1<<INT1)|(1<<INT0) ;  enable INT0
    OUT GICR,R16 
    sei ;   set global enable interrupt


; configure the microprocessor pins for the data lines
    sbi     lcd_D7_ddr, lcd_D7_bit          ; 8 data lines - output
    sbi     lcd_D6_ddr, lcd_D6_bit
    sbi     lcd_D5_ddr, lcd_D5_bit
    sbi     lcd_D4_ddr, lcd_D4_bit
    sbi     lcd_D3_ddr, lcd_D3_bit
    sbi     lcd_D2_ddr, lcd_D2_bit
    sbi     lcd_D1_ddr, lcd_D1_bit
    sbi     lcd_D0_ddr, lcd_D0_bit

; configure the microprocessor pins for the control lines
    sbi     lcd_E_ddr,  lcd_E_bit           ; E line - output
    sbi     lcd_RS_ddr, lcd_RS_bit          ; RS line - output


; initialize the LCD controller as determined by the equates (LCD instructions)
   call    lcd_init_8d                     ; initialize the LCD display for an 8-bit interface
; display the first line of information
    ldi     ZH, high(program_author)        ; point to the information that is to be displayed
    ldi     ZL, low(program_author)
    ldi     temp, lcd_LineOne               ; point to where the information should be displayed
    call    lcd_write_string_8d


; display the second line of information
    ldi     ZH, high(program_version)       ; point to the information that is to be displayed
    ldi     ZL, low(program_version)
    ldi     temp, lcd_LineTwo               ; point to where the information should be displayed
    call    lcd_write_string_8d

; endless loop
here:
	;------------------------------------------------------------------------------------------------------------
	PUSH R16
	PUSH R18									;write PRESET pass word to EEPROM
	PUSH R17
	PUSH R30
	PUSH R31


;check initial password condition
		wait_check:
			SBIC EECR, EEWE
			rjmp wait_check
			LDI  XL, 0x05
			LDI  XH, 0x03
			OUT  EEARL, XL
			OUT  EEARH, XH
			SBI EECR, EERE
			IN R18, EEDR
;load sample to check
			LDI	 ZL, LOW(pass_check<<1)
			LDI  ZH, HIGH(pass_check<<1)
			LPM R17, Z+
			CP R18, R17
			BRNE no_preset
			BREQ preset_done

no_preset:
	LDI	 ZL, LOW(preset_password<<1)
	LDI  ZH, HIGH(preset_password<<1)
	LDI  XL, 0x00
	LDI  XH, 0x03
wait_w1:
	SBIC EECR, EEWE
	rjmp wait_w1
	
	OUT  EEARL, XL
	OUT  EEARH, XH
	
	
		LPM R17, Z+
		OUT EEDR, R17
		SBI EECR, EEMWE
		SBI EECR, EEWE
		INC XL
		CPI R17, 0
		BRNE wait_w1
preset_done:
	POP R31
	POP R30
	POP R17
	POP R18
	POP R16
;--------------------------------------------------------------------------------------------------------------------------------------------------

	SBI PORTD,3
	LDI mode, 0
 ;mode 1: enter and check password
//while_true:
WHILE:
	
	LDI PW_count,0
    call enter_paswword_fn
	call check_password_function
	call clear_all
	//PW RIGHT / TUR NON MODE
	cpi flag, 1
	brne while
	cpi mode,1
	brne while
	breq change_password_mode

    ldi PW_count, 0
	call DELAY
	call DELAY
;-------------------------------------- change password mode -------------------------------------
change_password_mode:
	ldi     ZH, high(new_PW)        ; point to the information that is to be displayed
    ldi     ZL, low(new_PW)
    ldi     temp, lcd_LineOne               ; point to where the information should be displayed
    call    lcd_write_string_8d

	ldi     ZH, high(program_version)       ; point to the information that is to be displayed
    ldi     ZL, low(program_version)
    ldi     temp, lcd_LineTwo               ; point to where the information should be displayed
    call    lcd_write_string_8d

	call 	enter_paswword_fn1
	;--------------------------------------------------------------------------------------------------------
	LDI	 YL, LOW(current_password<<1)
	LDI  YH, HIGH(current_password<<1)
	LDI  XL, 0x00
	LDI  XH, 0x03
	LDI PW_count, 5
wait_w:
	SBIC EECR, EEWE
	rjmp wait_w
	
	OUT  EEARL, XL
	OUT  EEARH, XH
	
	
		LD R17, Y+
		OUT EEDR, R17
		SBI EECR, EEMWE
		SBI EECR, EEWE
		INC XL
		DEC PW_count
		CPI PW_count, 0
		BRNE wait_w
	
	LDI		mode, 0
	ldi     ZH, high(PW_changed)        ; point to the information that is to be displayed
    ldi     ZL, low(PW_changed)
    ldi     temp, lcd_LineOne               ; point to where the information should be displayed
    call    lcd_write_string_8d
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY
	CALL DELAY

	call clear_all
	ldi mode,0
	rjmp while
;-----------------------------------------enter password function------------------------
enter_paswword_fn:
; display the first line of information
    ldi     ZH, high(program_author)        ; point to the information that is to be displayed
    ldi     ZL, low(program_author)
    ldi     temp, lcd_LineOne               ; point to where the information should be displayed
    call    lcd_write_string_8d
; display the second line of information
    ldi     ZH, high(program_version)       ; point to the information that is to be displayed
    ldi     ZL, low(program_version)
    ldi     temp, lcd_LineTwo               ; point to where the information should be displayed
    call    lcd_write_string_8d
enter_paswword_fn1:
	LDI XL, LOW(current_password << 1)
	LDI XH, HIGH(current_password <<1)
LOOP:
	cpi PW_count, 5
	brne LOOP
	LDI PW_count, 0
	call my_LCD_clear
	ret
;------------------------------------------------------------ key pad ------------------------------------------------------------------------------------
EXT_INT0:
	push r17
	push r18
	IN R17, PINC
	CPI R17,0
	BRNE check_1
		LDI R18, 0x37 ;convert to '7'
		rjmp end_check
	
check_1:
	CPI R17, 1 ;Key = 0
	BRNE check_2
		LDI R18, 0x34 ;convert to '4'
		rjmp end_check
check_2: 
	cpi R17, 2 ;Key = 1
	BRNE check_4
		LDI R18, 0x31 ;convert to '1'
		rjmp end_check

	
check_4: 
	cpi R17, 4  
	brne check_5
		ldi r18, 0x38 ;convert to '8'
		rjmp end_check
check_5: 
	cpi r17, 5;
	brne check_6
		ldi r18, 0x35 ;convert to '5'
		rjmp end_check
check_6: 
	cpi r17, 6;
	brne check_7
		ldi r18, 0x32	;convert to '2'
		rjmp end_check
check_7:
	cpi r17, 7;
	brne check_8
	ldi r18, 0x30
	rjmp end_check
check_8: 
	cpi r17, 8;
	brne check_9
		ldi r18, 0x39
		rjmp end_check
check_9:
	cpi r17, 9;
	brne check_10
	ldi r18, 0x36
	rjmp end_check
check_10:
	cpi r17, 10;
	ldi r18, 0x33
	rjmp end_check



end_check:   
	MOV temp,R18 
	call lcd_write_character_8d
	ST X+, R18
	inc PW_count
	pop r18
	pop r17
    RETI



;----------------------------------------------------------------------------------------------
;									Check password
	
check_password_function:

	PUSH R16
	PUSH R17
	PUSH R26
	PUSH R27
	PUSH R30
	PUSH R31

	CALL read_data_from_EEPROM
	LDI XL, LOW(set_password<<1)
	LDI XH, HIGH(set_password<<1)

	LDI YL, LOW(current_password<<1)
	LDI YH, HIGH(current_password<<1)

	LDI PW_count, 5
check:
	LD	R16,X+
	LD  R17, Y+
	CP R16,R17
	BRNE end_compare
	DEC PW_count
	BRNE check
	; display the first line of information
   ldi     ZH, high(right)        ; point to the information that is to be displayed
    ldi     ZL, low(right)
    ldi     temp, lcd_LineOne               ; point to where the information should be displayed
    call    lcd_write_string_8d
	ldi R16, 6
	STS 0xF3, R16
	CPI mode, 1
	BREQ mode1
	BRNE mode0
mode0: 	
		SBI PORTD, 7
mode0_1:
	 ldi     ZH, high(right2)        ; point to the information that is to be displayed
    ldi     ZL, low(right2)
    ldi     temp, lcd_LineTwo               ; point to where the information should be displayed
    call    lcd_write_string_8d

	SBIS PORTD, 7
	JMP mode1
	JMP mode0_1
				
		

mode1:			
				LDI flag, 1
				POP R31
				POP R30
				POP R27
				POP R26
				POP R17
				POP R16
				ret

end_compare:
    ldi     ZH, high(wrong)       ; point to the information that is to be displayed
    ldi     ZL, low(wrong)
    ldi     temp, lcd_LineOne               ; point to where the information should be displayed
    call    lcd_write_string_8d	
	call DELAY

;----------------------------------------------------------------------------------------------
;									Read data from EEPROM
read_data_from_EEPROM:
	LDI ZL, LOW(set_password << 1)
	LDI ZH, HIGH(set_password << 1)

	LDI XL, 0x00
	LDI XH, 0x03
wait_r:
	SBIC EECR, EEWE
	rjmp wait_r

	OUT EEARL, XL
	OUT EEARH, XH
	SBI EECR, EERE
	IN R16, EEDR
	CPI R16, 0xFF
	BREQ end_reading
	ST 	Z+, R16
	INC XL

	rjmp wait_r
end_reading: 
	ret

DELAY:		
	LDI r21, 255	; load r19 = 64
DL1: 
	Ldi r22, 60	; load r20 = 100
DL2: 
	Ldi r23, 255	; load r21 = 48
DL3:
	dec r23			; 
	brne DL3		
	dec r22
	brne DL2
	dec r21
	brne DL1
	RET

EXT_INT1:
	SBIS PORTD, 7
	JMP intter
	JMP close_door

intter:
			LDI mode, 1
			reti
close_door:
	call my_LCD_clear
	ldi     ZH, high(close)        ; point to the information that is to be displayed
    ldi     ZL, low(close)
    ldi     temp, lcd_LineOne  
	call    lcd_write_string_8d
	call DELAY
	cbi PORTD, 7
	reti
; ****************************** End of Main Program Code *******************

; ============================== 8-bit LCD Subroutines ======================
; Name:     lcd_init_8d
; Purpose:  initialize the LCD module for a 8-bit data interface
; Entry:    equates (LCD instructions) set up for the desired operation
; Exit:     no parameters
; Notes:    uses time delays instead of checking the busy flag

my_lcd_clear:
; Power-up delay
    ldi     temp, 100                       ; initial 40 mSec delay
    call    delayTx1mS
	call    delayTx1mS

; Reset the LCD controller.
    ldi     temp, lcd_Clear       ; first part of reset sequence
    call    lcd_write_instruction_8d
	ret

clear_all:
; Power-up delay
    ldi     temp, lcd_FunctionReset                       ; initial 40 mSec delay
    call    delayTx1mS
	call    delayTx1mS

; Reset the LCD controller.
    ldi     temp, lcd_Clear       ; first part of reset sequence
    call    lcd_write_instruction_8d
	ret

lcd_init_8d:
; Power-up delay
    ldi     temp, 100                       ; initial 40 mSec delay
    call    delayTx1mS
	call    delayTx1mS

; Reset the LCD controller.
    ldi     temp, lcd_FunctionReset         ; first part of reset sequence
    call    lcd_write_instruction_8d
    ldi     temp, 10                        ; 4.1 mS delay (min)
    call    delayTx1mS
	call    delayTx1mS


    ldi     temp, lcd_FunctionReset         ; second part of reset sequence
    call    lcd_write_instruction_8d
    ldi     temp, 200                       ; 100 uS delay (min)
    call    delayTx1uS
	call    delayTx1uS

    ldi     temp, lcd_FunctionReset         ; third part of reset sequence
    call    lcd_write_instruction_8d
    ldi     temp, 200                       ; this delay is omitted in the data sheet
    call    delayTx1uS
	call    delayTx1uS

; Function Set instruction
    ldi     temp, lcd_FunctionSet8bit       ; set mode, lines, and font
    call    lcd_write_instruction_8d
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
	call    delayTx1uS
; The next three instructions are specified in the data sheet as part of the initialization routine,
;   so it is a good idea (but probably not necessary) to do them just as specified and then redo them
;   later if the application requires a different configuration.

; Display On/Off Control instruction
    ldi     temp, lcd_DisplayOff            ; turn display OFF
    call    lcd_write_instruction_8d
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
	call    delayTx1uS

; Clear Display instruction
    ldi     temp, lcd_Clear                 ; clear display RAM
    call    lcd_write_instruction_8d
    ldi     temp, 4                         ; 1.64 mS delay (min)
    call    delayTx1mS
    call    delayTx1mS

; Entry Mode Set instruction
    ldi     temp, lcd_EntryMode             ; set desired shift characteristics
    call    lcd_write_instruction_8d
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
	call    delayTx1uS

; This is the end of the LCD controller initialization as specified in the data sheet, but the display
;   has been left in the OFF condition.  This is a good time to turn the display back ON.

; Display On/Off Control instruction
    ldi     temp, lcd_DisplayOn             ; turn the display ON
    call    lcd_write_instruction_8d
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
	call    delayTx1uS
    ret

; ---------------------------------------------------------------------------
; Name:     lcd_write_string_8d
; Purpose:  display a string of characters on the LCD
; Entry:    ZH and ZL pointing to the start of the string
;           (temp) contains the desired DDRAM address at which to start the display
; Exit:     no parameters
; Notes:    the string must end with a null (0)
;           uses time delays instead of checking the busy flag

lcd_write_string_8d:
; preserve registers
    push    ZH                              ; preserve pointer registers
    push    ZL

; fix up the pointers for use with the 'lpm' instruction
    lsl     ZL                              ; shift the pointer one bit left for the lpm instruction
    rol     ZH

; set up the initial DDRAM address
    ori     temp, lcd_SetCursor             ; convert the plain address to a set cursor instruction
    call   lcd_write_instruction_8d         ; set up the first DDRAM address
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
	call    delayTx1uS

; write the string of characters
lcd_write_string_8d_01:
    lpm     temp, Z+                        ; get a character
    cpi     temp,  0                        ; check for end of string
    breq    lcd_write_string_8d_02          ; done

; arrive here if this is a valid character
    call    lcd_write_character_8d          ; display the character
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
	call    delayTx1uS
    rjmp    lcd_write_string_8d_01          ; not done, send another character

; arrive here when all characters in the message have been sent to the LCD module
lcd_write_string_8d_02:
    pop     ZL                              ; restore pointer registers
    pop     ZH
    ret

; ---------------------------------------------------------------------------
; Name:     lcd_write_character_8d
; Purpose:  send a byte of information to the LCD data register
; Entry:    (temp) contains the data byte
; Exit:     no parameters
; Notes:    does not deal with RW (busy flag is not implemented)

lcd_write_character_8d:
    sbi     lcd_RS_port, lcd_RS_bit         ; select the Data Register (RS high)
    cbi     lcd_E_port, lcd_E_bit           ; make sure E is initially low
    call    lcd_write_8                     ; write the data
    ret

; ---------------------------------------------------------------------------
; Name:     lcd_write_instruction_8d
; Purpose:  send a byte of information to the LCD instruction register
; Entry:    (temp) contains the data byte
; Exit:     no parameters
; Notes:    does not deal with RW (busy flag is not implemented)

lcd_write_instruction_8d:
    cbi     lcd_RS_port, lcd_RS_bit         ; select the Instruction Register (RS low)
    cbi     lcd_E_port, lcd_E_bit           ; make sure E is initially low
    call    lcd_write_8                     ; write the instruction
    ret

; ---------------------------------------------------------------------------
; Name:     lcd_write_8
; Purpose:  send a byte of information to the LCD module
; Entry:    (temp) contains the data byte
;           RS is configured for the desired LCD register
;           E is low
;           RW is low
; Exit:     no parameters
; Notes:    use either time delays or the busy flag

lcd_write_8:
; set up the data bits
    sbi     lcd_D7_port, lcd_D7_bit         ; assume that the data bit is '1'
    sbrs    temp, 7                         ; check the actual data value
    cbi     lcd_D7_port, lcd_D7_bit         ; arrive here only if the data was actually '0'

    sbi     lcd_D6_port, lcd_D6_bit         ; repeat for each data bit
    sbrs    temp, 6
    cbi     lcd_D6_port, lcd_D6_bit

    sbi     lcd_D5_port, lcd_D5_bit
    sbrs    temp, 5
    cbi     lcd_D5_port, lcd_D5_bit

    sbi     lcd_D4_port, lcd_D4_bit
    sbrs    temp, 4
    cbi     lcd_D4_port, lcd_D4_bit

    sbi     lcd_D3_port, lcd_D3_bit
    sbrs    temp, 3
    cbi     lcd_D3_port, lcd_D3_bit

    sbi     lcd_D2_port, lcd_D2_bit
    sbrs    temp, 2
    cbi     lcd_D2_port, lcd_D2_bit

    sbi     lcd_D1_port, lcd_D1_bit
    sbrs    temp, 1
    cbi     lcd_D1_port, lcd_D1_bit

    sbi     lcd_D0_port, lcd_D0_bit
    sbrs    temp, 0
    cbi     lcd_D0_port, lcd_D0_bit

; write the data
                                            ; 'Address set-up time' (40 nS)
    sbi     lcd_E_port, lcd_E_bit           ; Enable pin high
    call    delay1uS                        ; implement 'Data set-up time' (80 nS) and 'Enable pulse width' (230 nS)
    cbi     lcd_E_port, lcd_E_bit           ; Enable pin low
    call    delay1uS   
	call    delayTx1uS                     ; implement 'Data hold time' (10 nS) and 'Enable cycle time' (500 nS)
    ret

; ============================== End of 8-bit LCD Subroutines ===============

; ============================== Time Delay Subroutines =====================
; Name:     delayYx1mS
; Purpose:  provide a delay of (YH:YL) x 1 mS
; Entry:    (YH:YL) = delay data
; Exit:     no parameters
; Notes:    the 16-bit register provides for a delay of up to 65.535 Seconds
;           requires delay1mS

delayYx1mS:
    call    delay1mS                        ; delay for 1 mS
    dec     YL                        ; update the the delay counter
    brne    delayYx1mS                      ; counter is not zero

; arrive here when delay counter is zero (total delay period is finished)
    ret

; ---------------------------------------------------------------------------
; Name:     delayTx1mS
; Purpose:  provide a delay of (temp) x 1 mS
; Entry:    (temp) = delay data
; Exit:     no parameters
; Notes:    the 8-bit register provides for a delay of up to 255 mS
;           requires delay1mS

delayTx1mS:
    call    delay1mS                        ; delay for 1 mS
    dec     temp                            ; update the delay counter
    brne    delayTx1mS                      ; counter is not zero

; arrive here when delay counter is zero (total delay period is finished)
    ret

; ---------------------------------------------------------------------------
; Name:     delay1mS
; Purpose:  provide a delay of 1 mS
; Entry:    no parameters
; Exit:     no parameters
; Notes:    chews up fclk/1000 clock cycles (including the 'call')

delay1mS:
    push    YL                              ; [2] preserve registers
    push    YH                              ; [2]
    ldi     YL, low (((fclk/1000)-18)/4)    ; [1] delay counter
    ldi     YH, high(((fclk/1000)-18)/4)    ; [1]

delay1mS_01:
    dec     YL                        ; [2] update the the delay counter
    brne    delay1mS_01                     ; [2] delay counter is not zero

; arrive here when delay counter is zero
    pop     YH                              ; [2] restore registers
    pop     YL                              ; [2]
    ret                                     ; [4]

; ---------------------------------------------------------------------------
; Name:     delayTx1uS
; Purpose:  provide a delay of (temp) x 1 uS with a 16 MHz clock frequency
; Entry:    (temp) = delay data
; Exit:     no parameters
; Notes:    the 8-bit register provides for a delay of up to 255 uS
;           requires delay1uS

delayTx1uS:
    call    delay1uS                        ; delay for 1 uS
    dec     temp                            ; decrement the delay counter
    brne    delayTx1uS                      ; counter is not zero

; arrive here when delay counter is zero (total delay period is finished)
    ret

; ---------------------------------------------------------------------------
; Name:     delay1uS
; Purpose:  provide a delay of 1 uS with a 16 MHz clock frequency
; Entry:    no parameters
; Exit:     no parameters
; Notes:    add another push/pop for 20 MHz clock frequency

delay1uS:
    push    temp                            ; [2] these instructions do nothing except consume clock cycles
    pop     temp                            ; [2]
    push    temp                            ; [2]
    pop     temp                            ; [2]
    ret                                     ; [4]

; ============================== End of Time Delay Subroutines ==============
