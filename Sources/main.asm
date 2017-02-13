;*******************************************************************
; Lab 6
; Temperature Sensor using I2C  
; EELE 465, Section 2 @ MSU
; 15 APR 2014
; David Keltgen, Erik Moore
;
; This program is to control the state of the TEC unit and display
; reasonably accurate data about which state it is and temperature and
; time data.
 

;*******************************************************************

; Include derivative-specific definitions
            INCLUDE 'derivative.inc'
            XREF MCU_init

; export symbols
            XDEF _Startup, main
            ; we export both '_Startup' and 'main' as symbols. Either can
            ; be referenced in the linker .prm file or from C/C++ later on

            
            XREF __SEG_END_SSTACK   ; symbol defined by the linker for the end of the stack
            ;XDEF

*** SYSTEM DEFINITIONS AND EQUATES **************************************************
*** Internal Register Definitions
*** Application Specific Definitions


; variable/data section
MY_ZEROPAGE: SECTION


slave_address_r	DS.B		 $01   ; slave address (padded with last bit being read/write)
slave_address_w	DS.B		 $01   ; slave address (padded with last bit being read/write)
data_out		DS.B		 $01   ; data going to slave
BitCounter      DS.B		 $01
BitCounter2     DS.B		 $01
keypad_num      DS.B         $01   ; number pressed
DateTime        DS.B         $0C
DateTime_in     DS.B         $04
Seconds_in      DS.B         $01   ; variables coming back from RTC
Minutes_in      DS.B		 $01
Seconds_out     DS.B         $01   ; variables  used to set the RTC
Minutes_out     DS.B		 $01
write_num       DS.B         $01
Date_Counter    DS.B         $01
DateTime_Counter DS.B        $01
temp            DS.B         $01
temp1           DS.B         $02
tempx           DS.B         $01
ByteCounter     DS.B         $01
D_TMSG1         DS.B         $01
D_TMSG2         DS.B         $01
D_DMSG1         DS.B         $01
D_ENDMSG        DS.B         $01
AState          DS.B		 $06
ATemp           DS.B         $03
ASec            DS.B         $03
Temp_high       DS.B         $01
Temp_low        DS.B         $01
hundreds        DS.B		 $01
tens            DS.B		 $01
ones            DS.B		 $01
NUM1L			DS.B		 $01
NUM1H			DS.B		 $01
NUM2L			DS.B		 $01
NUM2H			DS.B		 $01
Result1			DS.B		 $01
Result2			DS.B		 $01
Result3			DS.B		 $01
kelvin_temp     DS.B         $02
counter         DS.B		 $01
value_selected  DS.B         $01
keypress_cntr   DS.B		 $01
BMode        	DS.B		 $01
B_tens			DS.B         $01
B_ones			DS.B		 $01
B_Target_Temp   DS.B		 $01

MY_DATA: SECTION         ; Insert here your data definition

MODE_MSG        DC.B         " Mode: A,B"
                DC.B         0	
MODEA_MSG1      DC.B         " TEC state:"
                DC.B         0	
          
MODEA_MSG2      DC.B         "                        T92:"
                DC.B         0	
MODEA_MSG3      DC.B         " K@T="
                DC.B         0	
MODEB_MSG1      DC.B         " Target Temp?"
				DC.B         0
MODEB_MSG2      DC.B         "                     Enter 10-40C"
				DC.B		 0
MODEB_MSG3      DC.B         " TEC state:Heat"
				DC.B         0
MODEB_MSG4      DC.B         " TEC state:Cool"
				DC.B         0
MODEB_MSG5      DC.B         "                        T92:"
                DC.B         0	
MODEB_MSG6      DC.B         " C@T="
                DC.B         0	
MODEB_MSG7		DC.B		 " TEC state:Hold"
				DC.B		 0                
SCL             EQU   3
SDA             EQU   2

CODE_SECT: SECTION
;XREF AllDone

;***************************************************************************************************
;*** Variable/data memory allocation 


; code section
MyCode:     SECTION
main:
_Startup:
			LDHX   #__SEG_END_SSTACK            ; initialize the stack pointer
			TXS
			
			;Clear the SLK LEDs
			MOV		0,PTAD			            ; clear ptb[0:7]
			MOV		#%00000001,PTADD            ; configure ptb[0:7] as 
			
			MOV     #%11111111,PTBDD            ; configure port B as output
  		  	LDA     #%00001000
			STA     PTBD                        ; Store to Port B
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD
		    
			LDA     #%00001001
			STA     PTBD                        ; Store to Port B
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD

			
			LDA     #$02                        ; equate the i2c pins
			STA     SDA
			LDA		#$03
			STA		SCL
			
			;Load spaces
			LDA     #%00100000
			STA     AState
			STA     AState + 1
			STA     AState + 2
			STA     AState + 3
			STA     AState + 4
			STA     AState + 5
			STA     ATemp
			STA     ATemp  + 1
			STA     ATemp  + 2
			STA     ASec
			STA     ASec   + 1
			STA     ASec   + 2
			
			LDA   	#$00
			STA		value_selected
			STA 	BMode   
			
			; Call generated device initialization function
			JSR    InitLCD
			JSR    MCU_init
			CLI
			CLRX
			JMP    mainLoop
;Example Code for LCD Module (HD44780) using 4-bit bus		    
*** Intialize Ports
InitLCD:
			lda	    #$FF 						; make ports outputs
			sta 	PTADD 						; PortA output
			sta 	PTBDD						; PortB output
			
	*** INITIALIZE THE LCD
	*** Wait for 15 ms
			jsr     Delay_short
			
	*** Send Init Command
			lda 	#$00 						; 
			sta 	PTAD                        ; set RS and R/W to 0
			lda     #%00111100                  ; enable the LCD
			sta     PTBD  
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD
			
	*** Wait for 4.1 ms
			jsr     Delay_short
			
	*** Send Init Command
			lda 	#$00 						; 
			sta 	PTAD                        ; set RS and R/W to 0
			lda     #%00111100                  ; enable the LCD  
			sta     PTBD
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD
			
    *** Wait for 100us
			jsr     Delay_short

	*** Send Init Command
			lda 	#$00 						; 
			sta 	PTAD                        ; set RS and R/W to 0
			lda     #%00111100                  ; enable the LCD  
			sta     PTBD
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD
				
			lda 	#$3C 						; LCD init command
			jsr 	LCD_WRITE 					; write data to LCD
			
	*** Send Function Set Command
	*** 4-bit bus, 2 rows, 5x7 dots
			lda 	#$2C 						; function set command
			jsr 	LCD_WRITE					; write data to LCD
			lda 	#$2C						; function set command
			jsr 	LCD_WRITE					; write data to LCD
			lda 	#$8C 						; function set command
			jsr 	LCD_WRITE					; write data to LCD
			
	*** Send Display Ctrl Command
	*** display on, cursor off, no blinking
			lda 	#$0C 						; display ctrl command MSB
			jsr 	LCD_WRITE 					; write data to LCD
			lda 	#$FC 						; display ctrl command LSB
			jsr 	LCD_WRITE					; write data to LCD
			
	*** Send Clear Display Command
	*** clear display, cursor addr=0
			lda 	#$0C 						; clear display command MSB
			jsr 	LCD_WRITE					; write data to LCD
			jsr     Delay_short
			lda 	#$1C 						; clear display command LSB
			jsr 	LCD_WRITE					; write data to LCD
			jsr     Delay_short
			
	*** Send Entry Mode Command
	*** increment, no display shift
			lda 	#$0C						; entry mode command MSB
			jsr 	LCD_WRITE					; write data to LCD
			lda 	#$6C 						; entry mode command LSB
			jsr 	LCD_WRITE					; write data to LCD
			RTS
	
mainLoop:   
			LDX     #$00
			JSR     InitLCD
            jsr     Display_Mode                ; write our startup message on startup or reset
            LDA     #$FF
            STA     counter
            JSR     keyscan_loop                ; poll for user input
            LDA     keypad_num                  ; load A or B
            
            ;if  key is A
            CBEQA   #$41, A_Loop                
            
            ;if key is B
            CBEQA   #$42, B_Loop
            
            BRA     mainLoop                    ; if neither, try again

A_Loop:
			;LDA     value_selected
			;BNE		A_Loop2
			
			JSR     InitLCD
			CLRX
            JSR     Display_State_A             ; Display MODEA_MSG1 and MODEA_MSG2 and modeamsg3
            LDA     #$FF
            STA     counter
            JSR     keyscan_loop                 ; poll for 0,1,2
            LDA     keypad_num                  ; load 0, 1 or 2
            
            ;if key is 0
            CBEQA   #$30, A_0_temp
            ;if key is 1
            CBEQA   #$31, A_1_temp
            ;if key is 2
            CBEQA   #$32, A_2_temp
           
            JSR     InitLCD
			LDX      #$00
			JSR     Display_State_A             ; Display MODEA_MSG1 and MODEA_MSG2 and modeamsg3
            JSR     Get_Time_Temp				; retrieve time from clock and temp from sensor 
                       ; final value to be displayed on LCD
            jsr     Delay_Sub						
            BRA     A_Loop
A_0_temp:
			JMP     A_0
A_1_temp:
			JMP     A_1           
A_2_temp:
			JMP     A_2 
           
B_Loop:
			LDA     #$01
			STA		BMode
			;LDA     $02
			;STA	    keypress_counter
			JSR     InitLCD
            LDX     #$00
            JSR     Display_Target_B 
            JSR		keyscan_loop
            LDA		keypad_num
            STA     B_tens		
			JSR		keyscan_loop
            LDA		keypad_num
			STA		B_ones	
			
			LDA     B_tens
			AND     #$0F
			;STA     B_tens
			
			LDX    #10
			MUL
			STA     temp
			
			
			LDA     B_ones
			AND     #$0F
			STA     B_ones
			
			
			LDA     temp
			ADD     B_ones
			STA     B_Target_Temp
			
			LDA     #$00
            STA	    Seconds_out
            STA     Minutes_out
            LDA     #%11010000                  ;initialize the slave address to the slaves address with write
			STA     slave_address_w    
            JSR     Start_Tx
			
			BRA     Regulate_Temp_B
Regulate_Temp_B:
			LDA     #$00
			STA     BMode
			JSR     keyscan_loop
			LDA     #$01
			STA     BMode
			JSR     Delay_Sub
			JSR     Get_Time_Temp              
			LDA     Temp_low					;Get degrees in celcius
			CMP     B_Target_Temp
			BGT		CoolDown
			BLT		HeatUp
Hold:		
 			LDA     #$08   						; select Q2 and Q1 on DFF, sends 0 to relay, turns TE off
 			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output
			STA     PTBD                        ; Store to Port B
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD 

			JSR     InitLCD
			JSR		Display_State_Hold_B		;Display Hold message
			BRA		Regulate_Temp_B	
				
HeatUp:
			LDA     #$88   						; select Q2 and Q1 on DFF, sends 8 to relay, turns TE to heating mode
 			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output
			STA     PTBD                        ; Store to Port B
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD
			
			JSR     InitLCD
			JSR		Display_State_Heat_B		;Display Heat message
			BRA		Regulate_Temp_B	
			
CoolDown:			
 			LDA     #$48   						; select Q2 and Q1 on DFF, sends 4 to relay, turns TE to cooling mode
 			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output
			STA     PTBD                        ; Store to Port B
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD  
			
			JSR     InitLCD
			JSR		Display_State_Cool_B		;Display Cool message
			BRA		Regulate_Temp_B           
                  
;turn off					
A_0:       
 			LDA     #$00
            STA	    Seconds_out
            STA     Minutes_out
            LDA     #%11010000                  ;initialize the slave address to the slaves address with write
			STA     slave_address_w    
            JSR     Start_Tx
 			LDA     #$08   						; select Q2 and Q1 on DFF, sends 0 to relay, turns TE off
 			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output
			STA     PTBD                        ; Store to Port B
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD 
			
			;load "off"
			LDA     #%01101111
			STA     AState
			LDA     #%01100110
			STA     AState + 1
			LDA     #%01100110
			STA     AState + 2
			
			;load spaces
			LDA     #%00100000
		    STA     AState + 3
			STA     AState + 4
			STA     AState + 5
			LDA     #$01
			STA     value_selected
			JMP     A_Loop
;heat			
A_1:
            LDA     #$00
            STA	    Seconds_out
            STA     Minutes_out
            LDA     #%11010000                  ;initialize the slave address to the slaves address with write
			STA     slave_address_w
            JSR     Start_Tx
			LDA     #$88   						; select Q2 and Q1 on DFF, sends 8 to relay, turns TE to heating mode 
 			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output
			STA     PTBD                        ; Store to Port B
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD 
			
			;load "heat"
			LDA     #%01101000
			STA     AState
			LDA     #%01100101
			STA     AState + 1
			LDA     #%01100001
			STA     AState + 2
			LDA     #%01110100
		    STA     AState + 3
		    
			;load spaces
			LDA     #%00100000
			STA     AState + 4
			STA     AState + 5
			
			LDA     #$01
			STA     value_selected
			JMP     A_Loop
;cool
A_2:		
            LDA     #$00
            STA	    Seconds_out
            STA     Minutes_out
            LDA     #%11010000                  ;initialize the slave address to the slaves address with write
			STA     slave_address_w
            JSR     Start_Tx
 			LDA     #$48   						; select Q2 and Q1 on DFF, sends 4 to relay, turns TE to cooling mode
 			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output
			STA     PTBD                        ; Store to Port B
			BCLR    3,PTBD                      ; Latch Data
			BSET    3,PTBD 
			
			;load "cool"
			LDA     #%01100011
			STA     AState
			LDA     #%01101111
			STA     AState + 1
			LDA     #%01101111
			STA     AState + 2
			LDA     #%01101100
		    STA     AState + 3
		    
			;load spaces
			LDA     #%00100000
			STA     AState + 4
			STA     AState + 5
			LDA     #$01
			STA     value_selected
			JMP     A_Loop
			
Get_Time_Temp:
			NOP
	    	LDA 	#%00001100					; set up as outputs
	    	STA     PTADD
	    	STA     PTAD                  		; driven high to start
	    	
			LDA     #%11010000                  ;initialize the slave address to the slaves address with write
			STA     slave_address_w
			LDA     #%11010001                  ;initialize the slave address to the slaves address with read
			STA     slave_address_r
			
			
			JSR     Start_Rx                    ;get the time (minutes and seconds)
			LDA     DateTime_in + 3
			STA     Seconds_in
			LDA     DateTime_in + 2            ;
			STA     Minutes_in
            
            ;LDA     Minutes_in                  ; add 60 seconds for every minute
            ;LDX     #60
            ;MUL     
            ;STA     temp1 + 1
            ;STX     temp1
            
            ;store Tens position of seconds to LCD
            LDA     Seconds_in
			AND  	#$F0
			NSA 
			ORA     #$30
			STA     ASec +1   
			
			
			LDA      Seconds_in
			AND  	 #$0F
			ORA      #$30
			STA      ASec + 2    
            LDA      Minutes_in
            AND      #$0F
            ORA      #$30
            STA      ASec
            
			LDA     #%10010000                  ;initialize the slave address to the slaves address with write
			STA     slave_address_w
			LDA     #%10010001                  ;initialize the slave address to the slaves address with read
			STA     slave_address_r
			
			LDA 	#%00001100					; set up as outputs
	    	STA     PTADD
	    	STA     PTAD                  		; driven high to start
	    	JSR     Start_Rx                    ; get temperature
	    	LDA     DateTime_in + 3
			STA     Temp_high
			LDA     DateTime_in + 2            ;
			STA     Temp_low
			
			;Shift right to account for offset in LM92 Register
			LSR     Temp_high
			ROR     Temp_low
			LSR     Temp_high
			ROR     Temp_low
			LSR     Temp_high
			ROR     Temp_low
			
			;Divide by 16
			LSR     Temp_high
			ROR     Temp_low
			LSR     Temp_high
			ROR     Temp_low
			LSR     Temp_high
			ROR     Temp_low
			LSR     Temp_high
		    ROR     Temp_low	
			
			PSHA
			PSHH
			PSHX
			
			LDA     BMode
			BEQ		Kelvin                      ; If in A mode, convert to kelvin		
Celcius:										; Otherwise B mode, convert celcsius
			 ;If in Mode B,  Get Celsius Temp in BCD
			 ;divide number by 10, quotient is the tens place, remainder is the ones place
		    LDA     #$00
            PSHA
            PULH
            LDHX     #$11
            LDA      #$1 
		    LDA     Temp_low
            LDX     #10
            DIV
            STA     tens 
            PSHH                                ; H is the remainder
            PULA      
            STA     ones                        ; save the remainder to ones place
            
            LDA     tens
            ORA     #$30
            STA     ATemp
            
            LDA     ones
            ORA     #$30
            STA     ATemp + 1
            BRA     End_Time_Temp
            

			
Kelvin:			
			;If in Mode A, get Kelvin Temp in BCD
			;add 274 to get temperature in Kelvin
			LDA    #$00
			STA    NUM1H
			LDA    Temp_low
			STA    NUM1L
			
			LDA    #$01       
			STA    NUM2H
			LDA    #$11
			STA    NUM2L     
            JSR    ADD16_16                     ;our sum will be in Result2(high) and Result3(low)
            
            LDA    Result2
            STA    kelvin_temp
            LDA    Result3
            STA    kelvin_temp + 1
			
			
		 ; divide kelvin_temp by 100
            LDA kelvin_temp
            PSHA
            PULH
            LDA kelvin_temp + 1
            LDX #100
           
            DIV
            ORA     #$30
            STA ATemp 
           
          
           ;now get remainder
            PSHH
            PULA
            STA   kelvin_temp
            LDA #$00
            PSHA
            PULH
            LDA  kelvin_temp                      
            ;divide kelvin_temp by 10, quotient is the tens place, remainder is the ones place
            LDX     #10
            DIV
            ORA     #$30
            STA     ATemp +1 
            PSHH                                ; H is the remainder
            PULA    
            ORA     #$30  
            STA     ATemp + 2                    ; save the remainder to ones place
			
End_Time_Temp:
			;Restore values
			PULX
            PULH
            PULA
			
			RTS
			
ADD16_16:
    ;Step 1 of the process
			LDA NUM1L     ;Move the low-byte into the accumulator
			ADD NUM2L     ;Add the second low-byte to the accumulator
			STA Result3     ;Move the answer to the low-byte of the result

    ;Step 2 of the process
			LDA  NUM1H     ;Move the high-byte into the accumulator
			ADC NUM2H    ;Add the second high-byte to the accumulator, plus carry.;
			STA  Result2     ;Move the answer to the high-byte of the result

    ;Step 3 of the process
			LDA  #00h   ;By default, the highest byte will be zero.
			ADC #00h  ;Add zero, plus carry from step 2. 
			STA  Result1 ;Move the answer to the highest byte of  the result

    ;Return - answer now resides in Result1, Result2, and Result3.
			RTS			
			
Display_Mode:		   
            LDA     MODE_MSG, X
            BEQ     Out_Mode
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODE_MSG, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		jsr     Delay_short
     		jsr     Delay_short
            bra     Display_Mode   
            
Out_Mode:
			RTS 
   
			
Display_State_A:
L0:		   
            LDA     MODEA_MSG1, X
            BEQ     Out_L0
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODEA_MSG1, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		;jsr     Delay_short
     		jsr     Delay_short
            bra     L0   
            
Out_L0:  
			LDA     AState  
			STA     write_num
			JSR     write_char
			LDA     AState + 1
			STA     write_num
			JSR     write_char
			LDA     AState + 2
			STA     write_num
			JSR     write_char
			LDA     AState + 3
			STA     write_num
			JSR     write_char
			LDA     AState + 4
			STA     write_num
			JSR     write_char
			LDA     AState + 5
			STA     write_num
			JSR     write_char
			CLRX
			
L1:		   
            LDA     MODEA_MSG2, X
            BEQ     Out_L1
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODEA_MSG2, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		jsr     Delay_short
     		jsr     Delay_short
            bra     L1   
            
Out_L1: 
			LDA     ATemp  
			STA     write_num
			JSR     write_char
			LDA     ATemp + 1
			STA     write_num
			JSR     write_char
			LDA     ATemp + 2
			STA     write_num
			JSR     write_char
			CLRX
			;LDA    
L2:
            LDA     MODEA_MSG3, X
            BEQ     Out_L2
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODEA_MSG3, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		jsr     Delay_short
     		jsr     Delay_short
            bra     L2
Out_L2:
			LDA     ASec  
			STA     write_num
			JSR     write_char
			LDA     ASec + 1
			STA     write_num
			JSR     write_char
			LDA     ASec + 2
			STA     write_num
			JSR     write_char 
			RTS
			
 		
Display_Target_B:
L0_B:		   
            LDA     MODEB_MSG1, X
            BEQ     Out_L0_B
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODEB_MSG1, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		;jsr     Delay_short
     		jsr     Delay_short
            bra     L0_B 
Out_L0_B: 
			CLRX
L1_B:
			LDA     MODEB_MSG2, X
            BEQ     Out_L1_B
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODEB_MSG2, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		jsr     Delay_short
            bra     L1_B 
Out_L1_B: 
			CLRX
			RTS
			
Display_State_Heat_B:
L2_B:					
			LDA     MODEB_MSG3, X
            BEQ     Out_L2_B
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODEB_MSG3, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		jsr     Delay_short
            bra     L2_B 
Out_L2_B: 
			CLRX
			LDA     B_tens
			ORA     #$30   
			STA     write_num
			
			JSR     write_char
			LDA     B_ones
			ORA     #$30 
			STA     write_num
			JSR     write_char
			JSR     L5_B

			
			RTS

Display_State_Cool_B:
L3_B:					
			LDA     MODEB_MSG4, X
            BEQ     Out_L3_B
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODEB_MSG4, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		jsr     Delay_short
            bra     L3_B 
Out_L3_B: 
			CLRX
			LDA     B_tens  
			ORA     #$30 
			STA     write_num
			JSR     write_char
			LDA     B_ones
			ORA     #$30 
			STA     write_num
			JSR     write_char
			JSR     L5_B
			RTS		
Display_State_Hold_B:
L4_B:					
			LDA     MODEB_MSG7, X
            BEQ     Out_L4_B
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODEB_MSG7, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		jsr     Delay_short
            bra     L4_B 
Out_L4_B: 
			CLRX
			LDA     B_tens 
			ORA     #$30  
			STA     write_num
			JSR     write_char
			LDA     B_ones
			ORA     #$30 
			STA     write_num
			JSR     write_char
			JSR     L5_B
			RTS		

L5_B:		   
            LDA     MODEB_MSG5, X
            BEQ     Out_L5_B
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODEB_MSG5, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		jsr     Delay_short
     		jsr     Delay_short
            bra     L5_B   
            
Out_L5_B: 
			LDA     ATemp  
			STA     write_num
			JSR     write_char
			LDA     ATemp + 1
			STA     write_num
			JSR     write_char
			;LDA     ATemp + 2
			;STA     write_num
			;JSR     write_char
			CLRX
			;LDA    
L6_B:
            LDA     MODEB_MSG6, X
            BEQ     Out_L6_B
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C   
            jsr     LCD_WRITE
            LDA     MODEB_MSG6, X
            NSA
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C               		
		    bset    1, PTAD
            jsr     LCD_WRITE
            incx
     		jsr     Delay_short
     		jsr     Delay_short
            bra     L6_B
Out_L6_B:
			LDA     ASec  
			STA     write_num
			JSR     write_char
			LDA     ASec + 1
			STA     write_num
			JSR     write_char
			LDA     ASec + 2
			STA     write_num
			JSR     write_char 
			RTS
			              
keyscan_loop:
			LDA     BMode
			BNE		scans
			LDA     counter 
            DECA
            STA     counter
            BEQ     End2_temp
            jmp     scans
End2_temp:
			jmp     End2;
            
scans:
 ;check for row1
			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output					   
			; send values out to data line
			LDA     #%11100010                  ; send out a zero to 273 and clock the 273 
			STA     PTBD                        ; send values out to data line and the hc138  
			MOV     #%00001111, PTBDD           ; configure ptb[7:4] as input, leave other pins as outputs
			LDA     #%00000011                  ; set the operation of the 245 to send data from A bus to B bus
			STA     PTBD                  
			LDA     PTBD                        ; read what is on port b
			AND     #%11110000                  ; only care about ptb [7:4]
			CBEQA   #%11100000, jump_1          ;  
			CBEQA   #%11010000, jump_2          ;  
			CBEQA   #%10110000, jump_3          ; 
			CBEQA   #%01110000, jump_A          ; 			
	    ;end check for row 1
;check for row2
			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output				
			; send values out to data line
			LDA     #%11010010                  ; send out a zero to 273 and clock the 273 
			STA     PTBD                        ; send values out to data line and the hc138
			MOV     #%00001111, PTBDD           ; configure ptb[7:4] as input, leave other pins as outputs
			LDA     #%00000011                  ; set the operation of the 245 to send data from A bus to B bus
			STA     PTBD                  
			LDA     PTBD                        ; read what is on port b
			AND     #%11110000                  ; only care about ptb [7:4];	   
			CBEQA   #%11100000, jump_4          ;  
			CBEQA   #%11010000, jump_5          ;  
			CBEQA   #%10110000, jump_6          ; 
			CBEQA   #%01110000, jump_B          ;
	    ;end check for row 2
;check for row3
			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output					
			; send values out to data line
			LDA     #%10110010                  ; send out a zero to 273 and clock the 273 
			STA     PTBD                        ; send values out to data line and the hc138
			MOV     #%00001111, PTBDD           ; configure ptb[7:4] as input, leave other pins as outputs
			LDA     #%00000011                  ; set the operation of the 245 to send data from A bus to B bus
			STA     PTBD                 
			LDA     PTBD                        ; read what is on port b
			AND     #%11110000                  ; only care about ptb [7:4]
			CBEQA   #%11100000, jump_7          ;  
			CBEQA   #%11010000, jump_8          ;  
			CBEQA   #%10110000, jump_9          ; 
			CBEQA   #%01110000, jump_C          ; 
	    ;end check for row3
;check for row4
			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output
			LDA     #%01110010                  ; send out a zero to 273 and clock the 273 
			STA     PTBD                        ; send values out to data line and the hc138
			MOV     #%00001111, PTBDD           ; configure ptb[7:4] as input, leave other pins as outputs
			LDA     #%00000011                  ; set the operation of the 245 to send data from A bus to B bus
			STA     PTBD                  
			LDA     PTBD                        ; read what is on port b
			AND     #%11110000                  ; only care about ptb [7:4]
		    CBEQA   #%11100000, jump_E          ;  
			CBEQA   #%11010000, jump_0          ;  
			CBEQA   #%10110000, jump_F          ; 
			CBEQA   #%01110000, jump_D          ; 
	    ;end check for row4
;end:

forever: ;keep in the main loop until a button pressed
			JMP    keyscan_loop

jump_1:     LDA     #$31          
			JMP   endISR;
jump_2:     LDA     #$32
			JMP   endISR;
jump_3:     LDA     #$33 
			JMP   endISR;
jump_A:		LDA     #$41        
			BRA     endISR;
jump_4:     LDA     #$34
			JMP   endISR;
jump_5:     LDA     #$35
			JMP   endISR;
jump_6:     LDA     #$36 
			JMP   endISR;
jump_B:		LDA     #$42     
			BRA     endISR;
jump_7:     LDA     #$37 
			JMP   endISR;
jump_8:     LDA     #$38
			JMP   endISR;
jump_9:     LDA     #$39
			JMP   endISR;
jump_C:		LDA     #$43       
			BRA     endISR;
jump_0:  	LDA     #$30   
			JMP     endISR;
jump_D:  	LDA     #$44       
			BRA     endISR;
jump_E:    	LDA     #$2A     
			JMP     mainLoop;
jump_F:     LDA     #$23    
			BRA     endISR;	
endISR:
            STA     keypad_num
			
		 ;display the number pressed
            BCLR    2, PTADD
			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output			
			LDA     keypad_num                        ; get ascii value
			AND     #%11110000                  ; keep upper nibble
			ORA     #$0C                        ; 
			jsr     LCD_WRITE
            LDA     keypad_num
            NSA
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C                        ; 
			jsr     LCD_WRITE		
			jsr		Delay_Sub;
			
			ADD     keypress_cntr
		    RTS
End2:       
			LDA     #$FF
			STA     keypad_num
			RTS
InitLCD3:
            jmp InitLCD 
   			
	
  
            
*----------------------------------------------
* Start of I2C Code
*----------------------------------------------

Start_Tx:
		;Initialize Variables
			;CLR     data_out                    ; Clear Variables
			CLR		BitCounter
	    ;Set up the ports
	    	LDA 	#%00001100					; set up as outputs
	    	STA     PTADD
	    	STA     PTAD                  		; driven high to start
	    	
	    	
* -------------------------------------------------------------
* Send the I2C transmission, including START, address,
* data, and STOP
* -------------------------------------------------------------

SendIt:
		;Start Condition
			JSR		I2CStartBit					; Give Start Condition
			
		;Address byte, consists of 7-bit address + 0 as LSbit
			LDA		slave_address_w             ;
			JSR		I2CTxByte					;
		
		;select the register on the Real-time-clock
			LDA     #$00
			JSR		I2CTxByte
			
        ;Send out our data
			LDA     Seconds_out
			JSR     I2CTxByte
			LDA     Minutes_out
			JSR     I2CTxByte
				
		;Stop Condition
			JSR		I2CStopBit   				; Give STOP Condition
			
			JSR 	I2CBitDelay					; Wait a bit
			;Bra		Start_Tx					; Repeat
			RTS
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; I2CTxByte
; Transmit the byte in Acc to the SDA pin
; (Acc will not be restored on return)
; Must be careful to change SDA values only while SCL is low,
; otherwise a STOP or START could be implied
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
I2CTxByte:
		; Intialize Variable
			LDX		#$08
			STX		BitCounter
I2CNextBit:
			ROLA								;Shift MSB into carry
			BCC		SendLow						;Send low or high bit
SendHigh:
			BSET	SDA, PTAD					;set the data bit value
			JSR		I2CSetupDelay				;Give some time for data setup
			BSET	SCL, PTAD					; Clock it in
			JSR		I2CBitDelay					; wait a bit
			BRA		I2CTxCont					; Continue
SendLow:
			BCLR 	SDA, PTAD
			JSR 	I2CSetupDelay
			BSET 	SCL, PTAD
			JSR 	I2CBitDelay
I2CTxCont:
			BCLR 	SCL, PTAD					;Restore clock to low state
			Dec		BitCounter					;Decrement the bit counter
			BEQ		I2CAckPoll1					;Last bit
			BRA		I2CNextBit
I2CAckPoll1:
            BSET	SDA, PTAD					;Restore clock to high state
			BCLR	SDA, PTADD					;set SDA as input
			JSR		I2CSetupDelay				; 
			BSET	SCL, PTAD					;Clock the line to get ACK
			JSR		I2CBitDelay					
			BRSET   SDA, PTAD, I2CNoAck1			; look for ack from slave device
			
			BCLR	SCL, PTAD					; restore clock line
			BSET	SDA, PTADD					; SDA back as output
			RTS		
			
I2CNoAck1:
			BCLR 	SCL, PTAD
			BSET 	SDA, PTADD
			RTS	
*------------------------------------------------------------------
* I2C Rx Routine 
*
* -send slave address with write
* -send the word address
* -repeated start
* -send slave address with read
* -clock the line to receive data, send acknowledge
* -repeat for desired # of bytes end with nack and stop bit
*
*------------------------------------------------------------------

Start_Rx:

			;LDX
		;Start Condition
			LDA 	#%00001100					; set up as outputs
	    	STA     PTADD
	    	STA     PTAD                  		; driven high to start
			JSR		I2CStartBit					; Give Start Condition
			
		;Address byte, consists of 7-bit address + 0 as LSbit
			LDA		slave_address_w             ;
			JSR		I2CTxByte					;
		
		;select the register on the Real-time-clock
			LDA     #$00                        ; 10 second and second register, or the temperature register
			JSR		I2CTxByte
			
		;Repeated start    
			LDA 	#%00001100					; set up as outputs
	    	STA     PTADD
	    	STA     PTAD                  		; driven high to start
		    BSET    SCL, PTAD
			JSR     I2CStartBit
			;LDA 	#%00001100					; set up as outputs
	    	;STA     PTADD
	    	;STA     PTAD                  		; driven high to start
			
			;JSR		I2CStartBit					; Give Start Condition			
		;Now send address for read
			LDA		slave_address_r             ;
			JSR		I2CTxByte					;
			
			;number of byte received from RTC
			LDX     #$03	
			
Receive_Data:		
            BCLR 	SDA, PTADD                  ; now set data as input
            ;LDA     #$07
            ;STA     Byte_Counter
            LDA     #$08
            STA     BitCounter2
			;LDX     #$07
			LDA     #$00
            STA     temp

			
            
I2CRx:    ; Just reading one byte for now      
			BSET 	SCL, PTAD                   ;
			JSR 	I2CBitDelay                 ;
			LDA     PTAD;
			AND     #%00000100
			LSRA                                ; move value to LSbit and combine with the temp value
			LSRA
			ORA     temp
			
			;if last bit store to variable and be done
			DEC     BitCounter2
			BEQ     EndByte

            ;if not last bit, move the value to the left 1 and run the clock again
			LSLA
			STA     temp
			BCLR 	SCL, PTAD 
			LDA     BitCounter2
			BNE		I2CRx  
			
			
EndByte:        ;on last bit, store the 8 bit value
			STA     DateTime_in,X
			DBNZX   Continue
			BRA     EndData
Continue:            
			BCLR	SCL, PTAD
			JSR 	I2CBitDelay		
            JSR     I2CAckPoll2
            BRA     Receive_Data
         ; if we still have to read bytes continue with reading
			
 
EndData:
         ; done getting data
		    JSR     I2CNoAck                    ; last byte of data followed by nack
		 ;Stop Condition
			JSR		I2CStopBit   				; Give STOP Condition
			JSR 	I2CBitDelay					; Wait a bit

            ;JMP     Start_Rx
			;JSR     Start_Rx
			RTS

			
I2CAckPoll:
			BSET	SDA, PTAD					;Restore clock to high state
			BCLR	SDA, PTADD					;set SDA as input
			JSR		I2CSetupDelay				; 
			BSET	SCL, PTAD					;Clock the line to get ACK
			JSR		I2CBitDelay					
			BRSET   SDA, PTAD, I2CNoAck			; look for ack from slave device
			
			BCLR	SCL, PTAD					; restore clock line
			BSET	SDA, PTADD					; SDA back as output
			RTS
I2CAckPoll2:
			BSET	SDA, PTADD					;set SDA as output
			BCLR	SDA, PTAD					;
			JSR		I2CSetupDelay				; 
			BSET	SCL, PTAD					;Clock the line to get ACK
			JSR		I2CBitDelay					
			BRSET   SDA, PTAD, I2CNoAck			; look for ack from slave device
			
			BCLR	SCL, PTAD					; restore clock line
			BCLR	SDA, PTADD					; SDA back as output
			RTS
			
			;No acknowledgment received from slave device
			;Some error action can be performed here
			;For now, just restore the bus
I2CNoAck:
			BCLR 	SCL, PTAD
			BSET 	SDA, PTADD
			RTS
			
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; A START condition is defined as a falling edge
; on SDA while SCL is high
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-			
I2CStartBit:
			BCLR	SDA, PTAD
			JSR		I2CBitDelay
		;	JSR		I2CBitDelay
		;	JSR		I2CBitDelay
			BCLR	SCL, PTAD
			RTS
			
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; A STOP condition is defined as a rising edge
; on SDA while SCL is high
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
I2CStopBit:
			BCLR SDA,PTAD
			BSET SCL,PTAD
			BSET SDA,PTAD
			JSR I2CBitDelay
			RTS

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; Provide some data setup time to allow
; SDA to stabilize in slave device
; Completely arbitrary delay (10 cycles)
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
I2CSetupDelay:
			NOP
			NOP
			RTS
			
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Bit delay to provide (approximately) the desired
; SCL frequency
; Again, this is arbitrary (16 cycles)
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
I2CBitDelay:
			NOP
			NOP
			NOP
			NOP
			NOP
			RTS
Initialize_time: ;initialize the time to $00
			LDA   #%11010000   ;address of the real time clock with write condition
			STA   slave_address_w
			LDA  #$00
			STA   Minutes_out
			STA   Seconds_out
		    JSR     SendIt

write_char:
            LDA     write_num                   ; load the character
            
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C                        ; 
			jsr     LCD_WRITE
			JSR     Delay_short
           LDA     write_num
            NSA
            AND     #%11110000                  ; keep upper nibble
			ORA     #$0C                        ; 
			jsr     LCD_WRITE
			jsr		Delay_short;
            RTS          

Delay_short:
			PSHX
			PSHA
			LDA   #$FF
short_center:
			DECA
			BNE   short_center         
short_done:
            PULA                                 ; Restore the registers
            PULX
            RTS
***************************************************************************
Delay_Sub:
			PSHX                                ; Save registers
			PSHA      

; DO (outer loop)
			LDA    #$FF                         ; Initialize Outer Loop
outer:
; WHILE (a != 0)
;    a = a-1           
             
			DECA              
			BEQ    all_done                     ; Branch to end when y=0        

;     DO (inner loop)
			LDX    #$FE                         ; Initialize Inner Loop
			PSHA
inner:
;     WHILE (x != 0)
;       x = x-1          
          
			LDA   #$0E
;     WHILE ( a != 0)
;	  a = a- 1			
center:
			DECA
			BNE   center
			DECX  
			BNE    inner     
			PULA
;     ENDWHILE (outer loop)           
            BRA    outer
               
; ENDWHILE (inner loop)
all_done:
            PULA                                 ; Restore the registers
            PULX
            RTS

*** SUBROUTINES ********************************************************************
*** Routine creates a delay according to the formula
*** TIME*100us using a 2-MHz internal bus
*** Cycle count per instruction shown

*** Routine sends LCD Data
LCD_WRITE:  
			MOV     #%11111111, PTBDD           ; configure ptb[0:7] as output
			sta     PTBD
		    BCLR    3,PTBD                      ; Latch Data
		    BSET    3,PTBD
		    jsr     Delay_short;
			rts
*** Routine sends LCD Address
LCD_ADDR:	
			bclr    0, PTAD
			bclr    1, PTAD 					;LCD in command mode
			sta     PTBD
			BCLR    3,PTBD                      ; Latch Data
		    BSET    3,PTBD
		    jsr     Delay_short
			bset    1, PTAD 				        ;LCD in data mode
			rts	          



















