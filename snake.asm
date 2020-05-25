[bits 16]
[ORG 0x7c00]


; ==============
; initialisation
; ==============


start:
	mov ax, 13h				; video mode 13h (320x200 pixels)
	int 10h					; interrupt 10h
	
	cld						; set correct scroll direction

	mov ax, 0A000h			; offset to video memeory
	mov es, ax				; point es to video memeory
	
	mov ax, 000c0h			; offset to snake information
	mov ds, ax

make_snake:					; create a snake for the first run
	in al, (0x40)			; "random" direction
	and al, 03h				; preserve lowest 2 bits (direction)
	add al, 0b0000_0100		; make initial length 1
	; mov di, 83h			; point di to length/dir byte
	mov [0x83], al			; save the initial snake info
	mov BYTE [0x80], 0d15
	mov BYTE [0x7f], 0d09
	
	call fn_make_candy			; make a candy

; ==============
; main game loop
; ==============

game_loop:
set_timer:
	; uses Int15/AH=83h for time keeping 
	; for more details: http://www.oldlinux.org/Linux.old/docs/interrupts/int-html/rb-1512.htm
	;mov cx, 0x0003
	;mov dx, 0x0D40			; CX:DX = 0x0001:0x86A0 = 100,000μs = 0.1s = 10 frame per second
		
	;mov bx, 0d65000			; posting byte = ES:BX (0A000h:0d64000) this byte will be updated after 0.1s
	;mov BYTE [es:bx], 0x00	; clear the posted byte for detection
	;
	;xor al, al				; AL = 0, set interval
	;mov ah, 0x86			; AH = 0x86 
	;int 0x15				; wait for 0.10s before releasing continuing

	xor ah, ah
	int 0x1a				; read system timer counter (18.2 count per sec) 
	inc dx					; break loop when count = start_count + 1
	mov bx, dx				; save start time
loop_timer:
	xor ah, ah
	int 0x1a
	cmp bx, dx
	jne loop_timer

	
	; refer to label "read_timer" for codes detecting the change in ES:BX
	
clear_screen:
	mov ax, 0				
	push ax					; argument: color = black
	call fn_draw_snake
	add sp, 2 
	
	; int 16h, 01h: http://vitaly_filatov.tripod.com/ng/asm/asm_027.2.html
check_input:
	mov ah, 0x01
	int 0x16
	jnz	get_input			; if an key is pressed, get input scan code
	jmp process_input

get_input:
	mov ah, 0x00
	int 0x16
	mov cx, ax				; save scan code temporarily
	jmp check_input			; cycle back until the buffer is empty

;  -----------------------------------------
; |important scan codes						|
; |				ah	al	dir		dir bits	|	
; |[Up]		: 	48 	00	north	0b00		|
; |[left]	: 	4B 	00	east  	0b01		|
; |[Down]	: 	50 	00	south 	0b10		|
; |[Right]	: 	4D 	00	west  	0b11		|
;  -----------------------------------------

process_input:
	
	mov ax, cx				; retrieve scan code
	xor bx, bx				; clear bx for writing
is_up:
	cmp ah, 0x48
	jne	is_right
	mov bx, 1
is_right:
	cmp ah, 0x4D
	jne	is_down
	mov bx, 2
is_down:
	cmp ah, 0x50
	jne	is_left
	mov bx, 3
is_left:
	cmp ah, 0x4B
	jne	is_arrow
	mov bx, 4
is_arrow:
	test bx, bx
	jz	post_input
input_process:
	dec bx
	mov si, 83h				; retrieve length/dir byte
	lodsb
	mov cl, al				; save l/d for later
	and al, 0b0000_0011		; get current direction bits
	xor	al, bl				; block magic
	cmp al, 0b10			; if AL XOR BL = 0b10, the inputted dir is opposite of the direction which
	je	post_input			; the snake is travelling at. This is illegal and will be ignored
	
input_store:
	and cl, 0b111_1100		; clear the lowest 2 bits
	or	cl, bl				; load in the new direction bits
	mov si, 0x83
	mov [ds:si], cl

post_input: 

move:
	mov si, 83h				; retrieve length/dir byte
	lodsb
	shr ax, 2			
	and ax, 00111111b		; preserve lowest 6 bits (length)
	mov cx, 2
	mul cx					; overall: ax = 2ax
	mov cx, ax				; cx = 2ax
			
move_flush:
	mov ax, 000c0h
	mov es, ax				; temporarily set es to snake location
	
	mov ax, 0x7f
	sub ax, cx
	mov di, ax				; overall: di = 0x7f - cx = 0x7f - 2 * LEN
	
	mov ax, 0x81
	sub ax, cx
	mov si, ax				; overall: si = 0x81 - cx = 0x81 - 2 * LEN
	
	rep
	movsb					; move the snake by one block
	
	mov ax, 0A000h
	mov es, ax				; reset es back to video ram
	
move_get_dir: 
	mov si, 83h				; retrieve length/dir byte
	lodsb
	and al, 0x03			; preserve lowest 2 bits (direction)
	mov cx, ax
	
	mov si, 0x7d			; point to old y-coord of snake head
	
	lodsb 					; load old y-coord
	cmp cl, 0b00			; moving north?
	jne di1
	dec ax
di1:cmp cl, 0b10			; moving south?
	jne	di2
	inc ax
di2:mov bx, ax				; bx = new y
	
	lodsb					; load old x-coord
	cmp cl, 0b01			; moving east?
	jne di3
	inc ax
di3:cmp cl, 0b11			; moving west?
	jne di4
	dec ax					; ax = new x
di4:cmp al, 0				; bound check
	jl	game_over
	cmp al, 31
	jg	game_over
	cmp bl, 0
	jl	game_over
	cmp bl, 19
	jg	game_over
	
	mov [0x80], al			; 0x0c80 = new x
	mov [0x7f], bl			; 0x0c7f = new y
	
	
collision_detection:  
	mov	bh, al				; bh = x, bl = y
	
	mov si, 0x83
	lodsb
	shr al, 2				; al = snake length
	cmp al, 1
	je eat_candy
	
	xor ah, ah				; clear higher byte
	mov cx, ax				; cx = ax
	dec cx					; cx = LEN - 1
	
	shl ax, 1				; ax = 2ax
	mov si, 0x81			
	sub si, ax				; si = 0x81 - 2ax = 0x81 - 2*LEN, e.g. LEN=2, si=0x7d
				
eat_itself:
	lodsb					; al = comparison y-coord
	cmp bl, al
	jne cont1				; if false, continue,
							; however, si needs to ++ as the second lodsb did not run
	lodsb					; al = comparison x-coord
	cmp bh, al
	jne cont2				; if false, continue
	jmp game_over
cont1:
	inc si			
cont2:
	loop eat_itself		
	
	
eat_candy: 
	mov si, 0x81			; candy y-coord
	lodsb
	cmp bl, al
	jne	draw_candy
	lodsb					; candy x-coord
	cmp bh, al
	jne draw_candy
	
ate_candy:
	mov si, 0x83
	mov al, [ds:si]
	add al, 0b0100			; increase snake length by one
	jc	victory 
grow:
	mov [ds:si], al			; write the increased length to memory
	call fn_make_candy			; generate a new candy
	
draw_candy: 
	mov si, 0081h			; y-coordinate
	lodsb					; load [ds:si] to al
	mov bx, ax				; bx = y
	lodsb					
	
	mov cx, 10		
	push cx					; 1st argument: color
	push bx					; 2nd argument: y
	push ax					; 3rd argument: x
	
	call fn_draw_block
	
	add sp, 6				; remove arguments from stack
			
draw_snake:
	mov ax, 7				
	push ax					; argument: color
	call fn_draw_snake
	add sp, 2 
	
read_timer:
	
;	mov si, 0d65000
;	mov al, [es:si]
;	cmp al, 0x00
;	je read_timer			; test again
	jmp game_loop			; restart loop

; =========
; FUNCTIONS
; =========	

fn_make_candy:					; function, create candy on a random tile
	in al, (0x40)			; generate a "random" number
	and ax, 31d				; 0 < ax < 31
	mov di, 0082h
	mov [ds:di], al			; save x-coordinate of candy
	
	in al, (0x40)			; get a "random" number again
	and ax, 15d				; 0 < ax < 15
	add ax, 02d				; 2 < ax < 27
	dec di					; di = 0081h
	mov [ds:di], al			; save y-coordinate of candy
	ret
	

fn_draw_snake:				; function, arguments 1) Color
	xchg bx, bx
	add sp, 2				; skip eip on stack 
	pop ax					; ax = color
	sub sp, 8				; save color to the 2nd 16bit slot on top of eip
	push ax					; leaving 1 slot for cx
	add sp, 6				; return to eip

	mov si, 0083h
	mov ax, [ds:si]			; ax = Length and Direction information, format:[0bLLLL_LLDD]
	shr ax, 2				; shift right by 2 bits, result: ax = 0b00LL_LLLL
	mov cx, ax				; cx = length of snake = number of blocks to draw
	xor ch, ch				; clear higher bits, not needed
	
	mov si, 0080h			; initial offset from memory is 3 for the first 3 bytes are used 
	
	; reserved information: cx = length of snake (decreasing every loop), di = memeory offset
	

	loop_block:
		push cx
		sub sp, 2			; skip saved color value, 1st paramter: color
		
		mov al, [ds:si]		; x-coordinate of the snake block (0-31)
		mov bx, ax			; bx = x
		
		dec si
		mov al, [ds:si]		; y-coordinate of the snake block (0-19), ax = y
		dec si				; next byte (for the next loop)
				
		push ax				; 2nd parameter: y
		push bx				; 3rd parameter: x
		
		call fn_draw_block
		add sp, 6			; remove the function arguments
		
		pop cx
	loop loop_block
	ret

	
fn_draw_block: 				; draw a 10x10 snake block from (x,y) to (x+9, y+9)
							; arguments: 1) Color 2) Y-coord 3) X-coord
	; OPT mov bp, sp
	add sp, 2 				; skip EIP at top of stack (Apparently 4 Bytes)
	pop ax					; x-coordinate, sp += 2
	; OPT xor ah, ah
	; calculate pixel coordinate
	mov cl, 10
	mul cl
	mov bx, ax				; bx = x-coordinate * 10 = pixel x-coordinate
		
	pop ax
	xor ah, ah				; all value lies within al
	mov cx, 0d3200			; screen is 320 pixels wide
	mul cx					; screen pixel index calculation
	add ax, bx				; ax = 320 * ax + bx = offset in video memory to start drawing
	mov di, ax				; save offset
	
	pop ax					; color, sp += 2
	mov cx, 10				; draw 10 lines
	
	draw_block_outer_loop:
		mov bx, cx			; save outer loop counter
		mov cx, 10			; set innter loop counter, draw 10 pixels in a roll
		rep
		stosb 
			
		add di, 310			; shift to the next line
		mov cx, bx			; retrieve outer loop counter
		loop draw_block_outer_loop
	
	; OPT mov sp, bp				; point top of stack at EIP so that we can return to the main process
	sub sp, 8
	ret
	
game_over:
victory:					; win or lose, we all ends up here by the end

mov cx, 0x1E
mov dx, 0x8084
mov ah, 0x86
int 0x15					; wait for 2.0 seconds before restarting game
jmp start

; bug:
; mov cx, 0d64000
; mov di, 0x0000
; mov al, 0x05
; rep stosb

; padding
times 510-($-$$) db 0
dw 0xaa55
