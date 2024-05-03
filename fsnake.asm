;Make snake self-moving ✔️
;No bugs ✔️
	;Make border deadly (border collision) ✔️
;bodyCollision ✔️
;Add score / high score
;Pause and Resume
;Add speed selector
	;Make apple not appear in a place where there's a snake
IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------
; Your variables here
; --------------------------
dir db 0
border db 0 ; 1 = exit (die

app dw 0       ;place of the apple
st_am dw 3
SPL dw 0, 0, 0  ;snake'sPlacesLocations

CODESEG
proc endGame ;displays end game message and Terminates program
	mov ax, 4c00h
	int 21h
 ret
endp endGame

proc black ;Paints the screen black & borders
;recives nothing, returns nothing
	push di ;blackBox
	push ax
	mov al, ' '
	mov ah, 0 ;black & and space
	mov di, 0
body:
	mov [es:di], ax
	add di, 2
	cmp di, 25*80*2
	jnz body
	pop ax
	pop di
 ret
endp black

proc apple ;put an apple on screen
;recives apple location, returns new apple location
;apple - 4
;IP
;bp...
	push bp ;blackBox
	mov bp, sp
    push ax
    push di
    push es
    
    mov ax, 40h
    mov es, ax ;set ES to timer/clock
    mov ax, [es:6ch]
    and ax, 0000001111111110b
;Make a loop that checks if the apple is in a location of any snake part
    push ax ;save for later use. To show the new apple on the screen
    mov di, [bp+4]
    mov [di], ax ;save the place of the apple
    mov ax, 0b800h
    mov es, ax
	
    mov al, '@' ;set ax to "apple mode"
    mov ah, 183
    pop di ;place of the new apple
    mov [es:di], ax ;put the "apple" on the screen
    
    pop es ;end blackBox
    pop di
    pop ax
    pop bp
 ret 2 ;Clear SG
endp apple

proc starterSnake ;Makes the first 3 snake parts show on screen & saves their places
;recives snake parts location, returns first 3 snake parts locations
;SPL - 4
;IP
;bp...
	push bp ;blackBox
	mov bp, sp
	push di
	push bx
	push ax
	
	mov bx, [bp+4]
	mov di, ((12*80+40)*2)-2 ;Head location
	mov [bx], di ;Save head location
	mov al, '*'
	mov ah, 0Ah
	mov [es:di], ax ;Put the head on screen
	add di, 2
	mov [bx+2], di ;Save 2nd part of initial snake
	mov [es:di], ax ;Put 2nd part of initial snake on screen
	add di, 2
	mov [bx+4], di ;Save 3rd part of initial snake
	mov [es:di], ax ;Put 3rd part of initial snake on screen
	
	pop ax ;End blackBox
	pop bx
	pop di
	pop bp
 ret 2 ;Clear SG
endp starterSnake

proc up ;Makes the snake move one step up
;Recives SPL, apple location & snake parts amount, returns new SPL, apple location & snake parts amount
;bodyCollision - 14
;direction - 12
;borderCheck - 10
;amount - 8
;apple - 6
;SPL - 4
;IP
;bp...
	push bp ;blackBox
	mov bp, sp
	push di
	push ax
	push bx
	
	mov di, [bp+4]
	mov di, [di]
	cmp di, 160
	jnb noDieUp
	call endGame
	
noDieUp:
;Give to bodyCollision SPL (snake's Part's Locations) and amount
	mov di, [bp+8]
	push di
	mov di, [bp+4]
	push di
	call bodyCollision
	
;Check if the head touches the apple (if so: call apple & inc st_am)
	mov di, [bp+4]
	mov di, [di]
	mov bx, [bp+6]
	mov bx, [bx]
	cmp di, bx
	jnz yesUp
;Initialize incrementing snake parts amount
	mov bx, [bp+8]
	mov di, [bx]
	inc di
	mov [bx], di
;Initialize calling apple
	mov di, [bp+6]
	push di ;Give to apple its location
	call apple
	
yesUp:
	mov di, [bp+4]
	push di ;Give to delete snake parts locations
	mov di, [bp+8]
	push di ;Give to delete snake parts amount
	call delete
	
	mov di, [bp+4]
	push di ;Give to move snake parts locations
	mov di, [bp+8]
	push di ;Give to move snake parts amount
	call move
	
	mov di, [bp+4]
	mov di, [di]
	sub	di, 160
	mov al, '*'
	mov ah, 0Ah
	mov [es:di], ax ;Show the new head on screen
	mov bx, [bp+4]
	mov [bx], di ;Save the new head location

noUp:
	pop bx ;end blackBox
	pop ax
	pop di
	pop bp
 ret
endp up

proc down ;Makes the snake move one step down
;Recives SPL, apple location & snake parts amount, returns new SPL, apple location & snake parts amount
;bodyCollision - 14
;direction - 12
;borderCheck - 10
;amount - 8
;apple - 6
;SPL - 4
;IP
;bp...
	push bp ;blackBox
	mov bp, sp
	push di
	push ax
	push bx
	
	mov di, [bp+4]
	mov di, [di]
	cmp di, (24*80*2)-1
	jng noDieDown
	call endGame
	
noDieDown:
;Give to bodyCollision SPL (snake's Part's Locations) and amount
	mov di, [bp+8]
	push di
	mov di, [bp+4]
	push di
	call bodyCollision
	
;Check if the head touches the apple (if so: call apple & inc st_am)
	mov di, [bp+4]
	mov di, [di]
	mov bx, [bp+6]
	mov bx, [bx]
	cmp di, bx
	jnz yesDown
;Initialize incrementing snake parts amount
	mov bx, [bp+8]
	mov di, [bx]
	inc di
	mov [bx], di
;Initialize calling apple
	mov di, [bp+6]
	push di ;Give to apple its location
	call apple
	
yesDown:
	mov di, [bp+4]
	push di ;Give to delete snake parts locations
	mov di, [bp+8]
	push di ;Give to delete snake parts amount
	call delete
	
	mov di, [bp+4]
	push di ;Give to move snake parts locations
	mov di, [bp+8]
	push di ;Give to move snake parts amount
	call move
	
	mov di, [bp+4]
	mov di, [di]
	add di, 160
	mov al, '*'
	mov ah, 0Ah
	mov [es:di], ax ;Show the new head on screen
	mov bx, [bp+4]
	mov [bx], di ;Save the new head location
	
noDown:
	pop bx ;end blackBox
	pop ax
	pop di
	pop bp
 ret
endp down

proc left ;Makes the snake move one step left
;Recives SPL, apple location & snake parts amount, returns new SPL, apple location & snake parts amount
;bodyCollision - 14
;direction - 12
;borderCheck - 10
;amount - 8
;apple - 6
;SPL - 4
;IP
;bp...
	push bp ;blackBox
	mov bp, sp
	push di
	push ax
	push dx
	push bx
	
	mov dx, 0
	mov bx, [bp+4]
	mov ax, [bx]
	mov di, 80*2
	div di
	cmp dx,0
	jnz noDieLeft
	call endGame
	
noDieLeft:
;Give to bodyCollision SPL (snake's Part's Locations) and amount
	mov di, [bp+8]
	push di
	mov di, [bp+4]
	push di
	call bodyCollision
	
;Check if the head touches the apple (if so: call apple & inc st_am)
	mov di, [bp+4]
	mov di, [di]
	mov bx, [bp+6]
	mov bx, [bx]
	cmp di, bx
	jnz yesLeft
;Initialize incrementing snake parts amount
	mov bx, [bp+8]
	mov di, [bx]
	inc di
	mov [bx], di
;Initialize calling apple
	mov di, [bp+6]
	push di ;Give to apple its location
	call apple
	
yesLeft:
	mov di, [bp+4]
	push di ;Give to delete snake parts locations
	mov di, [bp+8]
	push di ;Give to delete snake parts amount
	call delete
	
	mov di, [bp+4]
	push di ;Give to move snake parts locations
	mov di, [bp+8]
	push di ;Give to move snake parts amount
	call move
	
	mov di, [bp+4]
	mov di, [di]
	sub di, 2
	mov al, '*'
	mov ah, 0Ah
	mov [es:di], ax ;Show the new head on screen
	mov bx, [bp+4]
	mov [bx], di ;Save the new head location
	
noLeft:
	pop bx ;end blackBox
	pop dx
	pop ax
	pop di
	pop bp
 ret
endp left

proc right ;Makes the snake move one step right
;Recives SPL, apple location & snake parts amount, returns new SPL, apple location & snake parts amount
;bodyCollision - 14
;direction - 12
;borderCheck - 10
;amount - 8
;apple - 6
;SPL - 4
;IP
;bp...
	push bp ;blackBox
	mov bp, sp
	push di
	push ax
	push dx
	push bx
	
	mov dx, 0
	mov bx, [bp+4]
	mov ax, [bx]
	mov di, 80*2
	div di
	cmp dx, 158
	jnz noDieRight
	call endGame
	
noDieRight:
;Give to bodyCollision SPL (snake's Part's Locations) and amount
	mov di, [bp+8]
	push di
	mov di, [bp+4]
	push di
	call bodyCollision
	
;Check if the head touches the apple (if so: call apple & inc st_am)
	mov di, [bp+4]
	mov di, [di]
	mov bx, [bp+6]
	mov bx, [bx]
	cmp di, bx
	jnz yesRight
;Initialize incrementing snake parts amount
	mov bx, [bp+8]
	mov di, [bx]
	inc di
	mov [bx], di
;Initialize calling apple
	mov di, [bp+6]
	push di ;Give to apple its location
	call apple
	
yesRight:
	mov di, [bp+4]
	push di ;Give to delete snake parts locations
	mov di, [bp+8]
	push di ;Give to delete snake parts amount
	call delete
	
	mov di, [bp+4]
	push di ;Give to move snake parts locations
	mov di, [bp+8]
	push di ;Give to move snake parts amount
	call move
	
	mov di, [bp+4]
	mov di, [di]
	add di, 2
	mov al, '*'
	mov ah, 0Ah
	mov [es:di], ax ;Show the new head on screen
	mov bx, [bp+4]
	mov [bx], di ;Save the new head location
	
noRight:
	pop bx ;end blackBox
	pop dx
	pop ax
	pop di
	pop bp
 ret
endp right

proc delete ;Deletes the last part of the snake
;Recives SPL & amount, returns nothing
;SPL - 6
;amount - 4
;IP
;bp...
	push bp ;blackBox
	mov bp, sp
	push di
	push ax
	push bx
	
;Checking if there's even a snake
	mov di, [bp+4]
	mov di,[di]
	cmp di, 0
	jz noDel
;Initialize deleting the last part of the snake
	mov di, [bp+6]
	mov bx, [bp+4]
	mov bx, [bx]
	dec bx
	shl bx, 1
	mov di, [bx+di] ;Get to last snake part location
	mov al, ' '
	mov ah, 0
	mov [es:di], ax
	
noDel:
	pop bx ;end blackBox
	pop ax
	pop di
	pop bp
 ret 4 ;Clear SG
endp delete

proc move ;Move every snake part one spot to the right
;SPL - 6
;amount - 4
;IP
;bp...
	push bp ;blackBox
	mov bp, sp
	push di
	push si
	push bx
;Initialize getting to the last snake part
	mov bx, [bp+4]
	mov bx, [bx]
	dec bx
    shl bx, 1
	mov di, [bp+6]
;Initialize replacing every location with the one to its left
replace:
	mov si, [di+bx-2]
	mov [di+bx], si
	sub bx, 2
	jnz replace

	pop bx ;end blackBox
	pop si
	pop di
	pop bp
 ret 4
endp move

proc delay ;Delays snake movement
	push ax ;blackBox
	push cx

	mov cx, 0FFFFh
delay1:
	mov ax, 50
delay2:
	dec ax
	jnz delay2
	loop delay1
	
	pop cx ;end blackBox
	pop ax
 ret
endp delay

proc bodyCollision
;amount - 6
;SPL - 4
;IP
;bp
	push bp
	mov bp, sp
	push di
	push bx
	push si
	push ax
	push cx
	
	mov di, [bp+4]
	mov bx, [bp+6]
	mov cx, [bx] ;cx = amount
	xor ax, ax ;count
	mov si, di ;si = offset
	mov di, [di] ;di = head
	
checkBC:
	cmp cx, ax
	jz finCheck
	inc ax
	add si, 2
	mov bx, [si] ;bx = current Check
	cmp di, bx
	jnz checkBC
	call endGame
	
finCheck:
	pop cx
	pop ax
	pop si
	pop bx
	pop di
	pop bp
 ret 4
endp bodyCollision
start:
	mov ax, @data
	mov ds, ax
; --------------------------
; Your code here
; --------------------------
restart:
	mov ax, 0b800h
	mov es, ax
	
	mov di, 0
	mov bx, offset border
	mov [bx], di
	mov bx, offset dir
	mov [bx], di
	mov di, 3
	mov bx, offset st_am
	mov [bx], di
	
	call black
	
	mov di, offset app
	push di ;Give to apple apple location
	call apple
	
	mov di, offset SPL
	push di ;Give to starterSnake initial snake location
	call starterSnake
	
;Give to up/down/left/right SPL, apple & amount
	mov di, offset dir
	push di
	mov di, offset border
	push di
	mov di, offset st_am
	push di
	mov di, offset app
	push di
	mov di, offset SPL
	push di
	
wasd:
	mov ah, 1h
	int 16h
	jnz newDir ;Check if there's something in the buffer, if not continue
	
	call delay
   
	cmp [dir], 'q'
	jz exit
	cmp [dir], 'a'
	jz a
	cmp [dir], 's'
	jz s
	cmp [dir], 'd'
	jz d
	cmp [dir], 'w'
	jz w
	cmp [dir], 'r'
	jz restart
	
;recive new direction
newDir:
	mov ah, 0h
	int 16h
	mov [dir], al ;Change direction
	jmp wasd  
w:
    call up
	jmp wasd
s:
	call down
	jmp wasd	
a:
	call left
	jmp wasd
d:
	call right
	jmp wasd
	
exit:
	mov ax, 4c00h
	int 21h
END start