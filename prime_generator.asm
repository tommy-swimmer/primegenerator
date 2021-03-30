; CREDIT TO https://stackoverflow.com/questions/37605815/how-can-i-print-0-to-100-in-assembly-language-in-emu-8086
; for getting this code started.
; Identify Prime Numbers between 0 and n.
; TO-DO
; Eliminate duplicate numbers    
    
    .MODEL SMALL
     .STACK 100H
     .DATA
        NUM DW ?                                   
        lbk    db 13,10,'$'   ;LINE BREAK.
        numstr db '$$$$$'     ;STRING FOR 4 DIGITS.

     .CODE
     MAIN PROC
          MOV AX,@DATA
          MOV DS,AX

         MOV NUM, 2         ;FIRST NUMBER.
     START:
         CMP NUM, 120       ;IF NUM <= n...
         JBE PRINT          ;...DISPLAY NUM.           
         JMP END_

     PRINT:
;         MOV AH,2            ;THIS CODE
;         MOV DL,NUM          ;DISPLAYS
;         INT 21H             ;ONE CHAR ONLY.

     ;CONVERT NUMBER TO STRING.
         mov  si, offset numstr
         mov  ax, num
         call checkprimality
         call number2string    ;RETURNS NUMSTR.

     ;DISPLAY STRING.
         mov  ah, 9
         mov  dx, offset numstr
         int 21h     

     ;DISPLAY LINE BREAK.
         mov  ah, 9
         mov  dx, offset lbk
         int 21h     

         INC NUM              ;NUM++.
         JMP START
           
     END_:
         MOV Ax,4C00H
         int 21h
         MAIN ENDP

 
; CHECK PRIMALITY OF NUMBER, at the moment this will repeat prime numbers until a new one is found
checkprimality proc

    testprime: ; Testing multiples of 2
    mov bx, 2 ; Initial test if evenly divisible by two
    mov cx, 0 ; Loop counter, may be useful....
    mov dx, 0 
    push ax
    div bx
    push dx
    inc cx
    cmp dx,0 ; Compare remainder with 0. Meaning it's evenly divisible
    je notprime ; Jump to not prime if remainder is 0
    jmp testprime1
    jmp endcheck
    testprime1: ; Testing multiples of 3
    pop dx
    pop ax
    mov bx,3
    mov dx,0
    push ax
    div bx
    push dx
    cmp dx,0
    je notprime
    jmp testprime2
    jmp endcheck
    testprime2: ; Testing multiples of 5
    pop dx
    pop ax
    mov bx,5
    mov dx,0
    push ax
    div bx
    push dx
    cmp dx,0
    je notprime
    jmp testprime3
    jmp endcheck
    testprime3: ; Testing multiples of 7
    pop dx
    pop ax
    mov bx,7
    mov dx,0
    push ax
    div bx
    push dx
    cmp dx,0
    je notprime
    jmp endcheck
    testprime4: ; Testing multiples of 11
    pop dx
    pop ax
    mov bx,11
    mov dx,0
    push ax
    div bx
    push dx
    cmp dx,0
    je notprime
    jmp endcheck
    prime2:
    push ax
    push dx
    jmp endcheck 
    notprime:
    pop dx
    pop ax
    cmp ax,2
    je prime2
    cmp ax,3
    je prime2
    cmp ax,5
    je prime2
    cmp ax,7
    je prime2
    inc ax
    jmp testprime
    endcheck:
    pop dx
    pop ax
    ret
endp

     
 
;------------------------------------------
;CONVERT A NUMBER IN STRING.
;ALGORITHM : EXTRACT DIGITS ONE BY ONE, STORE
;THEM IN STACK, THEN EXTRACT THEM IN REVERSE
;ORDER TO CONSTRUCT STRING (STR).
;PARAMETERS : AX = NUMBER TO CONVERT.
;             SI = POINTING WHERE TO STORE STRING.

number2string proc
  call dollars ;FILL STRING WITH $.
  mov  bx, 10  ;DIGITS ARE EXTRACTED DIVIDING BY 10.
  mov  cx, 0   ;COUNTER FOR EXTRACTED DIGITS.
cycle1:       
  mov  dx, 0   ;NECESSARY TO DIVIDE BY BX.
  div  bx      ;DX:AX / 10 = AX:QUOTIENT DX:REMAINDER.
  push dx      ;PRESERVE DIGIT EXTRACTED FOR LATER.
  inc  cx      ;INCREASE COUNTER FOR EVERY DIGIT EXTRACTED.
  cmp  ax, 0   ;IF NUMBER IS
  jne  cycle1  ;NOT ZERO, LOOP. 
;NOW RETRIEVE PUSHED DIGITS.
cycle2:  
  pop  dx        
  add  dl, 48  ;CONVERT DIGIT TO CHARACTER.
  mov  [ si ], dl
  inc  si
  loop cycle2
      
  ret
number2string endp       

;------------------------------------------
;FILLS VARIABLE WITH '$'.
;USED BEFORE CONVERT NUMBERS TO STRING, BECAUSE
;THE STRING WILL BE DISPLAYED.
;PARAMETER : SI = POINTING TO STRING TO FILL.

proc dollars                 
  mov  cx, 5
  mov  di, offset numstr
dollars_loop:      
  mov  bl, '$'
  mov  [ di ], bl
  inc  di
  loop dollars_loop

  ret
endp  

;------------------------------------------



     END MAIN