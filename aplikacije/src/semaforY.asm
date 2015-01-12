; ==================================================================
; Univerzitet Union, Racunarski fakultet u Beogradu
; 08.2008. Operativni sistemi
; ==================================================================
; RAF_OS -- Trivijalni skolski operativni sistem
;
; SemaforR proces - tester za multitasking
; ------------------------------------------------------------------
; Inicijalna verzija 0.0.1 (Sasa Vuckovic, 12.01.2015)
; ------------------------------------------------------------------

%include "OS_API.inc"

petlja:

	mov si, 1h
	mov dh, 0
	mov dl, 4Fh
	mov di, 1
	mov bl, 11101110b
	push dx

	cli									; Zabrani prekide. 
	mov al, 080h						; Zabrani NMI prekide
	out 070h, al

	call OS:_get_cursor_pos
	mov ax, dx
	pop dx
	push ax
	
	call OS:_draw_block
	pop dx
	call OS:_move_cursor

	xor al, al							; Dozvoli NMI prekide
	out 070h, al						; Dozvoli prekide
	sti


	jmp petlja


