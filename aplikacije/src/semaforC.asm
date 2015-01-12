; ==================================================================
; Univerzitet Union, Racunarski fakultet u Beogradu
; 08.2008. Operativni sistemi
; ==================================================================
; RAF_OS -- Trivijalni skolski operativni sistem
;
; Dummy proces - tester za multitasking
; ------------------------------------------------------------------
; Inicijalna verzija 0.0.1 (Marko Bakovic, 04.01.2015)
; ------------------------------------------------------------------

%include "OS_API.inc"

	;call OS:_get_app_offset

petlja:

	call OS:_get_cursor_pos
	push dx

	mov si, 1h
	mov dh, 1
	mov dl, 30h
	mov di, 3
	mov bl, 10101010b
	call OS:_draw_block

	pop dx
	call OS:_move_cursor

	jmp petlja


Naslov db 'RAF_OS dummy', 13, 10, 0


