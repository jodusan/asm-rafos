; --------------------------------------------------------------------------
; _init_scheduler -- Inicijalizuje scheduler tabele i postavlja shell kao
; jedini proces
; --------------------------------------------------------------------------
_init_scheduler: 
	mov byte [sch_active_proc], 0		; trenutno aktivan proces je shell ciji je pid = 0
	mov byte [sch_sizes], 1 			; shell procesu dodeljujemo 1kb (sch_sizes[0])
	mov word [sch_stacks], sp			; izdvajamo 28kB od 0FFFFh (dno SS-a) za druge procese, a shell dobije od 28kB na gore
	mov byte [sch_queue], 0 			; u queue ubacimo shell proces
	mov byte [sch_queue_size], 1
	mov dword [sch_mmt], 1   			; svi memorijski segmenti osim nultog su slobodni (u nuli je shell)
	ret

; --------------------------------------------------------------------------
; _ubaci_proces -- Ucitava proces u memoriju
; Ulaz: AX = datoteka;
; Izlaz: CF=1 ukoliko nema trazene datoteke ili nije moguce ucitati datoteku u
; memoriju verovatno zato sto nema dovoljno memorije
; --------------------------------------------------------------------------
_ubaci_proces:	; TODO: PUSH/POP registre
	pusha
	push ax
	clc
	call _get_file_size
	clc
	jnc .nadjen_fajl					; ako je cf = 1 fajl ne postoji
	pop ax
	popa
	ret
.nadjen_fajl:
	call _dump_registers
	mov ax, bx							; pretvaramo velicinu u kb
	xor dx, dx
	mov bx, 1024
	div bx
	cmp dx, 0
	je	.nastavi
	inc ax
	call _dump_registers
.nastavi:
	mov cx, 1
	mov	bx, 0           				; brojac slobodnih memorijskih jedinica do cx-te
.petlja:
	push bx
	mov	bx, 1
	shl bx, cl
	test [sch_mmt], bx					; testiramo cx-ti bit u mmt-u
	jnz .resetuj_brojac
	pop bx
	inc bx
	cmp bx, ax
	jl .sledeci_bit
										; proces ucitavamo na lokacije [cx - ax(=bx) + 1, cx], a njegov pid je cx - ax + 1
	sub cx, ax
	inc cx								; na cx izracunamo pid								
	jmp .nadjen_pid						
.resetuj_brojac
	pop bx
	mov bx, 0
.sledeci_bit:
	inc cx
	cmp cx, 29
	jl .petlja
	stc
	pop ax
	popa 								; postavimo CF jer nismo nasli slobodnu memoriju
	ret 
.nadjen_pid:
	call _update_scheduler
	call _dump_registers
	pop ax
	popa
	mov bx, cx
	shl bx, 10
	add bx, 8000h
	mov cx, bx
	call _load_file
	ret

; --------------------------------------------------------------------------
; _update_scheduler -- Update-uje sch_mmt, sch_sizes, sch_queue... 
; Ulaz: AX = velicina, CL = pid procesa
; --------------------------------------------------------------------------
_update_scheduler:
	pusha

	xor ch, ch

	; update memory management table
	mov bx, 1
	xchg ax, cx
	shl bx, cl
	xchg ax, cx
	dec bx 								; 2^ax - 1
	shl bx, cl
	or 	[sch_mmt], bx
	
	; update sch_sizes
	mov si, sch_sizes
	add si, cx
	mov byte [si], al

	; izracunaj adresu za stack pointer
	mov bx, ax
	add bx, cx
	dec bx
	shl bx, 10
	add bx, 08FFFh

	sub bx, 2 							; odvojimo 2 bajta na dnu stack-a za labalu _izbaci_proces na koju ce proces da skoci nakon ret-a
	mov word [bx], word _izbaci_proces 

	; update sch_stacks
	mov si, sch_stacks
	add si, cx
	mov word [si], bx
	
	cli									; Zabrani prekide. 
	mov al, 080h						; Zabrani NMI prekide
	out 070h, al 	

	mov si, sch_queue
	add si, [sch_queue_size]
	mov [si], cx
	inc byte [sch_queue_size]
	
	xor al, al							; Dozvoli NMI prekide
	out 070h, al						; Dozvoli prekide
	sti

	popa
	ret

; --------------------------------------------------------------------------
; _izbaci_proces -- Brise proces pid iz queue-a, update-uje mmt i ostalo 
; --------------------------------------------------------------------------
_izbaci_proces:
	ret

sch_test db 'Pa cao iz schedulera!', 13, 10, 0
sch_queue times 32 db 0					; queue pid-ova koji cekaju na izvrsavanje
sch_queue_size db 0						; broj pid-ova u queue (broj aktivnih procesa)
sch_stacks times 32 dw 0				; stack pointeri na procese, primer sch_stacks[1] je stack pointer ciji je pid = 1
sch_sizes times 32 db 0					; velicine procesa u kb, primer sch_sizes[1] je velicina procesa ciji je pid = 1
sch_active_proc db 0					; trenutno aktivan proces
sch_mmt dw 0   							; 28 bitova najmanje tezine odredjuju zauzete memorijske prostore