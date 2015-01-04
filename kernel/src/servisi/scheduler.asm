; =============================================================================
; Univerzitet Union, Racunarski fakultet u Beogradu
; 08.2008. Operativni sistemi
; =============================================================================
; RAF_OS -- Trivijalni skolski operativni sistem
; Scheduler - RAF_OS multitasking 
;
; 
; Pogledati u prekidi.asm scheduler rutinu na int 08h.
; -----------------------------------------------------------------------------
; Inicijalna verzija 0.0.1 (Marko Bakovic, Sasa Vuckovic, Dusan Josipovic, 01.01.2015.).
;
; -----------------------------------------------------------------------------




; --------------------------------------------------------------------------
; _init_scheduler -- Inicijalizuje scheduler tabele i postavlja shell kao
; jedini proces
; --------------------------------------------------------------------------
_init_scheduler: 
	mov byte [sch_active_proc], 0		; trenutno aktivan proces je shell ciji je pid = 0
	mov byte [sch_sizes], 1 			; shell procesu dodeljujemo 1kb (sch_sizes[0])
	mov word [sch_stacks], sp			; izdvajamo 28kB od 0FFFFh (dno SS-a) za druge procese,
										; a shell dobije od 28kB na gore
	mov byte [sch_queue], 0 			; u queue ubacimo shell proces
	mov byte [sch_queue_size], 1
	mov byte [sch_mmt], 0FFh   			; svi memorijski segmenti osim nultog su slobodni (u nuli je shell)
	push ds  							; sacuvamo sadrzaj ds-a jer ga int 08h menja u 040h 
	pop gs 

	ret

; --------------------------------------------------------------------------
; _ubaci_proces -- Ucitava proces u memoriju
; Ulaz: AX = datoteka;
; Izlaz: CF=1 ukoliko nema trazene datoteke ili nije moguce ucitati datoteku u
; memoriju verovatno zato sto nema dovoljno memorije
; --------------------------------------------------------------------------
_ubaci_proces:
	pusha
	push ax 							; sacuvamo ime datoteke na stack-u

	; odredimo velicinu fajla
	xor bx, bx
	clc
	call _get_file_size
	cmp bx, 0
	jne .nadjen_fajl

	pop ax
	popa
	ret
.nadjen_fajl:
	mov ax, bx							; pretvaramo velicinu u kb
	xor dx, dx							; cistimo dx registar jer zelimo da delimo
	mov bx, 1024 						; dx(prazan):ax(velicina u bajtovima) sa bx(1024)
	div bx 								; da dobijemo broj kilobajtova u ax
	cmp dx, 0 							; ako ne postoji ostatak kul idemo dalje
	je	.sch_nastavi 						
	inc ax 								; ako postoji samo povecaj ax da bi mu dali dovoljno mesta
.sch_nastavi:
	mov cx, 1
	mov	bx, 0           				; brojac slobodnih memorijskih jedinica do cx-te
.petlja:
	mov	si, cx
	add si, sch_mmt
	cmp byte [si], 0					; testiramo cx-ti bajt u mmt-u
	jne .resetuj_brojac					; ako nije slobodno skoci na resetuj brojac 

	inc bx
	cmp bx, ax
	jl .sledeci_bajt
										; proces ucitavamo na lokacije [cx - ax(=bx) + 1, cx],
										; a njegov pid je cx - ax + 1
	sub cx, ax
	inc cx								; na cx izracunamo pid								
	jmp .nadjen_pid						
.resetuj_brojac
	xor bx, bx
.sledeci_bajt:
	inc cx
	cmp cx, 29
	jl .petlja
	mov si, sch_no_memory_error
	call _print_string
	stc
	pop ax
	popa 								; postavimo CF jer nismo nasli slobodnu memoriju
	ret 
.nadjen_pid:
	mov dx, ax
	pop ax

	push dx 							; ubacimo velicinu na stack
	push cx 							; ubacimo pid na stack

	mov bx, cx 							; na bx izracunamo gde program treba da se ucita
	shl bx, 10
	add bx, 8000h
	mov cx, bx 							; pomerimo adresu na cx jer se tako prosledjuje u _load_file_current_folder
	xor bx, bx

	call _load_file_current_folder		; ucitamo program u memoriju

	pop cx 								; na cx vratimo pid
	pop ax 								; na ax vratimo velicinu
	call _update_scheduler

	;DEBUG
	call _dbg_dump

	popa
	ret

; --------------------------------------------------------------------------
; _update_scheduler -- Update-uje sch_mmt, sch_sizes, sch_queue...
;  Ucitava program u memoriju 
; Ulaz: AX = velicina, CX = pid procesa
; --------------------------------------------------------------------------
_update_scheduler:
	pusha

	; update memory management table
	mov si, cx
	add si, sch_mmt
	push cx
	mov cx, ax
.petlja:
	mov	byte [si], 0FFh
	inc si
	loop .petlja
	pop cx

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

	; privremeno zameni stack
	mov dx, sp
	mov sp, bx

	; izracunaj adresu gde ce program biti ucitan
	mov bx, cx
	shl bx, 10
	add bx, 8000h

	; izracunaj cs koji cemo pushnut'
	;push ax
	;mov ax, cx
	;shl ax, 6								; cs = ( 8000h + pid * 400h ) / 10h
	;add ax, 800h							; skraceno: cs = 800h + pid * 40h
	
	; ubaci predefinisane vrednosti na stack novog procesa
	push _izbaci_proces 					; ubaci adresu na koju ce proces da skoci nakon ret-a
	pushf								
	push cs 								; ovo je u stvari izracunati cs
	push bx									; ubacimo prethodno izracunatu adresu na ip (odakle ce se skidati ip)
	push ax
	push bx
	push cx
	push dx
	push bp
	push si
	push di

	;pop ax 									; vrati ax

	; vrati stack i na bx pomeri adresu stack-a novog procesa (na koji su ubacene predefinisane vrednosti)
	mov bx, sp
	mov sp, dx

	; update sch_stacks
	mov si, sch_stacks
	add si, cx
	add si, cx
	mov word [si], bx
	
	cli									; Zabrani prekide. 
	mov al, 080h						; Zabrani NMI prekide
	out 070h, al 	

	mov si, sch_queue
	xor ax, ax
	mov al, byte [sch_queue_size]
	add si, ax							; u niz sch_queue, na sch_queue+sch_queue_size dodaj novi
	mov byte [si], cl 					; proces sa pidom cx

	inc byte [sch_queue_size] 			; povecavamo velicinu queue-a
	
	xor al, al							; Dozvoli NMI prekide
	out 070h, al						; Dozvoli prekide
	sti

	popa
	ret

; --------------------------------------------------------------------------
; _izbaci_proces -- Brise proces pid iz queue-a, update-uje mmt i ostalo 
; --------------------------------------------------------------------------
_izbaci_proces:
	xor ch, ch
	mov cl, byte [sch_active_proc]		; na cx vratimo pid
	
	mov si, sch_sizes
	add si, cx
	xor ah, ah
	mov al, byte [si]					; na ax vratimo velicinu

	; update sch_sizes
	mov si, sch_sizes
	add si, cx
	mov byte [si], 0

	; update memory management table
	mov si, cx
	add si, sch_mmt
	push cx
	mov cx, ax

	cli									; Zabrani prekide. 
	mov al, 080h						; Zabrani NMI prekide
	out 070h, al 	

.petlja:								; ocisti mmt
	mov byte [si], 0
	inc si
	loop .petlja

	pop cx

	dec byte [sch_queue_size]			; proces koji se izvrsava je na kraju queue-a,
										; tako da samo smanjimo qeueu_size

	xor al, al							; Dozvoli NMI prekide
	out 070h, al						; Dozvoli prekide
	sti

	;DEBUG
	call _dbg_dump
	jmp $								; cekamo prekidnu rutinu za scheduler

	ret

sch_no_memory_error db 'Nema dovoljno memorije!', 13, 10, 0
sch_active_proc db 0					; trenutno aktivan proces
sch_sizes times 32 db 0					; velicine procesa(kB), primer sch_sizes[1] 
										; je velicina procesa ciji je pid = 1
sch_stacks times 32 dw 0				; stack pointeri procesa, primer sch_stacks[1] je sp ciji je pid = 1
sch_queue times 32 db 0					; queue pid-ova koji cekaju na izvrsavanje
sch_queue_size db 0						; broj pid-ova u queue (broj aktivnih procesa)
sch_mmt times 32 db 0   				; tabela zauzetih memorijskih prostora


; --------------------------------------------------------------------------
; DEBUG
; --------------------------------------------------------------------------

_dbg_string_start 	db '---- DEBUG INFO -----', 13, 10, 0
_dbg_active_proc 	db 'sch_active_proc: ', 0
_dbg_sizes_content  db 'sch_sizes: ', 0
_dbg_queue_content  db 'sch_queue: ', 0
_dbg_queue_size 	db 'sch_queue_size: ', 0
_dbg_mmt_hex 		db 'mmt_hex: ', 0
_dbg_stacks 		db 'sch_stacks: ', 0
_dbg_string_end 	db '---- DEBUG INFO END -----', 13, 10, 0


_dbg_dump:
	pusha
	call _print_newline
	mov si, _dbg_string_start
	call _print_string
	
	; active process string: value
	mov si, _dbg_active_proc
	call _print_string
	xor ax,ax
	mov  al, byte [sch_active_proc]
	call _print_digit
	call _print_newline

	; sizes content petlj
	mov si, _dbg_sizes_content
	call _print_string
	mov cx, 32
	mov si, sch_sizes

	.dbg_petlja2:
	mov al, byte [si]
	call _print_2hex
	inc si
	loop .dbg_petlja2

	call _print_newline

	; queue content petlj
	mov si, _dbg_queue_content
	call _print_string
	xor ch, ch
	mov cl, byte [sch_queue_size]
	mov si, sch_queue

	.dbg_petlja:
	mov al, byte [si]
	call _print_2hex
	inc si
	loop .dbg_petlja

	;mov al, byte[sch_queue + 1]
	;call _print_2hex

	call _print_newline	

	; queue size sring: value
	mov si, _dbg_queue_size
	call _print_string
	xor ax, ax
	mov al, byte [sch_queue_size]
	call _print_digit
	call _print_newline

	; mmt string: value (u hexu) - littleendian-bigendian 
	mov si, _dbg_mmt_hex
	call _print_string
	mov cx, 32
	mov si, sch_mmt

	.dbg_petlja3:
	mov al, byte [si]
	call _print_2hex
	inc si
	loop .dbg_petlja3

	call _print_newline	

	mov si, _dbg_string_end
	call _print_string
	call _print_newline
	popa
	ret

_dbg_dump_stacks:
	pusha

	call _print_newline

	mov si, _dbg_string_start
	call _print_string

	mov si, _dbg_stacks
	call _print_string

	mov si, sch_stacks
	xor ch, ch
	mov cl, byte [sch_queue_size]
.petlja:
	mov ax, word [si]
	call _print_4hex
	call _print_space
	inc si
	inc si
	loop .petlja 

	call _print_newline	

	mov si, _dbg_string_end
	call _print_string

	call _print_newline	

	popa
	ret