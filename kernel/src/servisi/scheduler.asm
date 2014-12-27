_init_scheduler: 
	mov si, sch_test
	call _print_string
	ret

sch_test db 'Pa cao iz schedulera!', 13, 10, 0