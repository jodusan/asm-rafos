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
org app_main
    mov si, Naslov
    call _print_string
    ret

Naslov  db 'RAF_OS dummy', 13, 10, 0


