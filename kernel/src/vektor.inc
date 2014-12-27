; ==========================================================================
; Univerzitet Union, Racunarski fakultet u Beogradu
; 08.2008. Operativni sistemi
; ==========================================================================
; RAF_OS -- Trivijalni skolski operativni sistem
; Aplikacioni programski interfejs (API)
;
; Vektori eksportovnaih sistemskih poziva nalaze se na statickim lokacijama.
; Ove lokacije ne treba menjati! 
; ---------------------------------------------------------------------------
; Inicijalna verzija 0.0.1 (Stevan Milinkovic, 20.08.2010.)
; ---------------------------------------------------------------------------

    jmp _main                 ; 0000h 
    jmp _print_string         ; 0003h
    jmp _move_cursor          ; 0006h
    jmp _clear_screen         ; 0009h
    jmp _print_horiz_line     ; 000Ch
    jmp _print_newline        ; 000Fh
    jmp _wait_for_key         ; 0012h
    jmp _check_for_key        ; 0015h
    jmp _int_to_string        ; 0018h
    jmp _speaker_tone         ; 001Bh
    jmp _speaker_off          ; 001Eh
    jmp _load_file            ; 0021h
    jmp _pause                ; 0024h
    jmp _fatal_error          ; 0027h
    jmp _draw_background      ; 002Ah
    jmp _string_length        ; 002Dh
    jmp _string_uppercase     ; 0030h
    jmp _string_lowercase     ; 0033h
    jmp _input_string         ; 0036h
    jmp _string_copy          ; 0039h
    jmp _dialog_box           ; 003Ch
    jmp _string_join          ; 003Fh
    jmp _get_dir              ; 0042h
    jmp _string_compare       ; 0045h
    jmp _string_chomp         ; 0048h
    jmp _string_strip         ; 004Bh
    jmp _string_truncate      ; 004Eh
    jmp _bcd_to_int           ; 0051h
    jmp _get_time_string      ; 0055h
    jmp _get_api_version      ; 0057h
    jmp _get_random           ; 005Ah
    jmp _get_date_string      ; 005Dh
    jmp _send_via_serial      ; 0060h
    jmp _get_via_serial       ; 0063h
    jmp _find_char_in_string  ; 0066h
    jmp _get_cursor_pos       ; 0069h
    jmp _print_space          ; 006Ch
    jmp _dump_string          ; 006Fh
    jmp _print_digit          ; 0072h
    jmp _print_1hex           ; 0075h
    jmp _print_2hex           ; 0078h
    jmp _print_4hex           ; 007Bh
    jmp _long_int_to_string   ; 007Eh
    jmp _long_int_negate      ; 0081h
    times 3 db 0              ;               Rezervisano
    times 3 db 0              ;               Rezervisano
    jmp _show_cursor          ; 008Ah
    jmp _hide_cursor          ; 008Dh
    jmp _dump_registers       ; 0090h
    jmp _string_strincmp      ; 0093h
    jmp _write_file           ; 0096h
    jmp _file_exists          ; 0099h
    jmp _create_file          ; 009Ch
    jmp _remove_file          ; 009Fh
    jmp _rename_file          ; 00A2h
    jmp _get_file_size        ; 00A5h
    times 3 db 0              ;               Rezervisano
    times 3 db 0              ;               Rezervisano
    jmp _string_reverse       ; 00AEh
    jmp _string_to_int        ; 00B1h
    jmp _draw_block           ; 00B4h
    jmp _get_random           ; 00B7h
    jmp _string_charchange    ; 00BAh
    jmp _serial_port_enable   ; 00BDh
    jmp _sint_to_string       ; 00C0h
    jmp _string_parse         ; 00C3h
    jmp _run_batch            ; 00C6h
    jmp _port_byte_out        ; 00C9h
    jmp _port_byte_in         ; 00CCh
	jmp _print_file			  ; 00CFh	