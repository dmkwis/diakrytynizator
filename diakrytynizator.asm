; Default value for syscall of SYS_EXIT.
SYS_EXIT equ 60

; Given Modulo constant.
MODULO equ 0x10FF80


; Sizes of input and output buffers with maximal amout of bytes
; written into them (exceeding those constants will refill buffers 
; during next reading or writing).
SIZE_OF_INPUT_BUFFER equ 2048
SIZE_OF_MAX_READ equ 2044
SIZE_OF_OUTPUT_BUFFER equ 2048
SIZE_OF_MAX_WRITE equ 2044

; Thresholds for utf-8 encoding
; Upper-bounds for first bytes of given utf-8 character
; If first byte is less than one value and bigger or equal to previous one
; then it represents correspoding type of utf-8 character.
ONE_BYTE_CHARACTER_THRESHOLD equ 0b10000000
ERROR_CHARACTER_THRESHOLD equ 0b11000000
TWO_BYTE_CHARACTER_THRESHOLD equ 0b11100000
THREE_BYTE_CHARACTER_THRESHOLD equ 0b11110000
FOUR_BYTE_CHARACTER_THRESHOLD equ 0b11111000

; Constants that represent maximal value of unicode
; for utf-8 characters coded in corresponding number of bytes.
MAXIMAL_ONE_BYTE equ 0x7F
MAXIMAL_TWO_BYTE equ 0x7FF
MAXIMAL_THREE_BYTE equ 0xFFFF
MAXIMAL_FOUR_BYTE equ 0x10FFFF

; Constants representing proper byte format in utf-8 encoding
; following byte is defined as byte that takes part in coding utf-8 character
; but is not the first one.
PROPER_FOLLOWING_BYTE_FORMAT equ 0b10000000 ;10xxxxxx
PROPER_FOUR_BYTE_FORMAT equ 0b11110000 ;11110xxx
PROPER_THREE_BYTE_FORMAT equ 0b11100000 ;1110xxx
PROPER_TWO_BYTE_FORMAT equ 0b11000000 ;110xxxxx

; Following 16 constants represent state of one byte
; reading from the least significant bit
EIGHT_ONES equ 0xFF ; 0b11111111
SEVEN_ONES equ 0x7F ; 0b01111111
SIX_ONES equ 0x3F ; 0b00111111 and so on...
FIVE_ONES equ 0x1F
FOUR_ONES equ 0xF
THREE_ONES equ 0x7
TWO_ONES equ 0x3
ONE_ONES equ 0x1

EIGHT_ZEROS equ 0x0 ; 0b00000000
SEVEN_ZEROS equ 0x80 ; 0b10000000
SIX_ZEROS equ 0xC0 ; 0b1100000 and so on...
FIVE_ZEROS equ 0xE0
FOUR_ZEROS equ 0xF0
THREE_ZEROS equ 0xF8
TWO_ZEROS equ 0xFC
ONE_ZEROS equ 0xFE

; Macro for exiting with value given in its argument
%macro exit 1
        mov rax, SYS_EXIT
        mov rdi, %1
        syscall
%endmacro

section .bss
        input_buffer resb SIZE_OF_INPUT_BUFFER ; Pointer to an array in which input bytes will be stored
        output_buffer resb SIZE_OF_OUTPUT_BUFFER ; Pointer to an array in which output bytes will be stored
        processed_input_bytes resq 1 ; Pointer to an integer representing number of processed bytes in input_buffer
        bytes_in_buffer resq 1 ; Pointer to an integer representing number of not processed bytes in input_buffer
        used_output_bytes resq 1 ; Pointer to an integer representing how many bytes in output_buffer are filled
        coeff_num resq 1 ; Pointer to an integer representing how many polynomial coefficients are given
        how_many_bytes_read resq 1; Pointer to an integer repesenting how many bytes were read to input buffer

section .text
        global _start

; Function reading set number of bytes to input_buffer
; Ends program if there are no more bytes to read by jumping to _start_end_of_reading
; Used registers: rax, rdi, rsi, rdx, r15
_read_input:
        xor rax, rax
        xor rdi, rdi
        mov rsi, input_buffer
        mov rdx, SIZE_OF_MAX_READ
        syscall
        cmp rax, 0
        je _start_end_of_reading
        mov [how_many_bytes_read], rax
        mov [bytes_in_buffer], rax
        mov r15, 0
        mov [processed_input_bytes], r15
        cmp rax, 0
        jne _read_input_return
_read_input_return:
        ret

; Function that returns next character from input
; Sets values at [processed_input_bytes] and [bytes_in_buffer]
; Used registers: rax, rbx, r15, rdi, rsi, rdx
; Return values:
; rax - next character
; rbx - number of bytes (in utf-8 encoding) of returned character
_get_character:
        mov r15, 0
        cmp [bytes_in_buffer], r15
        jg _get_character_with_nonempty_buffer 
        call _read_input ; If there are no more bytes in buffer read new ones.
_get_character_with_nonempty_buffer:
        mov rbx, [processed_input_bytes]
        mov al, [input_buffer + rbx]
                                                ; Process character depending on number of bytes
                                                ; in its utf-8 encoding.
        cmp al, ONE_BYTE_CHARACTER_THRESHOLD
        jb _one_byte_character

        cmp al, ERROR_CHARACTER_THRESHOLD
        jb _character_error

        cmp al, TWO_BYTE_CHARACTER_THRESHOLD
        jb _two_byte_character

        cmp al, THREE_BYTE_CHARACTER_THRESHOLD
        jb _three_byte_character

        cmp al, FOUR_BYTE_CHARACTER_THRESHOLD
        jb _four_byte_character

        jmp _character_error
_get_character_return:
        add [processed_input_bytes], rbx
        sub [bytes_in_buffer], rbx
        ret

; Function that is called if first byte is wrong.
; Prints output_buffer buffer and ends program.
_character_error:
        call _print_buffer
        exit 1

; Function checks if byte at al is proper encoding of certain utf-8 character byte,
; at r9 there is a number representing what kind of byte will be checked
; if r9 is equal to 1 then proper following byte formatting is checked
; if r9 is equal to 2 then proper first byte of two byte utf-8 character is checked
; if r9 is equal to 3 then proper first byte of three byte utf-8 character formatting is checked
; if r9 is equal to 4 then proper first byte of four byte utf-8 character formatting is checked
; if byte is in incorrect formatting then function _wrong_utf8_encoding is called
; otherwise does nothing
; used registers: r14, rax, r9
_check_byte_correctness:
        cmp r9, 1
        je _check_byte_correctness_following_byte
        cmp r9, 2
        je _check_byte_correctness_first_2byte
        cmp r9, 3
        je _check_byte_correctness_first_3byte
        cmp r9, 4
        je _check_byte_correctness_first_4byte
        ret
; Following fuctions check proper formatting of byte at al
; Used registers: r14
_check_byte_correctness_following_byte:
        mov r14b, al
        and r14b, SIX_ZEROS
        cmp r14b, PROPER_FOLLOWING_BYTE_FORMAT
        jne _wrong_utf8_encoding
        jmp _check_byte_correctness_return

_check_byte_correctness_first_2byte:
        mov r14b, al
        and r14b, FIVE_ZEROS
        cmp r14b, PROPER_TWO_BYTE_FORMAT
        jne _wrong_utf8_encoding
        jmp _check_byte_correctness_return

_check_byte_correctness_first_3byte:
        mov r14b, al
        and r14b, FOUR_ZEROS
        cmp r14b, PROPER_THREE_BYTE_FORMAT
        jne _wrong_utf8_encoding
        jmp _check_byte_correctness_return

_check_byte_correctness_first_4byte:
        mov r14b, al
        and r14b, THREE_ZEROS
        cmp r14b, PROPER_FOUR_BYTE_FORMAT
        jne _wrong_utf8_encoding
        jmp _check_byte_correctness_return

_check_byte_correctness_return:
        ret

; Function that checks if following bytes are propperly format
; as well as processes their unicode value in r15
; at rax there is a character in utf-8 format
; at rcx there is a number of following bytes
; used registers: r9, rax, rcx, r15, r14
_following_bytes_format_and_correctness:
        mov r9, 1
_following_bytes_format_and_correctness_loop:
        shr rax, 8
        call _check_byte_correctness
        and al, SIX_ONES
        shl r15, 6
        add r15b, al
        dec rcx
        cmp rcx, 0
        jne _following_bytes_format_and_correctness_loop
        ret

; Following functions change utf-8 formatting to unicode, depending
; on the number of bytes used to do utf-8 encoding.
; They also check if character is properly encoded, if it is not
; then function _wrong_utf8_encoding is called, else 
; used registers: rax, r15, rbx, rcx, r14, r9, rdi, rsi, rdx
; Return values:
; rax - unicode value of encoded character
; rbx - number of bytes used in its utf-8 encoding
_one_byte_character:
        xor rax, rax

        mov r15, [processed_input_bytes]
        mov al, [input_buffer + r15]

        mov rbx, 1
        jmp _get_character_return

_two_byte_character:
        mov rcx, 1
        call _get_rest_of_bytes

        mov r15, [processed_input_bytes]
        mov rax, [input_buffer + r15]

        xor r15, r15

        mov r9, 2
        call _check_byte_correctness
        
        mov r15b, al
        and r15b, FIVE_ONES

        mov rcx, 1
        call _following_bytes_format_and_correctness

        cmp r15, MAXIMAL_ONE_BYTE
        jbe _wrong_utf8_encoding 

        mov rax, r15
        mov rbx, 2
        jmp _get_character_return

_three_byte_character:
        mov rcx, 2
        call _get_rest_of_bytes

        mov r15, [processed_input_bytes]
        mov rax, [input_buffer + r15]

        xor r15, r15

        mov r9, 3
        call _check_byte_correctness
        
        mov r15b, al
        and r15b, FOUR_ONES

        mov rcx, 2
        call _following_bytes_format_and_correctness

        cmp r15, MAXIMAL_TWO_BYTE
        jbe _wrong_utf8_encoding

        mov rax, r15
        mov rbx, 3
        jmp _get_character_return

_four_byte_character:
        mov rcx, 3
        call _get_rest_of_bytes

        mov r15, [processed_input_bytes]
        mov rax, [input_buffer + r15]

        xor r15, r15

        mov r9, 4
        call _check_byte_correctness
        
        mov r15b, al
        and r15b, THREE_ONES

        mov rcx, 3
        call _following_bytes_format_and_correctness

        cmp r15, MAXIMAL_THREE_BYTE
        jbe _wrong_utf8_encoding

        cmp r15, MAXIMAL_FOUR_BYTE
        ja _wrong_utf8_encoding

        mov rax, r15
        mov rbx, 4
        jmp _get_character_return

; This function checks if there are rcx free bytes in input_buffer
; If there are it doesn't do anything, otherwise it reads next bytes to input_buffer
; (there is a place in buffer because of used constants).
; If not enough bytes are read _wrong_utf8_encoding function is called.
; Used registers: rcx, rax, rdi, rsi, rdx
_get_rest_of_bytes:
        inc rcx
        cmp rcx, [bytes_in_buffer]
        jb _get_rest_of_bytes_return

        xor rax, rax
        xor rdi, rdi
        mov rsi, input_buffer
        add rsi, [how_many_bytes_read]
        mov rdx, rcx
        sub rdx, [bytes_in_buffer]
        push rdx

        syscall

        pop rdx
        cmp rax, rdx
        jne _wrong_utf8_encoding

_get_rest_of_bytes_return:
        ret

; This function is called when wrong utf-8 encoding is encountered.
; Prints buffer and ends program with exitcode 1.
_wrong_utf8_encoding:
        call _print_buffer
        exit 1

; Function that is called when there are not enough command line arguments in given instance
_not_enough_args:
        exit 1

; Main function of program
; It does a preprocessing of default values that are set
; Check command line arguments corectness and changes them into integers
; Process characters and writes them to output_buffer
; When _start_loop finishes it calls _print_buffer function and ends program
_start:
        mov r15, 0 ; Setting default values of variables used in whole program
        mov [how_many_bytes_read], r15
        mov [processed_input_bytes], r15
        mov [bytes_in_buffer], r15
        mov [used_output_bytes], r15
        pop r12 ; Handles command line arguments 
        sub r12, 1
        cmp r12, 0
        je _not_enough_args ; Checks if there are enough arguments
        mov [coeff_num], r12 ; If there are then it stores the number of supposed polynomial coeffictiens
        add rsp, 8 ; Moves register pointer (omits first command line argument)
        mov r12, rsp ; Moves stack pointer to polynomial coeffictients to r12 (this register is never changed throughout the program)
        call _convert_polynomial_coefficients
_start_loop:
        call _get_character ; Reads character
        cmp rbx, 1
        je _start_loop_write_to_output_buffer ; If the character is using only one byte in utf-8 encoding 
                                              ; just put it to output buffer
        mov rdi, rax  ; If it uses more perform _horner_method on its value
        sub rdi, 0x80
        call _horner_method
        add rax, 0x80
        call _convert_to_utf8 ; And convert it back to utf-8 (and write it to output_buffer)
_start_loop_write_to_output_buffer:  ; Writes character to output_buffer and continues _start_loop
        call _write_to_output_buffer
        jmp _start_loop
_start_end_of_reading: ; If there are no more characters print out the output_buffer and finish the program
        call _print_buffer
        exit 0

; Function is used to properly turn following bytes into utf-8 encoding
_following_bytes_to_utf8:
        mov r9, 1
        xor r14, r14
        mov r13, rax
_following_bytes_to_utf8_loop:
        mov r15, r13
        and r15, SIX_ONES
        or r15, SEVEN_ZEROS
        add r14, r15
        shl r14, 8
        shr r13, 6
        inc r9
        cmp r9, rbx
        jb _following_bytes_to_utf8_loop
        ret

; Converts unicode at rax to utf-8
; Used registers: rax, rbx, r14, r15, r9
; Return values:
; rax - utf-8 encoding
; rbx - used bytes in  utf-8 encoding
_convert_to_utf8:
        cmp rax, MAXIMAL_ONE_BYTE
        jbe _convert_to_utf8_one_byte
        cmp rax, MAXIMAL_TWO_BYTE
        jbe _convert_to_utf8_two_byte
        cmp rax, MAXIMAL_THREE_BYTE
        jbe _convert_to_utf8_three_byte
        
        jmp _convert_to_utf8_four_byte

_convert_to_utf8_one_byte:
        mov rbx, 1
        ret
_convert_to_utf8_two_byte:
        mov rbx, 2
        
        call _following_bytes_to_utf8
        
        mov r15, rax
        shr r15, 6
        or r15, SIX_ZEROS
        add r14, r15

        mov rax, r14

        jmp _convert_to_utf8_return
_convert_to_utf8_three_byte:
        mov rbx, 3

        call _following_bytes_to_utf8

        mov r15, rax
        shr r15, 12
        or r15, FIVE_ZEROS
        add r14, r15

        mov rax, r14

        jmp _convert_to_utf8_return
_convert_to_utf8_four_byte:
        mov rbx, 4

        call _following_bytes_to_utf8

        mov r15, rax
        shr r15, 18
        or r15, FOUR_ZEROS
        add r14, r15

        mov rax, r14

        jmp _convert_to_utf8_return
_convert_to_utf8_return:
        ret

; Function used to print out bytes in output_buffer
; Used registers: rax, rdi, rsi, rdx, r15
_print_buffer:
        mov rax, 1
        mov rdi, 1
        mov rsi, output_buffer
        mov rdx, [used_output_bytes]
        syscall
        mov r15, 0
        mov [used_output_bytes], r15
        ret

; Function used to write bytes into output_buffer.
; Bytes at rax.
; Number of bytes at rbx.
; Used registers: rax, rdi, rsi, rdx, r15, rbx, r14
_write_to_output_buffer:
        push rax
        mov r15, [used_output_bytes]
        add r15, rbx
        mov r14, SIZE_OF_MAX_WRITE
        cmp r15, r14
        jbe _write_to_output_buffer_not_full_buffer
        call _print_buffer ; If there isn't enough place in output_buffer print the buffer and start filling
                           ; it from the beggining.
_write_to_output_buffer_not_full_buffer:
        pop rax 
        mov r15, rax
        mov r14, 1
        mov rax, [used_output_bytes]
_write_to_output_buffer_loop: ; Put bytes in correct order in the buffer
        mov [output_buffer + rax + r14 - 1], r15b
        shr r15, 8
        inc r14
        cmp r14, rbx
        jbe _write_to_output_buffer_loop
        add [used_output_bytes], rbx ; Set the value of used_output_bytes
        ret

; Converts polynomial coeffictients on from cstrings to integers modulo MODULO
; At r12 address of coefficients (on stack).
; Used registers: r11, rax, rdi, rdx, r8, r9, r10
_convert_polynomial_coefficients:
        mov r11, 0
_convert_polynomial_coefficients_loop:
        mov rdi, [r12 + 8*r11]
        call _string_to_int
        mov [r12 + 8*r11], rax
        inc r11
        cmp r11, [coeff_num]
        jb _convert_polynomial_coefficients_loop

; Calculates value of a polynomial for a given argument.
; Argument at rdi.
; Polynomial coefficients at r12
; Used registers: rax, r9, r10, r11, rdx, r12
; Retrun value: 
; rax - value of polynomial for given argument (in Z_MODULO field)

_horner_method:
        mov r11, [coeff_num]
        mov r10, MODULO

        mov rax, rdi
        xor rdx, rdx
        div r10
        mov rdi, rdx

        xor rax, rax
_horner_method_loop:
        mov r9, [r12 + 8*r11 - 8]
        mul rdi
        add rax, r9

        xor rdx, rdx
        div r10
        mov rax, rdx

        sub r11, 1
        cmp r11, 0
        je _horner_method_end
        jmp _horner_method_loop

_horner_method_end:
        ret

; Converts value of string at rdi to an integer modulo MODULO
; Address of string at rdi
; Returns values:
; rax - integer value of string
; Used registers: rax, rdi, rdx, r8, r9, r10
_string_to_int:
        xor rax, rax
        mov r9, 1
        mov r10, MODULO
        movzx r8, byte [rdi]
        cmp r8, byte 0
        je _empty_string
_string_to_int_loop:
        movzx r8, byte [rdi] ; Checking if the word has ended.
        cmp r8, byte 0 
        je _string_to_int_exitloop ; If so exit loop.

        call _is_a_digit

        inc rdi

        sub r8, '0' ; Adding another digit to rax
        imul rax, 10
        add rax, r8
        
        xor rdx, rdx    ; rax := rax modulo MODULO
        div r10
        mov rax, rdx

        jmp _string_to_int_loop

_string_to_int_exitloop:
        ret

; Function that ends program with exitcode 1.
; Is called when commandline argument is an empty string.
_empty_string:
        exit 1

; Function checking if given character at r8 is a digit.
; If it is not program is finished with exitcode 1.
; Used registers: r8
_is_a_digit:
        cmp r8, '0'
        jb _is_a_digit_not_a_digit
        cmp r8, '9'
        ja _is_a_digit_not_a_digit
        ret
_is_a_digit_not_a_digit:
        exit 1