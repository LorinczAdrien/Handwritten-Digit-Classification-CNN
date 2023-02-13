%ifndef __UTIL_INC__
%define __UTIL_INC__

; Allocates memory from heap
; In: EAX - number of bytes
; Out: EAX - pointer
extern mem_alloc

; Frees memory
; In: EAX - pointer
extern mem_free

; Opens file
; In:
;	EAX - filename string pointer
;   EBX - open mode: 0 (read), 1 (write)
; Out:
;	EAX - file handle
extern fio_open

; Closes file
; In: EAX - file handle
extern fio_close

; Reads from file
; In:
;	EAX - file handle
;	EBX - data pointer
;   ECX - data size (number of bytes)
; Out:
;	EDX - number of bytes read
extern fio_read

; Writes to file
; In:
;	EAX - file handle
;	EBX - data pointer
;   ECX - data size (number of bytes)
; Out:
;	EDX - number of bytes written
extern fio_write

; Seeks in the file
; In:
;	EAX - file handle
;	EBX - seek origin: 0 (beginning of file), 1 (current position), 2 (end of file)
;	ECX - offset
; Out:
;	EDX - new absolute position in the file
extern fio_seek

; Sleeps for a specified amount of time
; In:
;	EAX - milliseconds
extern sleep

; Returns a timestamp in seconds as a 64-bit floating point value
; Out:
;	XMM0 - seconds (double-precision float)
extern timer_query

; Returns a 32-bit pseudorandom number
; Out:
;	EAX - number
extern rand

; Returns the command line arguments as a string (null-terminated)
; Out:
;   EAX - pointer
extern getargs

; sin (scalar single-precision)
; In/Out:
;	XMM0 - x
extern sin_ss

; cos (scalar single-precision)
; In/Out:
;	XMM0 - x
extern cos_ss

; tan (scalar single-precision)
; In/Out:
;	XMM0 - x
extern tan_ss

; exp (scalar single-precision)
; In/Out:
;	XMM0 - x
extern exp_ss

; log (scalar single-precision)
; In/Out:
;	XMM0 - x
extern log_ss

%endif