%ifndef __MIO_INC__
%define __MIO_INC__

; Reads a character
; Out: AL - ASCII code
extern mio_readchar

; Writes a character
; In: AL - ASCII code
extern mio_writechar

; Writes a string (null-terminated)
; In: EAX - pointer
extern mio_writestr

; New line
extern mio_writeln

%endif