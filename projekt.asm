; Compile
; nasm -f win32 projekt.asm
; nlink projekt.obj -lio -lmio -lgfx -lutil -o projekt.exe

%include 'io.inc'
%include 'mio.inc'
%include 'gfx.inc'
%include 'util.inc'

;=================================================================CONSTANTS===========================================================================
%define WIDTH  252              ; 9 * 28
%define HEIGHT 320              ; 9 * 28 + 68
%define DOWNSCALED_IMAGE 784    ; 28 * 28
;=====================================================================================================================================================

;=================================================================DEBUG TOOLS=========================================================================
%define DEBUG_TXT 0             ; 1 = DEBUG mode (txt allomany feldolgozasanak kiirasa)
%define DEBUG_PADDING 0         ; 1 = DEBUG mode (konvolucios retegnel a padding elvegzesenek kiirasa)
%define DEBUG_MAXPOOL 0         ; 1 = DEBUG mode (konvolucios retegnel a maxpool elvegzesenek kiirasa)
%define DEBUG_RELU 0            ; 1 = DEBUG mode (konvolucios es linearis retegnel a reLU elvegzesenek kiirasa)
%define DEBUG_CONV_FILTERS 0    ; 1 = DEBUG mode (konvolucios retegnel a beolvasott filterek kiirasa)
%define DEBUG_CONV_BIAS 0       ; 1 = DEBUG mode (konvolucios retegnel a beolvasott bias ertekek kiirasa)
%define DEBUG_CONV_MUVELETEK 0  ; 1 = DEBUG mode (konvolucios retegnel a konvolucios muveletek kiirasa)
%define DEBUG_CONV_OSSZEADAS 0  ; 1 = DEBUG mode (konvolucios retegnel a temp matrixbol eredmeny matrix kiirasa)
%define DEBUG_LINEAR 0          ; 1 = DEBUG mode (linearis retegnel a weights es bias ertekek kiirasa)
%define DEBUG_MATRIX_VECTOR 0   ; 1 = DEBUG mode (linearis retegnel a matrix vectorral valo szorzasanak kiirasa)

%define DEBUG_PICTURE_MID 0     ; 1 = DEBUG mode (megjeleniti a kozepso: 84 x 84 kep)
%define DEBUG_PICTURE_SMALL 0   ; 1 = DEBUG mode (megjeleniti a kis: 28 x 28 kep)

%define DEBUG_FLOAT_ACCURACY 2  ; megadja, hogy debugolas kozben a float szamok milyen pontossagal jelenjenek meg
%define BRUSH_SIZE 20           ; brush size (BRUSH_SIZE * BRUSH_SIZE meretu negyzet)
;=====================================================================================================================================================
global main

section .text

;=================================================================MAIN===============================================================================
main:
    ; dinamikus memoria foglalas a kep matrixoknak
        lea     eax, [WIDTH * WIDTH * 4]
        call    mem_alloc             
        mov     [nagy_matrix], eax      ; lement pointer (252 x 320 (x 4))

        lea     eax, [84 * 84 * 4]
        call    mem_alloc
        mov     [kozep_matrix], eax     ; lement pointer (84 x 84 (x 4))

        lea     eax, [28 * 28 * 4]
        call    mem_alloc
        mov     [kis_matrix], eax       ; lement pointer (28 x 28 (x 4))

        lea     eax, [28 * 28 * 4]
        call    mem_alloc
        mov     [veg_scaled], eax       ; lement pointer (28 x 28 (x 4)) float tomb

        lea     eax, [28 * 28]
        call    mem_alloc
        mov     [veg_matrix], eax       ; lement pointer (28 x 28)
   
    ; rajzolasi felulet es inicialis matrix lementese
        call    Rajzolas                        ; feltolti a nagy_matrix (252 x 252) reszet a bemenettel

    ; kep lecsokkentese (3x3 -> 1 pixel)
        mov     eax, [nagy_matrix]              ; ezt meretezzuk at
        mov     [regi_kep], eax 

        mov     [regi_meret], dword WIDTH       ; regi kep merete

        mov     eax, [kozep_matrix]             ; uj kepre mutato pointer
        mov     [uj_kep], eax   

        mov     [uj_meret], dword 84            ; a kep uj merete

        call    Kep_atmeretezes

        ; DEBUG
            mov     eax, DEBUG_PICTURE_MID
            cmp     eax, 0
            je      .no_debug_picture_mid
        
            ; kozep_matrix felrajzolasa
                mov     dword [grafika_merete], 84
                mov     eax, [kozep_matrix]
                mov     [mas_pointer], eax
                lea     eax, [84 * 84 * 4]
                mov     [mas_meret], eax
                call    Letrehozott_kep
        .no_debug_picture_mid:

    ; kep ujboli lecsokkentese (3x3 -> 1 pixel)
        mov     eax, [kozep_matrix]             ; ezt meretezzuk at
        mov     [regi_kep], eax     

        mov     [regi_meret], dword 84          ; regi kep merete 

        mov     eax, [kis_matrix]               ; uj kepre mutato pointer
        mov     [uj_kep], eax   

        mov     [uj_meret], dword 28            ; a kep uj merete

        call    Kep_atmeretezes

        ; DEBUG
            mov     eax, DEBUG_PICTURE_SMALL
            cmp     eax, 0
            je      .no_debug_picture_small
            ; matrix felrajzolasa
                mov     dword [grafika_merete], 28      ; uj kep merete
                mov     eax, [kis_matrix]
                mov     [mas_pointer], eax
                lea     eax, [28 * 28 * 4]
                mov     [mas_meret], eax
                call    Letrehozott_kep
            .no_debug_picture_small:

    ; pixelek osszevonasa (4 bgr0 -> 1)
        call    Pixelek_osszevonasa             ; veg_matrix-ban lesz tarolva a osszesuritott matrix

    ; pixelek skalazasa (0 - 255 -> -1 - 1)
        call    Pixelek_skalazasa               ; veg_scaled-ben lesz tarolva a feldolgozott matrix

    ; txt allomany beolvasasa es feldolgozasa
        call    Txt_allomany                    ; txt allomany feldolgozasa

    ; linearis es konvolucios reteg ertekeinek beolvasa es a neuralis halo feldolgozasa (reLU, MaxPool, SoftMax)
        call    Neuralis_halo

    ; valoszinusegek megjelenitese
        call    Valoszinuseg_megjelenitese

    ; memoria felszabaditasa (dinamikusan foglalt vectorok)
        mov     eax, [nagy_matrix]
        call    mem_free
        mov     eax, [kozep_matrix]
        call    mem_free
        mov     eax, [kis_matrix]
        call    mem_free
        mov     eax, [veg_matrix]
        call    mem_free
        mov     eax, [valoszinusegek]
        call    mem_free

    ret

;==============================================================FUNCTIONS=============================================================================
Neuralis_halo:
; neuralis halo felepitese es valoszinusegek kiszamitasa
    pusha

    ; beallitunk par fontos dolgot eloszor
    mov     [elso_bool], dword 0                        ; bool valtozo, ha elso matrix szorzasnal vagyunk-e vagy sem
    mov     [elso_conv], dword 0                        ; bool valtozo, ha elso inputnal vagyunk vagy sem (csak konvolucios halonal)
    mov     [volt_conv], dword 0                        ; bool valtozo, linearis retegnel jelzi, ha volt mar conv reteg

    mov     [in_channels_size], dword DOWNSCALED_IMAGE  ; ennyi pixelunk van eloszor (vegso kepunk merete)
    mov     [in_rows], dword 28                         ; 28x28 as a kezdeti matrixunk
    mov     [melyseg], dword 1                          ; kezdetben a konvolucios reteg melysege 1

    ; megnyitjuk a binaris allomanyt
    mov     eax, conv_bin           ; conv_model.bin
    xor     ebx, ebx                ; 0 - read

    call    fio_open                ; eax = file handle
    mov     [bin_handle], eax       ; lement pointer

    ; eloszor is atjarjuk a txt allomany feldogozott verziojat, hogy tudjuk meg mit kell beolvasni es hogyan kell feldolgozni az adatokat
    mov     esi, [bemenet]          ; feldolgozott pointer

    .txt_atjaras:

        xor     eax, eax

        mov     esi, [bemenet]
        lodsb
        mov     [bemenet], esi

        cmp     al, 0               ; feldolgozott allomany vege
        je      .vege_txt

        cmp     al, 'L'             ; L - jelkepez egy linearis reteget
        je      .linearis_reteg      

        cmp     al, 'C'             ; C - jelkepez egy konvolucios reteget
        je      .convolutional_reteg

        cmp     al, 'R'             ; R - jelkepez egy ReLU reteget
        je      .reLU_reteg

        cmp     al, 'S'             ; S - jelkepez egy Softmax reteget
        je      .softmax_reteg

        cmp     al, 'M'             ; M - jelkepez egy MaxPool2d reteget
        je      .max_pool2d

        jmp     .txt_atjaras

    .softmax_reteg:

        call    Softmax

        jmp     .txt_atjaras

    .reLU_reteg:

        cmp     [reLU_mode], dword 0
        je      .linearis_reLU
        jmp     .conv_reLU

        .linearis_reLU:
            ; 0 = lineris reteg volt az utolso, az e_vector-on elvegezzuk a muveleteket
            call    Rectified_Linear_Unit

            jmp     .txt_atjaras

        .conv_reLU:
            ; 1 = konvolucios reteg volt az utolso, a pointer_tomb_in-en elvegezzuk a muveleteket
            mov     esi, [pointer_tomb_in]          ; input matrixok
            mov     ecx, [out_channels]             ; input matrixok szama

            mov     eax, [in_channels_size]
            mov     [e_vector_size], eax            ; a matrix merete

            .elvegez_reLU:

                lodsd                               ; egy matrix
                mov     [e_vector], eax           

                call    Rectified_Linear_Unit

                loop    .elvegez_reLU

            jmp     .txt_atjaras

    .max_pool2d:
        ; DEBUG
            mov     eax, DEBUG_MAXPOOL
            cmp     eax, 0
            je      .no_debug_maxpool_szoveg
            ; kiir szoveg
                mov     eax, max_pool_sz
                call    mio_writestr
                call    mio_writeln
            .no_debug_maxpool_szoveg:

        ; minden egyes bemeneti matrixot atalakitunk
        mov     ecx, [out_channels]                 ; ennyi bemeneti matrixunk van (konvolucios reteg utan)
        mov     esi, [pointer_tomb_in]              ; matrix pointereket tarolo tomb
        mov     edi, [pointer_tomb_in]              ; egyuttal visszatoltjuk az uj pointereket

        .matrix_pool:

            dec     ecx                             ; loop--

            lodsd                                   ; egy matrix pointere

            mov     [pool], eax                     ; atmeretezendo matrix

            ; DEBUG
                mov     ebx, DEBUG_MAXPOOL
                cmp     ebx, 0
                je      .no_debug_maxpool1
                ; kiiratas
                    mov     [matrix], eax
                    mov     eax, [in_rows]
                    mov     [matrix_row], eax
                    mov     [matrix_collumn], eax
                    mov     eax, [in_channels_size]
                    mov     [matrix_size], eax
                    call    Kiir_matrix

                .no_debug_maxpool1:
            mov     eax, [in_rows]                  ; atmeretezendo matrix sorai
            mov     [pool_row], eax
            mov     eax, [in_channels_size]         ; elemek szama
            mov     [pool_size], eax

            call    Max_Pool                        ; new_pool-ban lesz az uj atmeretezett tomb

            ; felszabaditjuk a regi tomb memoriajat
            mov     eax, [pool]
            call    mem_free

            mov     eax, [new_pool]
            stosd                                   ; visszahelyez pointer

            ; DEBUG
                mov     ebx, DEBUG_MAXPOOL
                cmp     ebx, 0
                je      .no_debug_maxpool2
                ; kiiras megint
                    mov     [matrix], eax
                    mov     eax, [new_pool_row]
                    mov     [matrix_row], eax
                    mov     [matrix_collumn], eax
                    mov     eax, [new_pool_size]
                    mov     [matrix_size], eax
                    call    Kiir_matrix

                .no_debug_maxpool2:
            cmp     ecx, 0
            jg      .matrix_pool

        mov     eax, [new_pool_row]                 ; update sorok szama
        mov     [in_rows], eax
        mov     eax, [new_pool_size]                ; update matrix elemek szama
        mov     [in_channels_size], eax

        jmp     .txt_atjaras

    .linearis_reteg:
        ; eloszor is megnezzuk, ha pont konvolucios reteg volt elotte is ki kell-e lapitani a matrixokat
            cmp     [volt_conv], dword 1
            je      .volt_conv_reteg
            jmp     .nem_volt_conv_reteg

            .volt_conv_reteg:

                mov     [volt_conv], dword 0
                ; mivel elso input lesz, ezert a veg_scaled pointerre masoljuk oket
                mov     eax, [veg_scaled]
                call    mem_free

                ; helyet foglalunk az kilapitott vectornak
                mov     eax, [in_rows]              ; utolso matrixok sorainak szama
                imul    eax, eax                    ; utolso matrixok elemeinek a szama
                imul    eax, [out_channels]         ; ahany kimeneti matrixunk volt (mindegyik pl: 7x7)

                mov     [veg_scaled_size], eax      ; ez lesz a merete a kilapitott vectornak

                imul    eax, 4                      ; mindegyik elem 4 byte
                call    mem_alloc

                mov     [veg_scaled], eax           ; lement pointer

                ; most pedig sorra bemasoljuk a kilapitott matrixokat

                    mov     eax, [in_channels_size] ; egy matrix elemeinek szama
                    mov     [masolt_size], eax      ; ennyi elemet fogunk masolni egyszerre

                    mov     ecx, [out_channels]     ; masolando matrixok szama
                    mov     edi, [veg_scaled]       ; cel_vector
                    mov     [cel_vect], edi

                    mov     esi, [pointer_tomb_in]  ; masolando matrixok
                    
                    .lapitott_masolas:

                        lodsd                       ; egy lapitott matrix
                        mov     [masolt_vect], eax

                        call    Masolas_vector      ; bemasoljuk a megfelelo poziciora

                        mov     eax, [cel_vect]
                        mov     ebx, [masolt_size]
                        imul    ebx, 4              ; masolt eleme * 4 -el toljuk el, hogy tudjuk masolni a kovetkezo matrixot
                        add     eax, ebx            ; eltoljuk, hogy tudjuk masolni a kovetkezo matrixot
                        mov     [cel_vect], eax

                        loop    .lapitott_masolas

            .nem_volt_conv_reteg:

            mov     [reLU_mode], dword 0            ; 0 = linearis reteg

            mov     esi, [bemenet]

            lodsd                                   ; eax-be kerul az in_features
            mov     [in_features], eax  

            lodsd                                   ; eax-be kerul az out_features
            mov     [out_features], eax

            mov     [bemenet], esi

        ; dinamikus memoria foglalas (weights matrix es bias vector)
            mov     eax, [in_features]
            mov     ebx, [out_features]

            imul    ebx                             ; eax = in * out
            imul    eax, 4                          ; eax = in * out * 4

            mov     [weights_size], eax             ; lement meret (byte-ban)
            call    mem_alloc
            mov     [weights], eax                  ; weights vektor pointer

            mov     eax, [out_features]             ; ahany neuronunk van, annyi bias ertekunk is
            imul    eax, 4

            mov     [bias_size], eax                ; lemenet meret (byte-ban)
            call    mem_alloc
            mov     [bias], eax                     ; bias vektor pointer

        ; beolvasunk in * out * 4 byte-t (in * out float ertek) es out darab bias erteket
            mov     eax, [bin_handle]
            mov     ebx, [weights]                   ; eloszor beolvassuk a weight-ket
            mov     ecx, [weights_size]

            call    fio_read                        ; EDX = read bytes
            mov     [bin_handle], eax               ; update bin_handle

            cmp     edx, [weights_size]
            jne     .helytelen_beolvasas_weights    ; ha nem olvastunk megfelelo szamu byte-t

            mov     eax, [bin_handle]
            mov     ebx, [bias]                     ; out szamu bias beolvasasa
            mov     ecx, [bias_size]

            call    fio_read                        ; EDX = read bytes
            mov     [bin_handle], eax               ; update bin_handle

            cmp     edx, [bias_size]                ; ha nem olvastunk megfelelo szamu byte-t
            jne     .helytelen_beolvasas_bias

        ; beolvasott ertekek kiirasa (weights)
            ; DEBUG
                mov     eax, DEBUG_LINEAR
                cmp     eax, 0
                je      .no_debug_linear
            ; elso neuron
                mov     eax, weights_sz
                call    mio_writestr
                call    mio_writeln

                mov     eax, [weights]               ; kiirando pointer
                mov     [vector], eax

                mov     ebx, [in_features]
                mov     [vector_size], ebx    ; meret

                call    Beolvasott_vektor

                call    mio_writeln

            ; utolso neuron
                mov     ebx, [weights_size]
                mov     ecx, [in_features]
                sub     ebx, ecx                  ; utolso 100 erteket szeretnenk latni
                sub     ebx, ecx
                sub     ebx, ecx
                sub     ebx, ecx
                
                mov     eax, [weights]
                add     eax, ebx

                mov     [vector], eax                   ; pointer
                mov     ebx, [in_features]
                mov     dword [vector_size], ebx

                call    Beolvasott_vektor

                call    mio_writeln
                call    mio_writeln

            ; beolvasott ertekek kiirasa (bias)

                mov     eax, bias_sz
                call    mio_writestr
                call    mio_writeln

                mov     eax, [bias]                     ; kiirando pointer
                mov     [vector], eax

                mov     ebx, [out_features]
                mov     dword [vector_size], ebx        ; meret

                call    Beolvasott_vektor

                call    mio_writeln
                call    mio_writeln

            .no_debug_linear:
        ; matrix szorzas
            mov     eax, [elso_bool]

            cmp     eax, 0
            je      .elso_reteg
            jmp     .n_reteg

            .elso_reteg:

                mov     [elso_bool], dword 1            ; bool = true (tehat ezutan mar megtortent az elso matrix szorzas)

                ; vector adatok
                mov     eax, [veg_scaled]               ; szorzando vector
                mov     [vector], eax

                mov     eax, [veg_scaled_size]          ; vector merete
                mov     [vector_size], eax 

                jmp     .matrix_beallitas

            .n_reteg:
                ; ha n-dik retegnel vagyunk akkor az eredmeny vector-t be kell masolni a temp vectorba
                mov     eax, [e_vector]                 ; eredmeny vector indexe
                mov     [masolt_vect], eax
                mov     eax, [e_vector_size]            ; e_vector merete (elemek merete, nem byte-ban)
                mov     [masolt_size], eax

                ; memoria foglalas a temp vectornak
                mov     eax, [e_vector_size]            ; amekkora a masolando, akkora helyet kell foglalni
                mov     [temp_vector_size], eax         ; cel vector merete egyben
                imul    eax, 4                          ; 4 elemek szamat kell foglalni

                call    mem_alloc
                mov     [temp_vector], eax              ; lement pointer

                ; ide masolunk
                mov     eax, [temp_vector]             
                mov     [cel_vect], eax
                mov     eax, [temp_vector_size]         
                mov     [cel_size], eax

                call    Masolas_vector

                ; memoria felszabaditas
                mov     eax, [e_vector]                 ; elso vector memoriajanak a felszabaditasa
                call    mem_free  

                ; tehat mostmar a temp_vector-ban vannak a megfelelo adatok
                mov     eax, [temp_vector]              ; szorzando vector
                mov     [vector], eax

                mov     eax, [temp_vector_size]         ; vector merete
                mov     [vector_size], eax  

                jmp     .matrix_beallitas

            .matrix_beallitas:
                ; matrix adatok
                mov     eax, [weights]              ; szorzando matrix
                mov     [matrix], eax   

                mov     eax, [out_features]         ; a szorzando matrix sorainak a szama
                mov     [matrix_row], eax

                mov     eax, [in_features]          ; a szorzando matrix oszlopainak a szama
                mov     [matrix_collumn], eax

                call    Matrix_x_vector_product     ; e_vector = eredmeny, e_vector_size = eredmeny vector merete

        ; dinamikusan lefoglalt memoria felszabaditasa
            mov     eax, [weights]
            call    mem_free

            mov     eax, [bias]
            call    mem_free

        jmp     .txt_atjaras

    .convolutional_reteg:
        ; par valtozot beallitunk es lementjuk a in_channels es out_channels erteket
            mov     [reLU_mode], dword 1            ; 1 = konvolucios reteg
            mov     [volt_conv], dword 1            ; 1 = jelzi a lineris retegnek, hogy ki kell lapitani a matrixokat

            lodsd                                   ; eax-be kerul az in_channels
            mov     [in_channels], eax  

            lodsd                                   ; eax-be kerul az out_channels
            mov     [out_channels], eax
            mov     [bemenet], esi                  ; update txt pointer

        ; dinamikus memoria foglalas (filter matrix es bias vector)
            ; filterek
                ; eloszor helyet foglalunk a pointer_tomb_filter-nek, ami out_channels merete kell legyen, mert ennyi filterunk van
                mov     eax, [out_channels]             ; ennyi filter kell
                imul    eax, 4                          ; filter * 4 byte
                call    mem_alloc
                mov     [pointer_tomb_filter], eax      ; lement eax

                ; most pedig minden egyes filternek foglalunk helyet (3x3)
                mov     edi, [pointer_tomb_filter]      ; hova mentjuk a pointereket
                mov     ecx, [out_channels]             ; ennyi filternek foglalunk helyet

                .filter_alloc:

                    mov     eax, [melyseg]              ; melyseg megadja, hogy egy filter hany darab 3x3 matrixbol all
                    imul    eax, 3 * 3 * 4              ; melyseg darab 3x3 filter (ahol 3 * 3 * 4, mert mindegyik elem 4 byte)
                    call    mem_alloc
                    stosd                               ; lement pointer a pointer tombbe

                    loop    .filter_alloc

            ; bias ertekek
                mov     eax, [out_channels]             ; bias_size = out_channels
                imul    eax, 4
                mov     [bias_size], eax                ; lement size

                call    mem_alloc
                mov     [bias], eax                     ; lement pointer

            ; temporalis matrixok
                ; helyet foglalunk eloszor a pointer_tomb_temp-nek, ami mindig out_channels meretu lesz
                mov     eax, [out_channels]
                imul    eax, 4

                call    mem_alloc
                mov     [pointer_tomb_temp], eax        ; lement pointer

                ; helyet foglalunk minden egyes matrixnak (tenzor)
                mov     ecx, [out_channels]             ; temp matrixok (tenzorok) szama
                mov     edi, [pointer_tomb_temp]        ; ide mentjuk a pointereket

                .temp_tenzor:

                    mov     eax, [in_channels_size]     ; pl: 28 * 28
                    imul    eax, [melyseg]              ; megszorozzuk a tenzor melysegevel (pl: 28 * 28 * 32)
                    imul    eax, 4                      ; mindegyik elem 4 byte

                    call    mem_alloc

                    stosd                               ; lement pointer

                    loop    .temp_tenzor

            ; eredmeny matrixok
                ; most pedig az eredmeny matrixoknak kell foglalni helyet (itt taroljuk majd a vegso eredmenyeket)
                cmp     [elso_conv], dword 0
                je      .elso_input
                ; a bemeneti matrixok mar a pointer_tomb_in-ben vannak (ha nem ide ertunk)
                jmp     .eredmeny_matrixok

                .elso_input:
                    ; pointer_tomb_in-ben csak egy ertek lesz, ami a kezdeti skalazott kepmatrix
                    mov     eax, 4 * 1                  ; egy kezdeti matrix
                    call    mem_alloc
                    mov     [pointer_tomb_in], eax      ; lement pointer

                    mov     ebx, [veg_scaled]           ; kezdeti kepmatrix
                    mov     [eax], ebx                  ; tehat ez a bemeneti matrixunk

                    mov     [elso_conv], dword 1        ; tehat mar nem az elso inputnal vagyunk

                    jmp     .eredmeny_matrixok

                .eredmeny_matrixok:
                    ; pointer_tomb_ered matrixok memoria foglalasa
                    mov     eax, [out_channels]         ; ennyi eredmeny matrix lesz
                    imul    eax, 4
                    call    mem_alloc
                    mov     [pointer_tomb_ered], eax    ; lement pointer

                    mov     ecx, [out_channels]         ; ennyi matrixot kell letrehozni
                    mov     edi, [pointer_tomb_ered]    ; ide mentjuk a matrixok pointereit
                    
                    .e_matrixok:

                        mov     eax, [in_channels_size]
                        imul    eax, 4

                        call    mem_alloc
                        stosd                           ; lement pointer

                        loop    .e_matrixok

        ; beolvassuk az adatokat
            ; filterek beolvasasa
                ; DEBUG
                    mov     eax, DEBUG_CONV_FILTERS
                    cmp     eax, 0
                    je      .no_debug_filter_szoveg
                    ; kiir szoveg
                        mov     eax, filters_sz
                        call    mio_writestr
                        call    mio_writeln
                    .no_debug_filter_szoveg:

                mov     eax, [bin_handle]
                mov     ecx, [out_channels]             ; ennyi filtert olvasunk be
                mov     esi, [pointer_tomb_filter]      ; itt vannak a pointereink

                .filter_beolvasas:
                    dec     ecx                         ; loop index--

                    push    ecx

                    lodsd                               ; egy matrix pointere
                    mov     ebx, eax
                    mov     eax, [bin_handle]
                    mov     ecx, 3 * 3 * 4              ; ennyi byte-t olvasunk be (egy 3 x 3-as filter)
                    imul    ecx, [melyseg]              ; ezt meg meg kell szorozzuk a filter melysegevel (tenzor melysege)

                    call    fio_read
                    mov     eax, 3 * 3 * 4
                    imul    eax, [melyseg]
                    cmp     edx, eax                    ; ha nem olvastunk megfelelo szamu byte-t
                    jne     .helytelen_beolvasas_filter

                    ; DEBUG
                        mov     eax, DEBUG_CONV_FILTERS
                        cmp     eax, 0
                        je      .no_debug_filter
                        ; kiiras matrix
                            mov     eax, [esi - 4]
                            mov     [vector], eax
                            mov     eax, 3 * 3
                            imul    eax, [melyseg]
                            mov     [vector_size], eax

                            call    Beolvasott_vektor

                        .no_debug_filter:
                    pop     ecx

                    cmp     ecx, 0
                    jg      .filter_beolvasas

            ; bias ertekek
                mov     eax, bias
                mov     eax, [bin_handle]
                mov     ebx, [bias]
                mov     ecx, [bias_size]

                call    fio_read
                cmp     edx, [bias_size]
                jne     .helytelen_beolvasas_bias

                ; DEBUG
                    mov     eax, DEBUG_CONV_BIAS
                    cmp     eax, 0
                    je      .no_debug_konvolucios_bias
                    ; kiiratjuk a beolvasott vectort
                        mov     eax, bias_sz
                        call    mio_writestr
                        call    mio_writeln

                        mov     eax, [bias]
                        mov     [vector], eax
                        mov     eax, [bias_size]
                        mov     ebx, 4
                        cdq
                        idiv    ebx
                        mov     [vector_size], eax
                        call    Beolvasott_vektor
                    .no_debug_konvolucios_bias:

        ; padding minden pointer_tomb_in matrixnak
            ; DEBUG
                mov     eax, DEBUG_PADDING
                cmp     eax, 0
                je      .no_debug_padding_szoveg
                ; kiir szoveg
                    mov     eax, padding_sz
                    call    mio_writestr
                    call    mio_writeln
                .no_debug_padding_szoveg:

            mov     ecx, [in_channels]              ; ennyi bemeneti matrixunk van
            mov     esi, [pointer_tomb_in]
            mov     edi, [pointer_tomb_in]          ; ide toltjuk vissza az uj matrixokat

            .padding_matrix:

                dec     ecx                         ; loop--

                lodsd                               ; egy matrix cime

                mov     [padding], eax              ; matrix pointer

                ; DEBUG
                    mov     eax, DEBUG_PADDING
                    cmp     eax, 0
                    je      .no_debug_padding1
                    ; kiiras matrix
                        mov     eax, [padding]
                        mov     [matrix], eax
                        mov     eax, [in_channels_size]
                        mov     [matrix_size], eax
                        mov     eax, [in_rows]
                        mov     [matrix_collumn], eax
                        call    Kiir_matrix

                    .no_debug_padding1:
 
                mov     eax, [in_channels_size]     ; matrix merete
                mov     [padding_size], eax
                mov     eax, [in_rows]              ; matrix sorai
                mov     [padding_rows], eax

                call    Padding

                mov     eax, [padding]              ; regi matrix memoriajanak a felszabaditasa
                call    mem_free

                mov     eax, [new_padding]
                stosd                               ; visszahelyezzuk az uj matrixot

                ; DEBUG
                    mov     eax, DEBUG_PADDING
                    cmp     eax, 0
                    je      .no_debug_padding2
                    ; kiiras padding
                        mov     eax, [new_padding]          
                        mov     [matrix], eax
                        mov     eax, [new_padding_rows]     ; matrix sorai
                        mov     [matrix_row], eax
                        mov     [matrix_collumn], eax
                        mov     eax, [new_padding_size]
                        mov     [matrix_size], eax
                        call    Kiir_matrix

                    .no_debug_padding2:

                cmp     ecx, 0
                jg      .padding_matrix

            mov     eax, [in_rows]
            add     eax, 2
            mov     [in_rows_pad], eax              ; a padding matrix 2-vel nagyobb

            mov     eax, [in_rows_pad]
            imul    eax, eax
            mov     [in_channels_pad], eax          ; pl: 5x5 -> 7x7

        ; elvegezzuk a konvolucios muveleteket minden inputra
            ; DEBUG
                mov     eax, DEBUG_CONV_MUVELETEK
                cmp     eax, 0
                je      .no_debug_konvolucios_muveletek
                ; kiir szoveg
                    mov     eax, konvolucio_sz
                    call    mio_writestr
                    call    mio_writeln 
                .no_debug_konvolucios_muveletek:

            mov     ecx, [out_channels]                 ; ennyi filterunk van
            mov     edx, [pointer_tomb_filter]          ; filterek
            mov     esi, [pointer_tomb_in]              ; feldolgozando matrixok
            mov     edi, [pointer_tomb_temp]            ; ide mentjuk az itteni eredmenyeket (vegso eredmeny kesobb)

            .filterek_atjarasa:

                dec     ecx                             ; loop--

                push    ecx

                mov     ecx, [melyseg]                  ; a tenzor nagysagat adja meg, amit letrehozunk

                mov     eax, [edx]                      ; egy filter (tenzor)             
                mov     [k_filter], eax                 ; muvelethez a filter
                add     edx, 4                          ; kovetkezo filter  

                mov     eax, [edi]                      ; egy temp tenzor (ami tobb kisebb temp matrixot tartalmaz)
                mov     [k_temp], eax                   ; muvelethez temp tenzor
                add     edi, 4                          ; kovetkezo temp tenzor

                .filterek_melyseg_atjarasa:

                    lodsd                               ; egy bemeneti matrix
                    mov     [k_matrix], eax             ; muvelethez matrix

                    call    Konvolucios_muveletek_3x3

                    ; DEBUG
                        mov     eax, DEBUG_CONV_MUVELETEK
                        cmp     eax, 0
                        je      .no_debug_konvolucios_muveletek1
                        ; kiiras temp matrix
                            mov     eax, [k_temp]
                            mov     [matrix], eax
                            mov     eax, [in_rows]
                            mov     [matrix_row], eax
                            mov     [matrix_collumn], eax
                            mov     eax, [in_channels_size]
                            mov     [matrix_size], eax
                            call    Kiir_matrix

                        .no_debug_konvolucios_muveletek1:

                    ; update filter poz (tenzor)
                    mov     eax, [k_filter]
                    add     eax, 3 * 3 * 4              ; kovetkezo filter a tenzorban
                    mov     [k_filter], eax             ; update filter helye

                    ; update temp poz (tenzor)
                    mov     eax, [k_temp]               
                    mov     ebx, [in_channels_size]     ; bemeneti matrix merete (db)
                    imul    ebx, 4
                    add     eax, ebx                    ; kovetkezo temp matrix a tenzorban
                    mov     [k_temp], eax               

                    loop    .filterek_melyseg_atjarasa

                pop     ecx

                mov     esi, [pointer_tomb_in]          ; feldolgozando matrixok (reset)

                cmp     ecx, 0
                jg      .filterek_atjarasa

        ; temp matrixok => eredmeny matrixok (temp matrixok osszevonasa)
            mov     ecx, [out_channels]                 ; ennyi eredmeny matrixunk kell legyen
            mov     esi, [pointer_tomb_temp]            ; temp tenzorok
            mov     edi, [pointer_tomb_ered]            ; eredmeny matrixok

            .eredmeny_matrix_letrehozas:

                dec     ecx                             ; loop--

                push    ecx

                mov     ecx, [melyseg]                  ; ez mondja meg, hogy hany temp matrixot kell osszevonni egy matrixa

                ; eloszor is feltoltjuk az eredmeny matrixot 0-s elemekkel
                mov     eax, [edi]                      ; egy eredmeny matrix
                mov     [matrix], eax
                mov     eax, [in_channels_size]         ; pl: 28 * 28
                mov     [matrix_size], eax

                call    Feltolt_0                       ; feltolti a matrixot 0-s elemekkel
                ; ezutan hozzadjuk minden egyes temp matrixot az eredmeny matrixhoz
                mov     eax, [edi]                      ; egy eredmeny matrix
                mov     [matrix], eax                   ; osszeadashoz

                lodsd                                   ; egy temp tenzor
                mov     [added_matrix], eax             ; osszeadashoz

                mov     eax, [in_channels_size]         ; elemek szama
                mov     [matrix_size], eax

                .temp_hozzaadasa:

                    call    Matrixok_osszeadasa         ; osszeadjuk a megfelelo szamu matrixokat

                    mov     eax, [added_matrix]
                    mov     ebx, [in_channels_size]     ; elemek szama
                    imul    ebx, 4                      ; elemek * 4
                    add     eax, ebx                    ; kovetkezo matrix a tenzorban
                    mov     [added_matrix], eax         

                    loop    .temp_hozzaadasa

                ; DEBUG
                    mov     eax, DEBUG_CONV_OSSZEADAS
                    cmp     eax, 0
                    je      .no_debug_osszeadas_matrixok
                    ; kiiras osszeadva
                        mov     eax, osszeadott_sz
                        call    mio_writestr
                        call    mio_writeln
                        mov     eax, [in_rows]
                        mov     [matrix_collumn], eax
                        call    Kiir_matrix

                    .no_debug_osszeadas_matrixok:

                mov     eax, [matrix]                   ; eredmeny
                stosd                                   ; lement eredmeny

                pop     ecx

                cmp     ecx, 0
                jg    .eredmeny_matrix_letrehozas

        ; bias hozzaadasa minden matrixhoz
            mov     eax, [in_channels_size]             ; ekkora egy matrix
            mov     [matrix_row], eax 

            mov     ecx, [out_channels]                 ; ennyi eredmeny matrixunk kell legyen
            mov     esi, [pointer_tomb_ered]            ; eredmeny matrixok
            mov     edi, [bias]                         ; bias ertekek (egy matrixhoz egy bias)

            .bias_konvolucio:

                lodsd                                   ; egy eredmeny matrix
                mov     [e_vector], eax

                movss   xmm0, [edi]                     ; egy bias ertek (ezt atadjuk az alprogramnak)
                add     edi, 4

                call    Bias_hozzaadas_conv

                loop    .bias_konvolucio

        ; eredmeny matrixokat bemasoljuk a pointer_tomb_in-be a pointer_tomb_ered-bol
            ; a bemeneti matrixok felszabaditasa es pointer tomb felszabaditasa
            mov     ecx, [in_channels]
            mov     esi, [pointer_tomb_in]          ; pointereket tarolo tomb

            .bemenet_free:

                lodsd
                call    mem_free

                loop    .bemenet_free

            mov     eax, [pointer_tomb_in]          ; felszabaditjuk a regi pointer tombot
            call    mem_free

            mov     eax, [out_channels]             ; eredmeny matrixok szama
            imul    eax, 4
            
            call    mem_alloc
            mov     [pointer_tomb_in], eax

            ; johet a masolas
            mov     eax, [pointer_tomb_ered]        ; honnan
            mov     [masolt_vect], eax

            mov     eax, [pointer_tomb_in]          ; hova
            mov     [cel_vect], eax

            mov     eax, [out_channels]             ; hany elemet
            mov     [masolt_size], eax

            call    Masolas_vector

        ; melyseg frissitese
            mov     eax, [out_channels]             ; uj melyseg
            mov     [melyseg], eax 

        ; dinamikusan lefoglalt memoria felszabaditasa
            ; bias tomb felszabaditasa
                mov     eax, [bias]                     ; bias ertekek felszabaditasa
                call    mem_free

            ; filter tomb es filterek felszabaditasa
                mov     ecx, [out_channels]             ; ennyi filternek foglaltunk helyet
                mov     esi, [pointer_tomb_filter]      ; tarolja a pointereket

                .filter_free:

                    lodsd 
                    call    mem_free

                    loop    .filter_free

                mov     eax, [pointer_tomb_filter]      ; pointer tomb felszabaditasa
                call    mem_free

            ; temp tomb es temp tenzorok felszabaditasa
                mov     ecx, [out_channels]             ; ennyi temp tenzorunk van
                mov     esi, [pointer_tomb_temp]        ; tarolja a pointereket

                .temp_free:

                    lodsd
                    call    mem_free

                    loop    .temp_free

                mov     eax, [pointer_tomb_temp]        ; a pointer tomb felszabaditasa
                call    mem_free

        jmp     .txt_atjaras

    .vege_txt:

        mov     eax, [bin_handle]       ; bin handle

        call    fio_close               ; bezarjuk a binaris allomanyt

        popa

        clc

        ret

    .helytelen_beolvasas_weights:

        mov     eax, hiba_weights
        call    mio_writestr

        jmp     .hiba_stc

    .helytelen_beolvasas_bias:

        mov     eax, hiba_bias
        call    mio_writestr

        jmp     .hiba_stc

    .helytelen_beolvasas_filter:

        mov     eax, hiba_bias
        call    mio_writestr

        jmp     .hiba_stc

    .hiba_stc:

        mov     eax, [bin_handle]       ; bin handle

        call    fio_close               ; bezarjuk a binaris allomanyt

        popa

        stc

        ret

Konvolucios_muveletek_3x3:
; (input_matrix, filter) -> temp_matrix (a konvolucios muvelet)
    pusha 

    ; par dolog elotte
    mov     [atjart_elemek], dword 0            ; ebben szamoljuk hany elem van
    mov     eax, [in_rows_pad]                  ; k_matrix soranak a hossza
    imul    eax, 4
    mov     [k_matrix_row], eax                 ; sorok kozti lepeshez

    mov     eax, 3 * 4 * 4
    call    mem_alloc
    mov     [add_temp], eax                     ; temporalis 3 * 4 matrix

    xorps   xmm0, xmm0                          ; matrix elemek
    xorps   xmm1, xmm1                          ; filter elemek
    xorps   xmm7, xmm7                          ; 0-zni az utolso float elemet

    ; keszitunk egy maszkot az xmm7 regiszterbe
    xor     eax, eax
    push    eax

    mov     eax, 0xFFFFFFFF                     ; 1-sek
    push    eax
    push    eax
    push    eax

    movups  xmm7, [esp]

    pop     eax
    pop     eax
    pop     eax
    pop     eax

    ; kernel_3x3
    mov     ebx, [add_temp]                     ; temporalis 3 * 4 vector
    mov     edx, [k_filter]                     ; filterben levo elemek
    mov     esi, [k_matrix]                     ; innen vesszuk az elemeket
    mov     edi, [k_temp]                       ; eredmeny ide kerul

    add     esi, [k_matrix_row]                 ; elso sor kihagyasa
    add     esi, 4                              ; 2-s sor elso eleme is 0

    mov     ecx, [in_rows]                      ; minden sort atjarunk (ahol nem 0-sok vannak)
    imul    ecx, ecx                            ; minden elemet

    .elemek_kernel:

        dec     ecx                             ; elemek--

        ; elso 3 elem
            sub     esi, [k_matrix_row]             ; elozo sor
            sub     esi, 4

            movups  xmm0, [esi]                     ; elso 3 elem (matrix)
            andps   xmm0, xmm7                      ; kimaszkolni utolso elemet

            movups  xmm1, [edx]                     ; elso 3 elem (filter)
            andps   xmm1, xmm7

            mulps   xmm0, xmm1                      ; matrix_elemek * filter

            movups  [ebx], xmm0                     ; lement elso 3

        ; kozepso 3 elem
            add     esi, [k_matrix_row]             ; kovetkezo sor

            movups  xmm0, [esi]                     ; kovetkezo 3 elem (matrix)
            andps   xmm0, xmm7

            movups  xmm1, [edx + 3 * 4]             ; kovetkezo 3 elem (filter)
            andps   xmm1, xmm7

            mulps   xmm0, xmm1

            movups  [ebx + 3 * 4], xmm0             ; lement kovetkezo 3 elem

        ; utolso 3 elem
            add     esi, [k_matrix_row]

            movups  xmm0, [esi]                     ; utolso 3 elem (matrix)
            andps   xmm0, xmm7

            movups  xmm1, [edx + 6 * 4]             ; utolso 3 elem (filter)
            andps   xmm1, xmm7

            mulps   xmm0, xmm1

            movups  [ebx + 6 * 4], xmm0             ; lement kovetkezo 3 elem

        ; a 9 elem osszeadasa es vegso elem kiszamitasa
            xorps   xmm0, xmm0                      ; osszeg
            addss   xmm0, [ebx]
            addss   xmm0, [ebx + 4]
            addss   xmm0, [ebx + 2 * 4]
            addss   xmm0, [ebx + 3 * 4]
            addss   xmm0, [ebx + 4 * 4]
            addss   xmm0, [ebx + 5 * 4]
            addss   xmm0, [ebx + 6 * 4]
            addss   xmm0, [ebx + 7 * 4]
            addss   xmm0, [ebx + 8 * 4]

            movd    eax, xmm0                       ; eredmeny
            stosd                                   ; behelyezzuk az eredmeny matrixba

        ; esi cimet a kovetkezo elemre tesszuk
            sub     esi, [k_matrix_row]             ; kozepso sorba lepes
            add     esi, 8                          ; helyre allitas es kovetkezo elemre lepes

        ; megnezzuk, ha ki kell hagyni-e nullasokat (minden sor vegen 1 nullas es sor elejen 1 nullas)
            push    ecx

            mov     ecx, [atjart_elemek]
            inc     ecx                             ; atjart elemek++

            cmp     ecx, [in_rows]                  ; ha atjartuk egy sor minden elemet, akkor atkell szokni 2 0-s erteket
            jge     .atszokes
            jmp     .nem_atszokes

            .atszokes:

                add     esi, 8                      ; 2 elemet kiszokunk
                xor     ecx, ecx                    ; szamlalo = 0

            .nem_atszokes:

                mov     [atjart_elemek], ecx        ; update atjart_elemek

            pop     ecx

        cmp     ecx, 0
        jg      .elemek_kernel

    mov     eax, [add_temp]
    call    mem_free

    popa

    clc

    ret

Padding:
; egy matrixot korbevesz 0-s ertekekkel
    pusha

    ; eloszor helyet foglalunk egy uj matrixnak ami 1-el nagyobb mind 4 iranyban
    mov     eax, [padding_rows]
    add     eax, 2
    mov     [new_padding_rows], eax     ; az uj matrix sorainak merete (pl: 28 -> 30)
    imul    eax, eax                    ; pl 28x28 -> 30x30

    mov     [new_padding_size], eax     ; ennyi lepesszam kell legyen

    imul    eax, 4                      ; 30x30 * 4
    call    mem_alloc
    mov     [new_padding], eax

    ; most pedig atmasolunk mindent es 0-sokat teszunk le
    mov     edx, [new_padding_size]     ; az osszes lepes (pl 30x30)
    mov     esi, [padding]              ; innen masolunk
    mov     edi, [new_padding]          ; uj matrix

    mov     ecx, [new_padding_rows]     ; egy sor
    sub     edx, ecx                    ; ennyi lepest kivonunk
    xorps   xmm0, xmm0                  ; xmm0 = 0
    movd    eax, xmm0

    ; beteszunk new_padding_rows darab 0-st (elso sor)
    .elso_sor:

        stosd           

        loop    .elso_sor

    ; tobbi sor utolsoig es koztes elemek
    xorps   xmm0, xmm0                  ; xmm0 = 0

    mov     ecx, edx
    sub     ecx, [new_padding_rows]     ; utolso sort kihagyjuk

    .tobbi_sor:

        movd    eax, xmm0               ; sor eleji 0-s
        stosd
        dec     ecx                     ; elemek--

        sub     ecx, [padding_rows]     ; kivonjuk most, ugyanis a .masolt_elemek-ben ennyit fogunk letenni

        push    ecx

        mov     ecx, [padding_rows]     ; ahany elem van a regi matrix egy soraban

        .masolt_elemek:

            movsd                       ; 4 byte esi -> edi

            loop    .masolt_elemek

        pop     ecx

        movd    eax, xmm0               ; sor vegi 0-s
        stosd
        dec     ecx                     ; elemek--

        cmp     ecx, 0
        jg      .tobbi_sor
    
    ; utolso sorba is pedig 0-sokat teszunk
    mov     ecx, [new_padding_rows]     ; egy sor
    xorps   xmm0, xmm0                  ; xmm0 = 0
    movd    eax, xmm0

    .utolso_sor:

        stosd

        loop    .utolso_sor

    popa

    clc

    ret

Max_Pool:
; kep pixeleinek osszevonasa (4 -> 1) vagyis 1/4 pixel marad (kernel_size = 2x2, stride = 2)
    pusha

    mov     eax, [pool_size]        ; jelenleg a matrix db szam (ennyi helyet fogalunk, mert 4x kevesebb elem lesz, de egy elem 4 byte)
    call    mem_alloc
    mov     [new_pool], eax

    mov     eax, [pool_row]
    mov     ebx, 2
    cdq     
    idiv    ebx                     ; csak fele akkora matrix lesz (fele vagyis 28x28 -> 14x14)
    mov     [new_pool_row], eax

    mov     eax, [new_pool_row]     ; uj sorok szama
    imul    eax, eax                ; uj matrix elemeinek a szama
    mov     [new_pool_size], eax

    ; beallitunk egy maszkolasi xmm regisztert
        xorps   xmm7, xmm7

        xor     eax, eax
        push    eax
        push    eax
        mov     eax, 0xFFFFFFFF
        push    eax
        push    eax

        movups  xmm7, [esp]             ; elso ket ertek 0xFFFFFFFF tobbi 0

        pop     eax
        pop     eax
        pop     eax
        pop     eax

    ; most pedig felepitjuk az eredmeny matrixot
    mov     ecx, [new_pool_row] 

    mov     esi, [pool]             ; eredeti matrix
    mov     edi, [new_pool]         ; uj_matrix

    mov     edx, [pool_row]
    imul    edx, 4                  ; kovetkezo sorba lepeshez

    xorps   xmm0, xmm0
    xorps   xmm1, xmm1

    .sorok_pool:

        push    ecx

        push    edx
        mov     eax, [pool_row]     ; volt meret
        mov     ebx, 4              ; fele akkora az uj matrix es egyszerre 2 elemet dolgozunk fel
        cdq
        idiv    ebx
        mov     ecx, eax            ; ennyi lepes lesz
        mov     ebx, edx            ; ha marad a vegen egy (maradek jelzi, ha a vegen meg kell max_poolt vegezni egy 2x2 es kockara)
        pop     edx

        .oszlop_pool:

            movups  xmm0, [esi]             ; 4 elem, ahol 2 egyik maximumhoz, 2 masik maximumhoz

            add     esi, edx
            movups  xmm1, [esi]             ; 4 elem, ahol 2 egyik maximumhoz, 2 masik maximumhoz

            maxps   xmm0, xmm1              ; maximum szamitas (bal felen marad masodik 4 elemnek 2 max, jobb felen marad elso 4 elemnek 2 max)
            movups  xmm1, xmm0              ; masolas

            shufps  xmm1, xmm1, 00001101b   ; xmm1 = (lenyegtelen, lenyegtelen, masodik_max2, elso_max2)
            shufps  xmm0, xmm0, 00001000b   ; xmm0 = (lenyegtelen, lenyegtelen, masodik_max1, elso_max1)

            andps   xmm0, xmm7              ; kimaszkoljuk a nem szukseges elemeket
            andps   xmm1, xmm7

            maxps   xmm0, xmm1              ; elso ket elem = ket uj ertek a matrixba

            ; vissza allitjuk az esi erteket
            sub     esi, edx                ; vissza egy sor

            ; visszamasolhatnank movups-el is, viszont a vegen lehetseges, hogy felulirnank nem lefoglalt memoriat
            movd    eax, xmm0               ; elso ertek
            stosd

            shufps  xmm0, xmm0, 00000001b   ; csak az utolso 2 bit fontos (01 = masodik elem kerul elsore)
            movd    eax, xmm0               ; masodik ertek
            stosd

            add     esi, 4 * 4              ; kovetkezo 2 max

            loop    .oszlop_pool

        mov     ecx, ebx                    ; ha maradt a vegen elem (akkor kell meg egy max_pool, ami csak egy 4 -> 1 muveletet vegez el)
        jecxz   .nincs_vegen
        ; egy maximum szamitas
            lodsd
            movd    xmm0, eax           ; elem1

            lodsd
            movd    xmm1, eax           ; elem2
            maxss   xmm0, xmm1          ; elso ket elem maximuma
            
            sub     esi, 8              ; visszalepes 2-t
            add     esi, edx            ; kovetkezo sor

            lodsd
            movd    xmm1, eax
            maxss   xmm0, xmm1          ; elso harom elem maximuma

            lodsd
            movd    xmm1, eax
            maxss   xmm0, xmm1          ; elso negy elem maximuma

            movd    eax, xmm0           ; max

            stosd

            sub     esi, edx            ; vissza egy sort

        .nincs_vegen:

        add     esi, edx                    ; egy sort kihagyunk (stride = 2)

        pop     ecx

        loop    .sorok_pool

    popa

    clc

    ret

Softmax:
; a vegeredmenybol valoszinuseget szamol (a valoszinusegek tombbe)
    pusha

    ; helyet foglalunk a valoszinuseg tombbnek
    mov     eax, 10 * 4                 ; 40 byte (10 elem)
    call    mem_alloc
    mov     [valoszinusegek], eax       ; lement pointer

    ; eloszor is kiszamitjuk a nevezot a softmax-hoz (taroljuk xmm1-ben)
    mov     ecx, 10                     ; 10 lehetseges szamjegy
    mov     esi, [e_vector]             ; eredmeny vector

    xorps   xmm1, xmm1                  ; nevezo = 0

    .nevezo_szamitas:

        xorps   xmm0, xmm0
        
        lodsd                           ; egy elem
        movd    xmm0, eax         

        call    exp_ss                  ; e ^ (jelenlegi elem)

        addss   xmm1, xmm0              ; nevezohoz adjuk

        loop    .nevezo_szamitas

    mov     ecx, 10                     ; 10 lehetseges szamjegy
    mov     esi, [e_vector]             ; eredmeny vector
    mov     edi, [valoszinusegek]       ; valoszinusegeket tarolo vector

    mov     eax, 100
    cvtsi2ss    xmm2, eax               ; xmm2 = 100

    .valoszinuseg_szamitas:

        xorps   xmm0, xmm0

        lodsd                           ; egy elem
        movd    xmm0, eax

        call    exp_ss                  ; e ^ (jelenlegi elem)

        divss   xmm0, xmm1              ; valoszinuseg kiszamitasa

        mulss   xmm0, xmm2

        movd    eax, xmm0

        stosd                           ; valoszinuseg lementese

        loop    .valoszinuseg_szamitas

    popa

    clc

    ret

Rectified_Linear_Unit:
; reLU implementalasa (f(x) = max(0,x))

    pusha   
    
    ; megnezzuk, ha 4-el oszthato a matrix merete, ha nem, akkor a vegen kulon elvegezzuk a reLU-t a maradek elemekre
    mov     eax, [e_vector_size]        ; elemek db
    mov     ebx, 4
    cdq
    idiv    ebx                         ; edx = maradek
    mov     ebx, edx                    ; taroljuk a maradekot

    ; most elvegezzuk a muveleteket a 4-el oszthato elemeken
    mov     eax, [e_vector_size]        ; elemek db
    sub     eax, ebx                    ; elemek db - amig 4-el oszthato lesz
    ; mivel egyszerre 4 muveletet vegzunk, ezert osszuk 4-el a muveletek szamat
    mov     esi, 4                      ; ezzel osszuk
    cdq     
    idiv    esi                         ; eax /= 4
    
    mov     ecx, eax                    ; lepesek szama
    mov     esi, [e_vector]             ; es visszatevese megfeleloen

    xorps   xmm0, xmm0                  ; elemek ide kerulnek
    xorps   xmm1, xmm1                  ; 0

    .muvelet_elvegezese:

        movups  xmm0, [esi]             ; 4 elem
        maxps   xmm0, xmm1              ; xmm0 = max(xmm0, 0)

        movups  [esi], xmm0             ; visszatesszuk az elemeket
        add     esi, 4 * 4              ; kovetkezo 4 elem

        loop    .muvelet_elvegezese
    
    ; most pedig elvegezzuk a maradek elemekre

    mov     ecx, ebx                    ; maradt elemek
    mov     edi, esi                    ; ahol maradt el oda fogjuk visszatenni is

    jecxz   .nincs_maradt_elem
    .maradt_elemek:

        lodsd                           ; egy maradt elem
        movd    xmm0, eax

        maxss   xmm0, xmm1
        movd    eax, xmm0               ; vissza

        stosd                           ; visszahelyezzuk

        loop    .maradt_elemek
    .nincs_maradt_elem:

    ; DEBUG
        mov     eax, DEBUG_RELU
        cmp     eax, 0
        je      .no_debug_relu
        ; eredmeny vector kiirasa
            mov     eax, relu_sz
            call    mio_writestr
            call    mio_writeln

            mov     eax, [e_vector]
            mov     [vector], eax
            mov     eax, [e_vector_size]
            mov     [vector_size], eax

            call    Beolvasott_vektor
            call    mio_writeln

        .no_debug_relu:

    popa

    clc

    ret

Matrix_x_vector_product:
; osszeszoroz egy matrixot egy vector-al (+ bias)
    pusha

    ; helyet foglalunk az eredmeny vectornak
        mov     eax, [matrix_row]           ; ennyi neuronunk van, tehat ennyi eleme lesz a matrix_x_vector eredmenye is
        mov     [e_vector_size], eax        ; e_vector merete
        
        imul    eax, 4                      ; mindegyik elem 4 byte

        call    mem_alloc
        mov     [e_vector], eax             ; e_vector pointer

    ; keszitunk egy xmm maszkot (elso 3 float 0, kovetkezo 0xFFFFFFFF)
        xor     eax, eax
        push    eax
        push    eax
        push    eax

        mov     eax, 0xFFFFFFFF
        push    eax

        movups  xmm6, [esp]             ; jobb felen 0xFFFFFFFF lesz

        pop     eax
        pop     eax
        pop     eax
        pop     eax

    ; a matrix es vector szorzasa
    mov     ecx, [matrix_row]           ; neuronok szama (ennyi sort kell osszeszorozni)
    mov     edx, [vector]               ; szorzando vector
    mov     esi, [matrix]               ; szorzando matrix
    mov     edi, [e_vector]             ; eredmeny vector

    .sor_loop:

        dec     ecx                     ; loop-- (short jump out of range)

        push    ecx                     ; sorok szama

        mov     eax, [matrix_collumn]   ; egy sor elemeinek szama (ilyen hosszu egy sor a matrixban)
        mov     ecx, 4
        cdq
        idiv    ecx                     ; elemek szama / 4 (edx = maradek)
        
        push    edx                     ; maradek elemek (amit majd kulon kell szorozni)

        mov     ecx, eax                ; elemek szama / 4
        mov     edx, [vector]           ; szorzando vector

        xorps   xmm7, xmm7              ; szumma = 0

        .oszlop_4_loop: 

            movups  xmm0, [edx]             ; 4 elem (x1 ... xn)
            add     edx, 4 * 4              ; kovetkezo 4 elem
 
            movups  xmm1, [esi]             ; matrix 4 eleme (w1,1 ... wn,m)
            add     esi, 4 * 4              ; kovetkezo 4 elem

            mulps   xmm0, xmm1              ; xmm0 = m_elem * v_elem (4x)

            movups  xmm2, xmm0              ; lement xmm0

            andps   xmm0, xmm6              ; csak a jobb felen levo float marad
            addss   xmm7, xmm0              ; szumma += weight * input (elso)

            movups  xmm0, xmm2              ; vissza
            shufps  xmm0, xmm0, 00000001b   ; csak az utolso 2 bit fontos, 00 = utolso marad a vegen, 01 = masodik, 10 = harmadik, 11 = negyedik
            andps   xmm0, xmm6              ; csak a jobb felen levo float marad
            addss   xmm7, xmm0              ; szumma += weight * input (masodik)

            movups  xmm0, xmm2              ; vissza
            shufps  xmm0, xmm0, 00000010b   
            andps   xmm0, xmm6              ; csak a jobb felen levo float marad
            addss   xmm7, xmm0              ; szumma += weight * input (harmadik)

            movups  xmm0, xmm2              ; vissza
            shufps  xmm0, xmm0, 00000011b  
            andps   xmm0, xmm6              ; csak a jobb felen levo float marad
            addss   xmm7, xmm0              ; szumma += weight * input (masodik)

            loop    .oszlop_4_loop

        ; most pedig a maradt elemeket kulon szorozzuk
        pop     ecx                         ; a maradek ez (push edx-tol)
        jecxz   .nem_maradt_oszlopban

        xorps   xmm0, xmm0
        xorps   xmm1, xmm1
        .maradt_oszlopban:

            mov     ebx, [edx]              ; vector egy eleme
            add     edx, 4                  ; kovetkezo elem index
            movd    xmm0, ebx               ; x1....xn

            lodsd                           ; matrix egy eleme
            movd    xmm1, eax               ; w1,1 ... wn,m

            mulss   xmm0, xmm1              ; xmm0 = m_elem * v_elem
            addss   xmm7, xmm0              ; szumma += weight * input

            loop    .maradt_oszlopban

        .nem_maradt_oszlopban:

        movd     eax, xmm7              ; eax = szumma
        stosd                           ; elhelyezzuk az eredmenybe

        pop     ecx

        cmp     ecx, 0
        jg      .sor_loop

    ; ezutan pedig hozza adhatjuk a neuronokhoz a bias ertekeket
    call    Bias_hozzaadas

    ; DEBUG
        mov     eax, DEBUG_MATRIX_VECTOR
        cmp     eax, 0
        je      .no_debug_matrix_vector
        ; eredmeny vector kiirasa
            mov     eax, ered_szoveg
            call    mio_writestr
            call    mio_writeln

            mov     eax, [e_vector]
            mov     [vector], eax
            mov     eax, [e_vector_size]
            mov     [vector_size], eax

            call    Beolvasott_vektor

        .no_debug_matrix_vector:

    popa

    clc

    ret

Bias_hozzaadas_conv:
; egy bias hozzaadasa egy tenzorhoz (e_vector) (a biast megkapja xmm0-ban)

    pusha

    mov     ecx, [matrix_row]           ; neuronok szama
    mov     esi, [e_vector]

    .e_vector_atjaras:
        
        movss   xmm1, [esi]             ; egy eleme az eredmenynek  
        addss   xmm1, xmm0

        movss   [esi], xmm1             ; eredmeny vissza helyezese

        add     esi, 4                  ; kovetkezo elem

        loop .e_vector_atjaras

    popa

    clc

    ret

Bias_hozzaadas:
; bias hozzaadasa minden neuronhoz (e_vector hoz)

    pusha

    mov     ecx, [matrix_row]           ; neuronok szama
    mov     esi, [bias]
    mov     edi, [e_vector]

    .e_vector_atjaras:
        
        xor     eax, eax
        xor     ebx, ebx

        mov     ebx, [edi]              ; ebx = e_vector egy eleme
        movd    xmm0, ebx               ; convert

        lodsd                           ; eax = egy bias ertek
        movd    xmm1, eax               ; convert

        addss   xmm0, xmm1              ; eax = elem + bias
        movd    eax, xmm0               ; vissza convert

        stosd                           ; elem visszahelyezese

        loop .e_vector_atjaras

    popa

    clc

    ret

Txt_allomany:
; feldolgozza a txt allomanyt
    pusha

    ; txt-bol valo olvasas
    xor     eax, eax
    mov     [bemenet_ind], eax

    mov     eax, 100
    call    mem_alloc
    mov     [bemenet], eax

    mov     eax, 1000
    call    mem_alloc

    mov     [szoveg], eax           ; txt pointer

    mov     eax, conv_txt           ; "conv_model.txt"
    xor     ebx, ebx                ; 0 - read mode

    call    fio_open                ; EAX = file handle

    mov     [txt_handle], eax
    mov     ebx, [szoveg]
    mov     ecx, 1000

    call    fio_read                ; EDX-be kerul a beolvasott byte-k szama

    mov     ebx, [szoveg]
    mov     [ebx + edx], byte 0     ; lezaro karakter

    mov     eax, [txt_handle]
    call    fio_close               ; close file

    ; txt allomany feldolgozasa
    mov     esi, [szoveg]
    xor     eax, eax

    .szoveg_atjaras:

        lodsb                   ; txt karakterei

        cmp     al, 0
        je      .vege_atjaras

        cmp     al, 'L'
        je      .linear

        cmp     al, 'R'
        je      .relu

        cmp     al, 'S'
        je      .softmax

        cmp     al, 'C'
        je      .convolutional

        cmp     al, 'M'
        je      .max_pool

        jmp     .kovetkezo_karakter

        .linear:

            lodsb
            cmp     al, 'i'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'n'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'e'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'a'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'r'
            jne     .kovetkezo_karakter

            ; berakjuk az L betut (linearis)
            mov     al, 'L'
            mov     ebx, [bemenet_ind]
            mov     edi, [bemenet]
            
            mov     [edi + ebx], al

            inc     ebx
            mov     [bemenet_ind], ebx

            .nem_szam:

                lodsb  

                cmp     al, '0'
                jl      .nem_szam

                cmp     al, '9'
                jg      .nem_szam

                sub     al, 48
                mov     ebx, eax        ; eddigi szam
                call    ReadInt_Txt

                ; berakjuk a szamot
                mov     ebx, [bemenet_ind]
                mov     edi, [bemenet]
                
                mov     [edi + ebx], eax

                add     ebx, 4          ; 4 byte
                mov     [bemenet_ind], ebx

                xor     eax, eax

            .nem_szam2:

                lodsb  

                cmp     al, '0'
                jl      .nem_szam2

                cmp     al, '9'
                jg      .nem_szam2

                sub     al, 48
                mov     ebx, eax        ; eddigi szam
                call    ReadInt_Txt

                ; berakjuk a szamot
                mov     ebx, [bemenet_ind]
                mov     edi, [bemenet]
                
                mov     [edi + ebx], eax

                add     ebx, 4          ; 4 byte
                mov     [bemenet_ind], ebx

            xor     eax, eax

            jmp     .kovetkezo_karakter

        .convolutional:

            lodsb
            cmp     al, 'o'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'n'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'v'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, '2'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'd'
            jne     .kovetkezo_karakter

            ; berakjuk az C betut (convolutional)
            mov     al, 'C'
            mov     ebx, [bemenet_ind]
            mov     edi, [bemenet]
            
            mov     [edi + ebx], al

            inc     ebx
            mov     [bemenet_ind], ebx

            .nem_szam3:

                lodsb  

                cmp     al, '0'
                jl      .nem_szam3

                cmp     al, '9'
                jg      .nem_szam3

                sub     al, 48
                mov     ebx, eax        ; eddigi szam
                call    ReadInt_Txt

                ; berakjuk a szamot
                mov     ebx, [bemenet_ind]
                mov     edi, [bemenet]
                
                mov     [edi + ebx], eax

                add     ebx, 4          ; 4 byte
                mov     [bemenet_ind], ebx

                xor     eax, eax

            .nem_szam4:

                lodsb  

                cmp     al, '0'
                jl      .nem_szam4

                cmp     al, '9'
                jg      .nem_szam4

                sub     al, 48
                mov     ebx, eax        ; eddigi szam
                call    ReadInt_Txt

                ; berakjuk a szamot
                mov     ebx, [bemenet_ind]
                mov     edi, [bemenet]
                
                mov     [edi + ebx], eax

                add     ebx, 4          ; 4 byte
                mov     [bemenet_ind], ebx

            xor     eax, eax

            jmp     .kovetkezo_karakter

        .max_pool:

            lodsb
            cmp     al, 'a'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'x'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'P'
            jne     .kovetkezo_karakter  

            lodsb
            cmp     al, 'o'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'o'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'l'
            jne     .kovetkezo_karakter 

            lodsb   
            cmp     al, '2'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'd'
            jne     .kovetkezo_karakter

            ; berakjuk az M betut (MaxPool2d)
            mov     al, 'M'
            mov     ebx, [bemenet_ind]
            mov     edi, [bemenet]
            
            mov     [edi + ebx], al

            inc     ebx
            mov     [bemenet_ind], ebx

            jmp     .kovetkezo_karakter

        .relu:

            lodsb
            cmp     al, 'e'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'L'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'U'
            jne     .kovetkezo_karakter

            ; berakjuk az R betut (ReLU)
            mov     al, 'R'
            mov     ebx, [bemenet_ind]
            mov     edi, [bemenet]
            
            mov     [edi + ebx], al

            inc     ebx
            mov     [bemenet_ind], ebx

            jmp     .kovetkezo_karakter

        .softmax:

            lodsb
            cmp     al, 'o'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'f'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 't'
            jne     .kovetkezo_karakter

            lodsb
            cmp     al, 'm'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'a'
            jne     .kovetkezo_karakter

            lodsb   
            cmp     al, 'x'
            jne     .kovetkezo_karakter

            ; berakjuk az S betut (Softmax)
            mov     al, 'S'
            mov     ebx, [bemenet_ind]
            mov     edi, [bemenet]
            
            mov     [edi + ebx], al

            inc     ebx
            mov     [bemenet_ind], ebx

            jmp     .kovetkezo_karakter

        .kovetkezo_karakter:

            jmp    .szoveg_atjaras

    .vege_atjaras:

    mov     ebx, [bemenet]
    mov     eax, [bemenet_ind]

    mov     [ebx + eax], byte 0     ; lezarjuk a karakterlancot
    ; DEBUG
        mov     eax, DEBUG_TXT  
        cmp     eax, 0
        je      .no_debug_txt
        call    Txt_allomany_kiiras             ; megjelenitjuk a feldolgozott ertekeket
        .no_debug_txt:

    popa

    clc

    ret

Pixelek_skalazasa:
; skalazza a pixeleket (0 - 255 -> -1 - 1)
    pusha

    ; eloszor is kivonunk 128-at minden pixelbol, igy a [-128, 128) intervallumba kerulnek
    ; egyuttal atalakitjuk float ertekekke es lementjuk a veg_scaled vektorba

    ; lementunk egy 128.0 float erteket az xmm1 regiszter mind 4 reszebe
    xorps   xmm1, xmm1          ; xmm1 = 0.0
    mov     eax, 128
    cvtsi2ss    xmm1, eax       ; xmm1 = 128.0
    movd    eax, xmm1           ; convert

    push    eax
    push    eax
    push    eax
    push    eax
    movups  xmm1, [esp]
    pop     eax
    pop     eax
    pop     eax
    pop     eax

    mov     esi, [veg_matrix]   ; 28 x 28 1byte-s pixel ertekek
    mov     edi, [veg_scaled]   ; 28 x 28 (x 4) 4 byte-s float ertekek
    mov     ecx, 196            ; egyszerre 4 elemmel dolgozunk ezert 784 / 4 lepes lesz

    .elemek_4_loop:
    ; eloszor at konvertaljuk az ertekeket float ertekke
        xorps   xmm0, xmm0

        xor     eax, eax
        lodsb                           ; egy byte = 1 pixel
        cvtsi2ss    xmm0, eax           ; 1 elem
        movd        eax, xmm0
        stosd                           ; temporalisan bekerul a celba

        xor     eax, eax
        lodsb                           ; egy byte = 1 pixel
        cvtsi2ss    xmm0, eax           ; 2 elem
        movd        eax, xmm0
        stosd                           ; temporalisan bekerul a celba

        xor     eax, eax
        lodsb                           ; egy byte = 1 pixel
        cvtsi2ss    xmm0, eax           ; 3 elem
        movd        eax, xmm0
        stosd                           ; temporalisan bekerul a celba

        xor     eax, eax
        lodsb                           ; egy byte = 1 pixel
        cvtsi2ss    xmm0, eax           ; 4 elem
        movd        eax, xmm0
        stosd                           ; temporalisan bekerul a celba

        movups      xmm0, [edi - 4 * 4] ; a 4 temporalis elem
        subps       xmm0, xmm1          ; -128
        divps       xmm0, xmm1          ; 4 elem / 128 (skalazas)

        movups      [edi - 4 * 4], xmm0 ; visszahelyezes

        loop    .elemek_4_loop

    popa

    clc

    ret

Pixelek_osszevonasa:
; 4 pixelbol csinal 1-et, a neuralis halo szamara
    pusha

    mov     esi, [kis_matrix]   ; 28 x 28 (x 4) matrixunk
    mov     edi, [veg_matrix]   ; 28 x 28 vegso matrixunk

    mov     ecx, 28             ; sorok szama

    .sorok_loop:

        push    ecx             ; push sorok

        mov     ecx, 28

        .oszlopok_loop:

            movsb               ; veg_matrix <- matrix (1 byte)

            add     esi, 3      ; kihagyjuk a 2 szines pixelt es az alpha pixelt

            loop    .oszlopok_loop

        pop     ecx             ; sorok

        loop    .sorok_loop

    popa

    clc

    ret

Kep_atmeretezes:
; minden 9 pixelbol csinal 1-et interpolalva (3x3 -> 1)
    pusha

    mov     esi, [regi_kep]         ; regi kep pointer

    mov     edi, [uj_kep]           ; uj kep pointer
    mov     ecx, [uj_meret]         ; sorok

    .sorok_atjaras:

        dec     ecx                 ; sorok--

        push    ecx
        mov     ecx, [uj_meret]

        .3x3_1x1:

            dec     ecx             ; oszlopok--

            xor     eax, eax
            xor     ebx, ebx
            xor     edx, edx        ; pixelek osszege = 0

            .elso_sor:

                mov     al, [esi]       ; felso bal
                add     edx, eax        ; pixelek hozza adasa

                mov     al, [esi + 4]   ; felso kozep
                add     edx, eax

                mov     al, [esi + 8]   ; felso jobb
                add     edx, eax
            
            .masodik_sor:

                mov     ebx, [regi_meret]       ; tulajdonkeppen egy sor hossza
                imul    ebx, 4

                mov     al, [esi + ebx]         ; kozep bal
                add     edx, eax

                mov     al, [esi + ebx + 4]     ; kozep kozep
                add     edx, eax

                mov     al, [esi + ebx + 8]     ; kozep jobb
                add     edx, eax

            .harmadik_sor:

                imul    ebx, 2                  ; 2 sor hossza

                mov     al, [esi + ebx]         ; kozep bal
                add     edx, eax

                mov     al, [esi + ebx + 4]     ; kozep kozep
                add     edx, eax

                mov     al, [esi + ebx + 8]     ; kozep jobb
                add     edx, eax

            ; pixelek ossze adva
            add     esi, 12         ; kovetkezo 3 pixel !!! (1 pixel 4 byte, ezert 3 * 4 byte-t lepunk elore)

            mov     eax, edx        ; pixelek osszege
            mov     ebx, 9
            cdq
            idiv    ebx             ; hanyados az eax-ben

            ; behelyezzuk a cel matrixba
            stosb
            stosb
            stosb
            mov     al, 0

            stosb

            .vege_oszlop:

                cmp     ecx, 0
                jg      .3x3_1x1

        xor     eax, eax

        mov     eax, [regi_meret]
        imul    eax, 8              ; kovetkezo 2 sor kihagyasa

        add     esi, eax

        pop     ecx                 ; a sorok indexe
        
        cmp     ecx, 0
        jg    .sorok_atjaras

    popa

    clc

    ret
  
Letrehozott_kep:
; megjelenit egy matrixot kepkent
    pusha

    mov     eax, [grafika_merete]   ; grafikus felulete merete (negyzet)
    mov     ebx, [grafika_merete]
    xor     ecx, ecx                ; windowed mode
    mov     edx, caption            ; szoveg

    call    gfx_init
    call    gfx_map

    mov     edi, eax                ; kep pointer
    mov     esi, [mas_pointer]

    xor     ebx, ebx    
    mov     ecx, [mas_meret]

    .elemek_atmasolasa:

        movsb                       ; EDI <- ESI byte-k

        loop    .elemek_atmasolasa

    .rajz_mutatas:

        call    gfx_unmap
        call    gfx_draw

        call    gfx_getevent        ; esc es ablak bezarasa

        cmp     eax, 23
        je      .vege_mutatas

        cmp     eax, 27
        je      .vege_mutatas

        jmp     .rajz_mutatas

    .vege_mutatas:

        call    gfx_destroy

        popa

        clc

        ret

Rajzolas:
; rajzolasi felulet es kezdeti matrix eltarolasa
    mov     eax, WIDTH              ; x
    mov     ebx, HEIGHT             ; y

    xor     ecx, ecx                ; window mode
    mov     edx, caption            ; az ablak szovege

    call    gfx_init

    cmp     eax, 0                  ; hiba eseten 0
    jne     .kezdes

    ; hiba eseten
	mov		eax, hiba_grafika
	call	io_writestr
	call	io_writeln

    .kezdes:

        lea     esi, [WIDTH / 2]    ; esi = szelesseg fele (gomboknak)
        call    gfx_map             ; eax-be kerul a pointer (map framebuffer)

        mov     [pointer], eax      ; lementjuk a pointert
        xor     ecx, ecx            ; sorok szama = 0

    .y_loop:

        cmp     ecx, WIDTH          ; ha mar atjartuk a sorokat (negyzetig megyunk, itt)
        jge     .y_loop_gomb

        xor     edx, edx            ; oszlopok szama = 0

    .x_loop:

        cmp     edx, WIDTH
        jge     .x_vege

        ; blue
        mov		[eax], byte 0
        ; green
        mov		[eax + 1], byte 0
        ; red
        mov		[eax + 2], byte 0
        ; zero
        mov		[eax + 3], byte 0
        
        add		eax, 4		    ; kovetkezo pixel
        inc		edx             ; oszlop++

        jmp		.x_loop

    .x_vege:

        inc     ecx             ; sor++
        jmp		.y_loop

    .y_loop_gomb:
    ; itt pedig a piros es zold 'gombok' pixeleit inicializaljuk

        cmp     ecx, HEIGHT ; ha mar atjartuk a sorokat a negyzet alatt
        jge     .y_vege

        xor     edx, edx    ; oszlopok szama = 0

    .x_loop_gomb:

        cmp     edx, WIDTH
        jge     .x_vege_gomb

        cmp     edx, esi        ; szelesseg fele
        jge     .x_zold
        jmp     .x_piros

        .x_zold:

            ; blue
            mov		[eax], byte 0
            ; green
            mov		[eax + 1], byte 128
            ; red
            mov		[eax + 2], byte 0
            ; zero
            mov		[eax + 3], byte 0

            jmp     .vege_also

        .x_piros:

            ; blue
            mov		[eax], byte 0
            ; green
            mov		[eax + 1], byte 0
            ; red
            mov		[eax + 2], byte 128
            ; zero
            mov		[eax + 3], byte 0
        
        .vege_also:

            add		eax, 4		    ; kovetkezo pixel
            inc		edx             ; oszlop++

            jmp		.x_loop_gomb

    .x_vege_gomb:

        inc     ecx                 ; sor++
        jmp		.y_loop_gomb

    .y_vege:

        call	gfx_unmap		    ; EAX - pointer eleje
        call	gfx_draw		    ; EAX pointertol rajzol

    .event_loop:
    ; itt kezeljuk le a rajzolast, illetve a ket 'gomb' lenyomasat

        call	gfx_getevent        ; kovetkezo input, 0 - ha nincs input
        
        cmp		eax, 1			    ; bal mouse button lett lenyomva
        jne		.left_not_pressed

        ; megnezzuk, ha nincs e a 2 gomb reszen
        call	gfx_getmouse	    ; EAX - x, EBX - y

        cmp     ebx, WIDTH          ; ha benne van e a negyzetbe
        jge     .benne_van
        jmp     .nincs_benne

        .benne_van:

            cmp     eax, esi
            jge     .rajz_vege      ; vege a rajznak (zold zona)

            ; ellenkezo esetben pedig a piros zonaban volt, tehat frissitjuk a rajz feluletet
            jmp     .kezdes

        .nincs_benne:

            mov		dword [movemouse], 1    ; true (bool) mouse pressed

            jmp		.event_loop             ; vissza

    .left_not_pressed:

        cmp		eax, -1			            ; left button released
        jne		.mouse_moving

        mov		dword [movemouse], 0        ; false (bool) nem meg mozditjuk a mouse-t
        jmp		.event_loop

    .mouse_moving:
        ; kilepesi feltetel
        cmp		eax, 23			; ablak piros x
        je		.rajz_vege

        cmp		eax, 27			; ESC billentyu
        je		.rajz_vege

        cmp     eax, 0		    ; ha nem kell update-lni
        jne		.event_loop
        
        ; ha nem mozdul a mouse, nem kell rajzolni
        cmp		dword [movemouse], 0
        je		.event_loop

        call	gfx_getmouse	    ; EAX - x, EBX - y

        lea     ecx, [WIDTH - BRUSH_SIZE / 2]
        cmp     ebx, ecx            ; abban az esetben, ha a gombokra rajzolnank
        jge     .event_loop

        imul    eax, 4              ; x * 4
        imul    ebx, 4              ; y * 4

        mov     edx, WIDTH          ; szelesseg
        imul    edx, ebx            ; szelesseg * sorok_szama
        add     edx, eax            ; + oszlopok

        mov     edi, [pointer]
        add     edi, edx            ; igy megkaptuk a matrixban a jelenlegi poziciot

        mov     edx, WIDTH
        imul    edx, 4              ; szelesseg * 4 (kepmatrixban egy sor)

        ; tulajdonkeppen egyszerre 15x15 pixelre rajzolunk
        mov     eax, 0x00FFFFFF     ; 3 db FF (feher) ertek es 1 db 0 ertek

        lea     ecx, [BRUSH_SIZE / 2]   ; visszalepunk BRUSH_SIZE / 2 sort es (BRUSH_SIZE / 2)x4 pixelt           
        .vissza_lepes:

            sub     edi, edx            ; sor--
            sub     edi, 4              

            loop   .vissza_lepes
        
        mov     ecx, BRUSH_SIZE                 ; sorok (ecx = pixelek szama)
        .sorok_rajzolas:

            push    ecx
            mov     ecx, BRUSH_SIZE             ; oszlopok
            .oszlopok_rajzolas:

                cmp     edi, [pointer]          ; tulajdonkeppen, ha a matrixon kivul van akkor oda nem rajzolunk
                jl      .rossz_helyre_rajzolas

                stosd

                loop    .oszlopok_rajzolas

            sub     edi, BRUSH_SIZE * 4         ; vissza all a kezdeti poziciora

            .rossz_helyre_rajzolas:             ; ha rossz helyre rajzoltunk, akkor ideszokhetunk
            add     edi, edx                    ; kovetkezo sor
            pop     ecx
            loop    .sorok_rajzolas

        call	gfx_unmap		; EAX - pointer eleje
        call	gfx_draw		; EAX pointertol rajzol

        jmp 	.event_loop
        
    ; vege
    .rajz_vege:

        xor     eax, eax    ; byte
        xor     ebx, ebx    ; index
        xor     edx, edx    ; cim

        mov     edx, [nagy_matrix]
        mov     esi, [pointer]

        lea     ecx, [WIDTH * WIDTH * 4]

        .masolas:

            lodsb

            mov     [edx + ebx], al     
            inc     ebx

            loop    .masolas

        call	gfx_destroy

        clc

        ret

Feltolt_0:
; egy matrixot feltolt 0-s elemekkel
    pusha

    mov     edi, [matrix]               ; a matrix
    mov     ecx, [matrix_size]          ; matrix elemei (db)

    xorps   xmm0, xmm0                  ; xmm0 = 0
    movd    eax, xmm0

    .feltoltes:

        stosd                           ; behelyezes nullas elemek

        loop    .feltoltes

    popa

    clc

    ret

Masolas_vector:
; egy vector masolasa egy masikba (masolt_vect = honnan, cel_vect = hova, masolt_size = hany)

    pusha

    ; masolas
    mov     esi, [masolt_vect]      ; masolando vector
    mov     edi, [cel_vect]         ; cel vector
    mov     ecx, [masolt_size]

    .masolas:

        movsd

        loop    .masolas  

    popa

    clc

    ret

Matrixok_osszeadasa:
; osszead ket matrixot (matrix = cel, added_matrix = honnan, matrix_size = elemek db)
    pusha

    mov     esi, [added_matrix]         ; hozzaadando matrix
    mov     edi, [matrix]               ; cel matrix
    mov     ecx, [matrix_size]          ; elemek db

    xorps   xmm0, xmm0
    xorps   xmm1, xmm1

    .hozzadas_elemek:

        lodsd                           ; egy elem a hozzadando matrixbol
        movd    xmm0, eax

        mov     ebx, [edi]              ; egy elem a cel matrixbol
        movd    xmm1, ebx

        addss   xmm0, xmm1              ; osszeadjuk

        movd    eax, xmm0               ; eredmeny

        stosd                           ; eredmeny tarolasa

        loop    .hozzadas_elemek

    popa

    clc

    ret

ReadInt_Txt:
; Specialiasan atalakitott ReadInt, ami a txt allomany feldolgozasanal hasznalt
	
	push 	ebx 	; szam felepites
	push 	ecx 	; szamjegy szamolo
	push 	edx 	; szorzas modositja

	xor 	eax, eax 	; eax = 0
	xor 	ecx, ecx 	; elojel jelzo regiszter = 0

	;eloszor is megnezzuk, ha az elsonek beolvasott ertek egy elojel jelzo ertek volt-e

	xor 	eax, eax 	; eax = 0

	.feldolgozas:

		xor 	eax, eax 				; eax = 0

		lodsb							; szamjegyek sorba (esi++)

		cmp 	al, ',' 				; specialis esetek txt kezelesnel
		je 		.decimal_done

        cmp 	al, ')' 				
		je 		.decimal_done
		
		cmp 	al, 48			
		jl		.error					; < '0'
		
		cmp 	al, 57					; > '9'
		jg		.error
		
		; tehat ha eljut ide akkor biztos decimalis (0 - 9)
	
		sub 	al, 48					; '0' -> 0, tehat megvan a szamjegyunk

		imul 	ebx, 10 				; szorozzuk az eddigi szamot
		jo 		.error

		add 	ebx, eax 				; hozza adjuk a szamjegyet
		jo 		.error

		jmp 	.feldolgozas

	.error:

		pop 	edx
		pop 	ecx	
		pop 	ebx

		stc

		ret

	.decimal_done:

		mov 	eax, ebx

        pop 	edx
        pop 	ecx	
        pop 	ebx

        clc

        ret

Valoszinuseg_megjelenitese:
; megjeleniti a vegso valoszinusegeket formazva
    pusha

    ; vector altalanos pointert hasznaljuk
    mov     eax, [valoszinusegek]           ; kiirando vector
    mov     [vector], eax       
    mov     [vector_size], dword 10         ; merete

    xor     ebx, ebx
    xor     edx, edx                ; maximalis ertekkel rendelkezo indexet tarolja
    mov     ecx, [vector_size]      ; mennyit iratunk ki
    mov     esi, [vector]           ; honnan

    xorps   xmm1, xmm1              ; maxss-nek
    xorps   xmm2, xmm2              ; temp tarolni
    .vector_kiiras:

        xor     eax, eax

        mov     eax, ebx
        call    io_writeint

        mov     eax, ':'
        call    mio_writechar

        mov     eax, ' '
        call    mio_writechar

        lodsd                       ; 4 byte-s ertek
        movd    xmm0, eax           ; behelyezzuk egy xmm regiszterbe

        movss   xmm2, xmm1          ; masolas
        maxss   xmm1, xmm0          ; maximumot szamitunk, ha valtozik xmm1, akkor talaltunk egy nagyobb elemet

        comiss  xmm1, xmm2 
        jz      .nem_maximum

        mov     edx, ebx            ; lement index

        .nem_maximum:

        call    io_writeflt

        mov     al, '%'
        call    mio_writechar
        call    mio_writeln

        inc     ebx

        loop    .vector_kiiras

    call    mio_writeln
    mov     eax, rajzolt_sz
    call    mio_writestr

    mov     eax, edx                ; index
    call    io_writeint

    popa

    clc

    ret

Beolvasott_vektor:
; kiir a kepernyore egy vectort
    pusha

    xor     ebx, ebx
    mov     esi, [vector]           ; honnan
    mov     ecx, [vector_size]      ; mennyit iratunk ki

    .vector_kiiras:

        xor     eax, eax

        lodsd                       ; 4 byte-s ertek

        movd    xmm0, eax           ; behelyezzuk egy xmm regiszterbe

        call    WriteFloat

        mov     al, ' '
        call    mio_writechar

        inc     ebx

        ;cmp     ebx, 4
        ;je      .uj_sor
        
        .vissza:

            loop    .vector_kiiras

    jmp     .vege_kiiras

    .uj_sor:

        call    mio_writeln
        xor     ebx, ebx

        jmp     .vissza

    .vege_kiiras:

        call    mio_writeln
        call    mio_writeln

        popa

        clc

        ret

Txt_allomany_kiiras:
; kiirja a kepernyore a txt allomany feldolgoztt adatait
    pusha

    xor     eax, eax
    xor     ebx, ebx

    mov     esi, [bemenet]

    .atjaras:

        lodsb

        cmp     al, 0           ; ha a vegen vagyunk
        je      .vege_atjaras

        cmp     al, 'L'
        je      .linear_kiiras

        cmp     al, 'C'
        je      .conv_kiiras

        call    mio_writechar
        mov     al, ' '
        call    mio_writechar

        jmp     .atjaras

        .linear_kiiras:

            call    mio_writechar
            mov     al, ' '
            call    mio_writechar

            lodsd

            call    io_writeint
            mov     al, ' '
            call    mio_writechar

            lodsd

            call    io_writeint
            mov     al, ' '
            call    mio_writechar

            jmp     .atjaras

        .conv_kiiras:

            call    mio_writechar
            mov     al, ' '
            call    mio_writechar

            lodsd

            call    io_writeint
            mov     al, ' '
            call    mio_writechar

            lodsd

            call    io_writeint
            mov     al, ' '
            call    mio_writechar

            jmp     .atjaras

        jmp     .atjaras

    .vege_atjaras:

        call    mio_writeln

        popa

        ret

Beolvas_vector:
; kepernyorol beolvas egy vectort
    pusha   

    call    io_readint
    mov     [vector_size],eax           ; vector hossz
    call    mem_alloc
    mov     [vector], eax               ; lement pointer

    mov     ecx, [vector_size]          ; beolvasando vector hossza
    mov     edi, [vector]

    .beolvas_loop:

        call    io_readflt

        movd    eax, xmm0

        stosd

        loop    .beolvas_loop

    popa

    clc

    ret

Beolvas_matrix:
; kepernyorol beolvas egy matrixot
    pusha

    mov     al, 'x'
    call    mio_writechar
    mov     al, ':'
    call    mio_writechar

    call    io_readint

    mov     [matrix_row], dword 0
    mov     [matrix_row], eax       ; sorok szama

    mov     ecx, eax                ; lement sorok szama

    mov     al, 'y'
    call    mio_writechar
    mov     al, ':'
    call    mio_writechar

    call    io_readint

    mov     [matrix_collumn], eax   ; oszlopok szama

    imul    ecx                     ; a matrix merete
    mov     [matrix_size], eax

    mov     ecx, eax
    call    mem_alloc
    mov     [matrix], eax           ; lement pointer

    mov     edi, [matrix]

    .beolvas_matrix:

        call    io_readflt

        movd    eax, xmm0

        stosd

        loop    .beolvas_matrix

    popa

    clc

    ret

Kiir_matrix:
; kiir a kepernyore egy adott matrixot
    pusha

    xor     ebx, ebx                ; elem szamolo
    mov     esi, [matrix]           ; pointer
    mov     ecx, [matrix_size]
    mov     edx, [matrix_collumn]   ; oszlop szamolo

    .kiir_matrix:

        lodsd

        movd    xmm0, eax

        call    WriteFloat

        mov     al, ' '
        call    mio_writechar

        inc     ebx

        cmp     ebx, edx
        jge     .new_line

        .vissza:

            loop    .kiir_matrix

    jmp     .vege_kiiras

    .new_line:

        call    mio_writeln

        xor     ebx, ebx

        jmp     .vissza

    .vege_kiiras:

        call    mio_writeln

        popa

        clc

        ret

WriteFloat:
; debuggolni specialisan atalakitott float kiiras

    push    eax     ; konverziohoz
    push    ecx     ; elojel
    ; eloszor kiirjuk a pont elotti reszt
    ; megnezzuk, ha negativ szam-e
    mov     ecx, 0x80000000     ; elso bit 1 tobbi 0

    movd    eax, xmm0           ; eax = xmm0 bitjeit

    and     ecx, eax            ; ha negativ, akkor ecx elso bitje 1 marad

    cmp     ecx, 0x80000000     ; ha negativ, akkor kiirunk egy '-' jelet
    je      .negative_float
    jmp     .feldolgozas

    .negative_float:

        push    eax

        mov     eax, '-'
        call    mio_writechar

        pop     eax

        and     eax, 0x7FFFFFFF ; bittekben 011111...111

        movd    xmm0, eax

        jmp     .feldolgozas

    .feldolgozas:

        mov     ecx, DEBUG_FLOAT_ACCURACY   ; pontossag

        mov     eax, 10
        cvtsi2ss    xmm2, eax               ; XMM2 = 10

        cvttss2si   eax, xmm0               ; convert

        call    io_writeint                 ; kiirjuk a szam egesz reszet

        mov     al, '.'
        call    mio_writechar

        roundss     xmm1, xmm0, 1

        subss   xmm0, xmm1                  ; szam = szam - egeszresz

        .pont_utan:

            xor     eax, eax

            mulss   xmm0, xmm2              ; .utani szamjegyek .ele kerulnek 1 enkent

            cvttss2si   eax, xmm0           ; eax = kovetkezo szamjegy

            roundss     xmm1, xmm0, 1

            subss   xmm0, xmm1              ; szam = szam - egeszresz

            add     eax, 48

            call    mio_writechar

            loop    .pont_utan

        pop     ecx
        pop     eax


        ret
;=======================================================================================================================================================
section .data
;=====================================================================STRINGS===========================================================================
    caption         db      "Rajzolasi felulet", 0
	hiba_grafika    db      "Hiba: Nem sikerult inicializalni a grafikus feluletet", 0
    hiba_weights    db      "Helytelen beolvasas binaris file-nal (weights)", 0
    hiba_bias       db      "Helytelen beolvasas binaris file-nal (bias)", 0
    hiba_filter     db      "Helytelen beolvasas binaris file-nal (filter)", 0
    rajzolt_sz      db      "A rajzolt szamjegy: ", 0
    conv_txt        db      "conv_model.txt", 0
    conv_bin        db      "conv_model.bin", 0
    bias_sz         db      "bias:", 0
    weights_sz      db      "weights:", 0
    filters_sz      db      "filters: ", 0
    padding_sz      db      "padding: ", 0
    osszeadott_sz   db      "osszevonva: ", 0
    relu_sz         db      "reLU: ", 0
    konvolucio_sz   db      "konvolucios muveletek: ", 0
    ered_szoveg     db      "eredmeny vector:", 0
    max_pool_sz     db      "max pool eredmenye: ", 0
;=======================================================================================================================================================
section .bss
;=====================================================================VARIABLES=========================================================================
	movemouse           resd    1   ; (bool valtozo)
    pointer             resd    1   ; rajzolasnal pointer
    nagy_matrix         resd    1   ; 252 x 320 (x 4) matrix pointer
    kozep_matrix        resd    1   ; 84 x 84 (x 4) matrix pointer
    kis_matrix          resd    1   ; 28 x 28 (x 4) matrix pointer
    veg_matrix          resd    1   ; 28 x 28 matrix (1 pixel 1 byte)
    veg_scaled          resd    1   ; 28 x 28 (x 4) float matrix
    veg_scaled_size     resd    1   ; ebben taroljuk a meretet, mikor eljut a linearis reteghez

    grafika_merete      resd    1   ; grafika meretet (x * y)
    mas_pointer         resd    1   ; masolas pointer
    mas_meret           resd    1   ; masolas meret

    regi_kep            resd    1   ; regi kep pointer
    regi_meret          resd    1   ; regi kep merete
    uj_kep              resd    1   ; uj kep pointer
    uj_meret            resd    1   ; atmeretezes uj merete

    txt_handle          resd    1   ; txt file handle
    bin_handle          resd    1   ; bin file handle

    szoveg              resd    1   ; txt pointer
    bemenet             resd    1   ; tarolni a bemenet adatait (pointer)
	bemenet_ind         resd    1   ; bemenet indexe

    weights             resd    1   ; linearis reteg vektoranak az indexe
    weights_size        resd    1   ; linearis reteg merete (byte-ban)
    in_features         resd    1   ; linear reteg bemeneti neuronok
    out_features        resd    1   ; linear reteg kimenetei neuronok
    bias                resd    1   ; bias ertekek pointer
    bias_size           resd    1   ; bias vector merete

    vector              resd    1   ; vector indexe kiirashoz
    vector_size         resd    1   ; vector merete kiirashoz   
    matrix              resd    1   ; szorzando matrix pointer
    matrix_row          resd    1   ; matrix sorainak db
    matrix_collumn      resd    1   ; matrix oszlopainak db
    matrix_size         resd    1   ; szorzando matrix meret
    e_vector            resd    1   ; eredmeny vector
    e_vector_size       resd    1   ; eredmeny vector meret
    temp_vector         resd    1   ; temp vector
    temp_vector_size    resd    1   ; temp vector meret

    elso_bool           resd    1   ; bool valtozo (0 = nem volt mar linearis reteg, 1 = volt mar linearis reteg)

    masolt_vect         resd    1   ; masolashoz pointer (kezdeti vector)
    masolt_size         resd    1   ; masolt vector merete
    cel_vect            resd    1   ; cel vector pointer
    cel_size            resd    1   ; cel vector merete   

    valoszinusegek      resd    1   ; valoszinusegeket tarolo vector  

    in_channels         resd    1   ; konvolucios halonal a szorzando matrixok szama
    in_channels_size    resd    1   ; a szorzando matrix merete
    in_channels_pad     resd    1   ; in_channels_size padding-el
    in_rows             resd    1   ; szorzando matrix sorainak merete
    in_rows_pad         resd    1   ; in_rows erteke padding-el
    out_channels        resd    1   ; konvolucios halonal a letrehozott uj matrixok szama    
    elso_conv           resd    1   ; bool valtozo, elso konvolucios retegnel csak 1 input lehet 
    pointer_tomb_in     resd    1   ; egy tomb ami pointereket tarol az in_channels-ekre   
    pointer_tomb_filter resd    1   ; egy tomb ami pointereket tarol az out_channels-ekre (filtereket)
    pointer_tomb_ered   resd    1   ; a matrix szorzasok eredmenye ide kerul   
    pointer_tomb_temp   resd    1   ; a vegso szumma elott a konvolucios muveletek erteke ide kerul
    temp_db             resd    1   
    melyseg             resd    1   ; a konvolucios halo melyseget jegyzi meg 

    pool                resd    1   ; max_pool pointer (ezt szeretnenk atmeretezni)
    pool_row            resd    1   ; sorok szama
    pool_size           resd    1   ; elemek szama
    new_pool            resd    1   ; atmeretezett matrix
    new_pool_row        resd    1   ; uj sorok szama
    new_pool_size       resd    1   ; atmeretezett matrix merete

    padding             resd    1   ; ezt a matrixot szeretnenk korbevenni
    padding_size        resd    1   ; matrix merete
    padding_rows        resd    1   ; atmeretezendo matrix sorainak szama
    new_padding         resd    1   ; uj matrix paddinggel pointer
    new_padding_size    resd    1   ; uj matrix paddinggel meret (db)
    new_padding_rows    resd    1   ; uj matrix paddinggel sorok

    k_matrix            resd    1   ; konvolucios muveletnel az input_matrix
    k_matrix_row        resd    1   ; tulajdonkeppen in_rows * 4 (kovetkezo sorba lepeshez)
    k_filter            resd    1   ; konvolucios muveletnel a filter
    k_temp              resd    1   ; konvolucios muveletnel a letrehozott temp matrix
    add_temp            resd    1   ; egy temporalis 3x3 matrix
    atjart_elemek       resd    1   ; szamolja hany elemre vegeztuk el a konvoluciot a matrixban

    added_matrix        resd    1   ; ket matrix osszeadasnal a hozzaadott matrix

    reLU_mode           resd    1   ; jelzi a reLU retegnek, hogy linearis reteg vagy konvolucios reteg utan kovetkezik (0 = linear, 1 = conv)
    volt_conv           resd    1   ; bool valtozo, ami jelzi a linearis retegnek, ha elotte conv reteg volt
;=======================================================================================================================================================