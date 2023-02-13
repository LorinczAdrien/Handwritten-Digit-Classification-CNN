%ifndef __GFX_INC__
%define __GFX_INC__

; Creates the graphics window
; In:
;   EAX - window width in pixels (X)
;	EBX	- window height in pixels (Y)	
;   ECX - window mode: 0 (window), 1 (fullscreen)
;   EDX - window title string pointer (can be 0)
; Out:
;   EAX - result: 0 (error), 1 (success)
extern gfx_init

; Destroys the graphics window
extern gfx_destroy

; Maps the framebuffer
; Returns a pointer to an *uninitialized* image (array of pixels)
; Out:
;	EAX - pointer to the framebuffer (top-left pixel)
;
; Use this to get a pointer to a pixel buffer which you can use to draw on the screen
; The size of a pixel is 4 bytes and contains the following values in *this* order: B, G, R, 0 (blue, green, red, zero/not used)
; The buffer starts with the first line of pixels, then the second, etc.
; Changing the contents of this buffer will *not* be immediatly visible! Initialize all pixels, then call gfx_unmap, and finally gfx_draw!
; Always initialize all pixels!
extern gfx_map

; Unmaps the framebuffer
extern gfx_unmap

; Draws the contents of the framebuffer
; Call once in each iteration of the main loop
extern gfx_draw

; Gets the next keyboard/mouse/window event
; Out:
;	EAX - code of the event (32-bit *signed* integer)
;
; The events are queued and this function will return *all* new events
; If there are no more events, the return value is 0
; *Always* call this function in a loop (event loop) until 0, in each iteration of the main loop!
; The event code can be a positive or negative (!) ASCII code (keyboard event) or a mouse button index (mouse event)
; Mouse button indexes: 1 (left), 2 (middle), 3 (right), 4 (wheel up), 5 (wheel down)
; Special ASCII code: 23 (window close button -> quit)
; If the code is POSITIVE, the key/button was pressed (e.g. 1 - the left mouse button was pressed)
; If the code is NEGATIVE, the key/button was released (e.g. -'w' - the W key was released)
extern gfx_getevent

; Gets the current coordinates of the mouse
; Out:
;	EAX - X coordinate
;	EBX - Y coordinate
extern gfx_getmouse

; Shows/hides the cursor
; In:
;	EAX - 0 (hide), 1 (show)
extern gfx_showcursor

%endif