; Copyright (C) 1994 DJ Delorie, see COPYING.DJ for details
;
; $Id$
; $Log$
; Revision 1.1  2000-07-13 06:30:40  michael
; + Initial import
;
; Revision 1.1  1998/12/21 13:07:03  peter
;   * use -FE
;
; Revision 1.1.1.1  1998/03/25 11:18:42  root
; * Restored version
;
; Revision 1.2  1997/11/27 16:28:13  michael
; Change submitted by Pierre Muller.
;
; Revision 2.0  1994/03/14  00:47:04  dj
; initial version
;
;

;-----------------------------------------------------------------------------
;  sbrk 16-bit helper
;
;  Transferred to 16-bit code segement to run in protected mode.
;  Will make DPMI segment altering requests and update selectors
;  as needed.  Image will always need to allocate an exact
;  multiple of 16 bytes, load offset will always be zero.
;  Number of bytes to copy will always be multiple of four.
;
;  Application must set cs_selector, ds_selector, and local_ds
;  appropriately.  Application uses first word in image to find
;  API entry point.  Call with FAR call.
;
;  Call with:   BX:CX = new size
;               SI:DI = old handle
;  Returns:     BX:CX = new base
;               SI:DI = new handle
;               all others trashed

        .type   "bin"

;-----------------------------------------------------------------------------
;  Start of API header

offset_of_api:                  ; offset of API function entry point
        .dw     sbrk_16_helper
cs_selector:                    ; code selector to be updated
        .dw     0
ds_selector:                    ; data selector to be updated
        .dw     0
local_ds:                       ; selector mapped to same as local cs
        .dw     0
bytes_to_allocate:              ; number of bytes app allocates for this image
        .dw     stack

;-----------------------------------------------------------------------------
;  Start of local data

save_ss:
        .dw     0
save_esp:
        .dd     0
save_ds:
        .dw     0

;-----------------------------------------------------------------------------
;  Start of code

sbrk_16_helper:

        mov     ax, ds                  ; switch to local data segment
        mov     ds, cs:[local_ds]
        mov     [save_ds], ax
        mov     [save_ss], ss           ; switch to local stack
        mov     [save_esp], esp
        mov     ss, [local_ds]
        mov     esp, stack

        mov     ax, 0x0503              ; realloc memory
        int     0x31
        jc      error_return            ; bx:cx = base address

        mov     dx, cx
        mov     cx, bx                  ; cx:dx = base address
        mov     bx, [cs_selector]
        mov     ax, 0x0007
        int     0x31                    ; set cs to new base
        mov     bx, [ds_selector]
        mov     ax, 0x0007
        int     0x31                    ; set ds to new base

        push    es                      ; reload es
        pop     es
        push    fs                      ; reload fs
        pop     fs
        push    gs                      ; reload gs
        pop     gs

        mov     bx, cx
        mov     cx, dx                  ; bx:cx = base address

error_return:

        mov     ss, [save_ss]           ; return to old stack
        mov     esp, [save_esp]
        mov     ds, [save_ds]           ; return to old data segment

        .opsize                         ; 32-bit far return
        retf

;-----------------------------------------------------------------------------
;  Start of stack

        .align  4                       ; so that image size is longwords
        .bss
        .align  16                      ; so that alloc size is paragraphs
        .db     512 .dup 0
stack:
