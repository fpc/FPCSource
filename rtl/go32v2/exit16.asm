; Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details
;-----------------------------------------------------------------------------
;  exit 16-bit helper
;
;  Used to clean up 32-bit arena on exit, so as to release as many
;  selectors and as much memory as possible.
;
;  Call with:   BX = 32-bit CS to free
;               SI:DI = 32-bit memory handle to free
;               DL = exit status

        .type   "bin"

        mov     ax, 0x0001
        int     0x31

        mov     ax, 0x0502
        int     0x31

        mov     al, dl
        mov     ah, 0x4c
        int     0x21
