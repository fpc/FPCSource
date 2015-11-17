; test for linking a TASM external object module for the i8086-msdos platform
; assemble with TASM version 3.2 - it is the most commonly used version for
; linking with 16-bit pascal code, since it comes bundled with Borland Pascal 7

          .MODEL  large,PASCAL
          .DATA
          .CODE
IncDWord  PROC FAR  a: DWORD
          PUBLIC IncDWord
          mov ax, word ptr a
          mov dx, word ptr [a+2]
          add ax, 1
          adc dx, 0
          ret
IncDWord  ENDP
          END
