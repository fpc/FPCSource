{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 3577 }
{ Submitted by "Simon Kissel" on  2005-01-19 }
{ e-mail: scamp@untergrund.net }

{$mode delphi}

procedure IntCopy16;
asm
        MOV     EAX,[ESI]
        MOV     [EDI],EAX
        MOV     EAX,[ESI+4]
        MOV     [EDI+4],EAX
        MOV     EAX,[ESI+8]
        MOV     [EDI+8],EAX
        MOV     EAX,[ESI+12]
        MOV     [EDI+12],EAX
        MOV     EAX,[ESI+16]
        MOV     [EDI+16],EAX
        MOV     EAX,[ESI+20]
        MOV     [EDI+20],EAX
        MOV     EAX,[ESI+24]
        MOV     [EDI+24],EAX
        MOV     EAX,[ESI+28]
        MOV     [EDI+28],EAX
        MOV     EAX,[ESI+32]
        MOV     [EDI+32],EAX
        MOV     EAX,[ESI+36]
        MOV     [EDI+36],EAX
        MOV     EAX,[ESI+40]
        MOV     [EDI+40],EAX
        MOV     EAX,[ESI+44]
        MOV     [EDI+44],EAX
        MOV     EAX,[ESI+48]
        MOV     [EDI+48],EAX
        MOV     EAX,[ESI+52]
        MOV     [EDI+52],EAX
        MOV     EAX,[ESI+56]
        MOV     [EDI+56],EAX
        MOV     EAX,[ESI+60]
        MOV     [EDI+60],EAX
end;

procedure Q_CopyMem(Source, Dest: Pointer; L: Cardinal);
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     EDX,ECX
        MOV     ESI,EAX
        TEST    EDI,3
        JNE     @@cl
        SHR     ECX,2
        AND     EDX,3
        CMP     ECX,16
        JBE     @@cw0
@@lp0:  CALL    IntCopy16
        ADD     ESI,64
        SUB     ECX,16
        ADD     EDI,64
        CMP     ECX,16
        JA      @@lp0
@@cw0:  JMP     DWORD PTR @@wV[ECX*4]
@@cl:   MOV     EAX,EDI
        MOV     EDX,3
        SUB     ECX,4
        JB      @@bc
        AND     EAX,3
        ADD     ECX,EAX
        JMP     DWORD PTR @@lV[EAX*4-4]
@@bc:   JMP     DWORD PTR @@tV[ECX*4+16]
@@lV:   DD      @@l1, @@l2, @@l3
@@l1:   AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     [EDI],AL
        MOV     AL,[ESI+1]
        MOV     [EDI+1],AL
        MOV     AL,[ESI+2]
        SHR     ECX,2
        MOV     [EDI+2],AL
        ADD     ESI,3
        ADD     EDI,3
        CMP     ECX,16
        JBE     @@cw1
@@lp1:  CALL    IntCopy16
        ADD     ESI,64
        SUB     ECX,16
        ADD     EDI,64
        CMP     ECX,16
        JA      @@lp1
@@cw1:  JMP     DWORD PTR @@wV[ECX*4]
@@l2:   AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     [EDI],AL
        MOV     AL,[ESI+1]
        SHR     ECX,2
        MOV     [EDI+1],AL
        ADD     ESI,2
        ADD     EDI,2
        CMP     ECX,16
        JBE     @@cw2
@@lp2:  CALL    IntCopy16
        ADD     ESI,64
        SUB     ECX,16
        ADD     EDI,64
        CMP     ECX,16
        JA      @@lp2
@@cw2:  JMP     DWORD PTR @@wV[ECX*4]
@@l3:   AND     EDX,ECX
        MOV     AL,[ESI]
        MOV     [EDI],AL
        INC     ESI
        SHR     ECX,2
        INC     EDI
        CMP     ECX,16
        JBE     @@cw3
@@lp3:  CALL    IntCopy16
        ADD     ESI,64
        SUB     ECX,16
        ADD     EDI,64
        CMP     ECX,16
        JA      @@lp3
@@cw3:  JMP     DWORD PTR @@wV[ECX*4]
@@wV:   DD      @@w0, @@w1, @@w2, @@w3
        DD      @@w4, @@w5, @@w6, @@w7
        DD      @@w8, @@w9, @@w10, @@w11
        DD      @@w12, @@w13, @@w14, @@w15
        DD      @@w16
@@w16:  MOV     EAX,[ESI+ECX*4-64]
        MOV     [EDI+ECX*4-64],EAX
@@w15:  MOV     EAX,[ESI+ECX*4-60]
        MOV     [EDI+ECX*4-60],EAX
@@w14:  MOV     EAX,[ESI+ECX*4-56]
        MOV     [EDI+ECX*4-56],EAX
@@w13:  MOV     EAX,[ESI+ECX*4-52]
        MOV     [EDI+ECX*4-52],EAX
@@w12:  MOV     EAX,[ESI+ECX*4-48]
        MOV     [EDI+ECX*4-48],EAX
@@w11:  MOV     EAX,[ESI+ECX*4-44]
        MOV     [EDI+ECX*4-44],EAX
@@w10:  MOV     EAX,[ESI+ECX*4-40]
        MOV     [EDI+ECX*4-40],EAX
@@w9:   MOV     EAX,[ESI+ECX*4-36]
        MOV     [EDI+ECX*4-36],EAX
@@w8:   MOV     EAX,[ESI+ECX*4-32]
        MOV     [EDI+ECX*4-32],EAX
@@w7:   MOV     EAX,[ESI+ECX*4-28]
        MOV     [EDI+ECX*4-28],EAX
@@w6:   MOV     EAX,[ESI+ECX*4-24]
        MOV     [EDI+ECX*4-24],EAX
@@w5:   MOV     EAX,[ESI+ECX*4-20]
        MOV     [EDI+ECX*4-20],EAX
@@w4:   MOV     EAX,[ESI+ECX*4-16]
        MOV     [EDI+ECX*4-16],EAX
@@w3:   MOV     EAX,[ESI+ECX*4-12]
        MOV     [EDI+ECX*4-12],EAX
@@w2:   MOV     EAX,[ESI+ECX*4-8]
        MOV     [EDI+ECX*4-8],EAX
@@w1:   MOV     EAX,[ESI+ECX*4-4]
        MOV     [EDI+ECX*4-4],EAX
        SHL     ECX,2
        ADD     ESI,ECX
        ADD     EDI,ECX
@@w0:   JMP     DWORD PTR @@tV[EDX*4]
@@tV:   DD      @@t0, @@t1, @@t2, @@t3
@@t3:   MOV     AL,[ESI+2]
        MOV     [EDI+2],AL
@@t2:   MOV     AL,[ESI+1]
        MOV     [EDI+1],AL
@@t1:   MOV     AL,[ESI]
        MOV     [EDI],AL
@@t0:   POP     ESI
        POP     EDI
end;

begin
end.
