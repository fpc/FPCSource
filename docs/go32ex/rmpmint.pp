uses
        crt,
        go32;

var
        r : trealregs;
        axreg : Word;

        oldint21h : tseginfo;
        newint21h : tseginfo;
procedure int21h_handler; assembler;
asm
        cmpw $0x3001, %ax
        jne .LCallOld
        movw $0x3112, %ax
        iret

.LCallOld:
        ljmp %cs:oldint21h
end;

procedure resume;
begin
        Writeln;
        Write('-- press any key to resume --'); readkey;
        gotoxy(1, wherey); clreol;
end;

begin
        clrscr;
        Writeln('Executing real mode interrupt');
        resume;
        r.ah := $30; r.al := $01;  realintr($21, r);
        Writeln('DOS v', r.al,'.',r.ah, ' detected');
        resume;
        Writeln('Executing protected mode interrupt without our own',
                ' handler');
        Writeln;
        asm
                movb $0x30, %ah
                movb $0x01, %al
                int $0x21
                movw %ax, axreg
        end;
        Writeln('DOS v', r.al,'.',r.ah, ' detected');
        resume;
        Writeln('As you can see the DPMI hosts default protected mode',
                'handler');
        Writeln('simply redirects it to the real mode handler');
        resume;
        Writeln('Now exchanging the protected mode interrupt with our ',
                'own handler');
        resume;

        newint21h.offset := @int21h_handler;
        newint21h.segment := get_cs;
        get_pm_interrupt($21, oldint21h);
        set_pm_interrupt($21, newint21h);

        Writeln('Executing real mode interrupt again');
        resume;
        r.ah := $30; r.al := $01; realintr($21, r);
        Writeln('DOS v', r.al,'.',r.ah, ' detected');
        Writeln;
        Writeln('See, it didn''t change in any way.');
        resume;
        Writeln('Now calling protected mode interrupt');
        resume;
        asm
                movb $0x30, %ah
                movb $0x01, %al
                int $0x21
                movw %ax, axreg
        end;
        Writeln('DOS v', lo(axreg),'.',hi(axreg), ' detected');
        Writeln;
        Writeln('Now you can see that there''s a distinction between ',
                'the two ways of calling interrupts...');
        set_pm_interrupt($21, oldint21h);
end.