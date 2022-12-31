program heapsize;

{$AsmMode ATT}

procedure syscall; external name '___SYSCALL';

var a:longint;

procedure writeheapsize;

begin
    asm
        movl $0x7f00,%eax
        xorl %edx,%edx
        call syscall       
        mov %eax,A
    end;
    writeln(a);
end;

begin
    writeheapsize;
    asm
        movl $0x7f00,%eax
        movl $327680,%edx
        call syscall
    end;
    writeheapsize;
end.
