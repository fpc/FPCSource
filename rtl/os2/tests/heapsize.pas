program heapsize;

var a:longint;

procedure writeheapsize;

begin
    asm
        movl $0x7f00,%ax
        xorl %edx,%edx
        call ___syscall
        mov %eax,_A
    end;
    writeln(a);
end;

begin
    writeheapsize;
    asm
        movl $0x7f00,%ax
        movl $327680,%edx
        call ___syscall
    end;
    writeheapsize;
end.
