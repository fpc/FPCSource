{ This example demonstrates the use of the flag constants in
conjunction with an interrupt call

In detail it checks if APM (advanced power management) is
available.

Int 15h 5300h - APM specification : Installation check
Input : AX = 5300h
        BX = device id of system BIOS (= 0000h)
Return : Carry clear if successful
        AH = major version (BCD)
        AL = minor version (BCD)
}

uses
        go32;

var
        r : trealregs;

begin
        { set register values and issue real mode interrupt call }
        r.ax := $5300;
        r.bx := 0;
        realintr($15, r);
        { check if carry clear and write a suited message }
        if ((r.flags and carryflag)=0) then begin
                Writeln('APM v', (r.ah and $f), '.',
                        (r.al shr 4), (r.al and $f), ' detected');
        end else
                Writeln('APM not present');
end.