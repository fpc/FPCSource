uses
        crt,
        go32;

begin
        outportb($61, $ff);
        delay(50);
        outportb($61, $0);
end.