uses
        go32;

var
        r : trealregs;

begin
        r.ah := $30;
        r.al := $01;
        realintr($21, r);
        Writeln('DOS v', r.al,'.',r.ah, ' detected');
end.