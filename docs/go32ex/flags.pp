uses
        go32;

var
        r : trealregs;

begin
        r.ax := $5300;
        r.bx := 0;
        realintr($15, r);
        if ((r.flags and carryflag)=0) then begin
                Writeln('APM v', (r.ah and $f), '.',
                        (r.al shr 4), (r.al and $f), ' detected');
        end else
                Writeln('APM not present');
end.