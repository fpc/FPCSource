{ %recompile }
{ %opt=-gh }

program tw22741;
{$mode objfpc}
    uses uw22741a;
    type
        te= class(td)
            procedure address(d: td); virtual;
        end;
            procedure te.address(d: td);
                var anIo: iIO;
            begin
                writeln(d.className);
                writeln(nativeuint(iIO(d)));
                writeln(nativeuint(iIO(d.fiio)));
                anIo:= d;
                writeln(nativeuint(anIo));
            end;
    var
        e1, e2: te;
begin
    e1:= te.create;
    e2:= te.create;
    e1.address(e2);
    e1.destroy;
    e2.destroy;
end.

