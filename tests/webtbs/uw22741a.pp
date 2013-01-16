unit uw22741a;
{$mode objfpc}

interface
    uses uw22741b;

    type
        iIO= interface
            procedure read;
            procedure write;
        end;

        tc= class(tInterfaceObject, iIO)
            procedure read; virtual;
            procedure write; virtual;
            destructor destroy; override;
        end;
    type
        td= class(tObject, iIO)
            ftc: tc;
            fiio: iIO;
            property io: tc read ftc implements iIO;
            constructor create; virtual;
            destructor destroy; override;
        end;


implementation


procedure tc.read; begin end;
procedure tc.write; begin end;
destructor tc.destroy;
begin
    writeln('tc ', nativeuint(self), ' destroyed');
    inherited;
end;

constructor td.create;
begin
    inherited;
    ftc:= tc.create;
    fiio:= ftc; // increace reference counter to one
end;
destructor td.destroy;
begin
    fiio:= nil; // ftc is automatically destroyed
    ftc.free;
    writeln('td ', nativeuint(self), ' destroyed');
    inherited;
end;

end.

