{ %norun }

unit tw19434b;
{$ifdef fpc}
{$mode delphi}
{$endif}

interface

type
  tintf = interface
    procedure connect(s: string; port: longint = 23);
  end;

  tc = class(tinterfacedobject,tintf)
    procedure connect(s: string; port: longint);
  end;


implementation


  procedure tc.connect(s: string; port: longint);
    begin
    end;

end.

