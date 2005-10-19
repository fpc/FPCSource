{ Source provided for Free Pascal Bug Report 3829 }
{ Submitted by "Thomas Schatzl" on  2005-03-25 }
{ e-mail:  }
{$mode delphi}

uses
  sysutils;

type
  IAny = interface  ['{A40AEE53-8C88-4DD2-9F14-05A8C1A64849}']
  end;

  TAny = class(TInterfacedObject, IAny)
  end;

var
  a : TAny;

begin
  a:=TAny.Create;

  (a as IAny)._Addref();

  if (supports(a, IAny)) then
    writeln('ok')
  else
    halt(1);
end.
