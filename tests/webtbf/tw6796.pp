{ %FAIL }
program intftest;
{$mode objfpc} {$H+}
uses
  Classes, SysUtils;

type
  {$INTERFACES CORBA}
  IMyCorba = interface
    ['{11111111-1111-1111-1111-111111111111}']
    procedure A;
  end;
  {$INTERFACES DEFAULT}

  TMyCorba = class(TObject, IMyCorba)
    procedure A;
  end;

procedure TMyCorba.A;
begin
  WriteLN('A: Who called me ?');
end;

var
  I: IUnknown;
  C: IMyCorba;

begin
  C := TMyCorba.Create;
  I := C as IUnknown;
//  Supports(C, IUnknown); <- gives atleast some error
end.
