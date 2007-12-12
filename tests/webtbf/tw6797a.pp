{ %fail }
program intfbug2;
{$ifdef fpc}
{$mode objfpc} {$H+}
{$endif fpc}

uses
  Classes, SysUtils;

type
  IMyCom1 = interface
    procedure A1;
  end;
  IMyCom2 = interface
    procedure A2;
  end;

  TMyComCorba = class(TInterfacedObject, IMyCom1, IMyCom2)
    procedure A1;
    procedure A2;
    procedure B;
  end;

procedure TMyComCorba.A1;
begin
  WriteLN('Com1');
end;

procedure TMyComCorba.A2;
begin
  WriteLN('Com2');
end;

procedure TMyComCorba.B;
begin
  WriteLN('Corba');
end;

var
  I: IUnknown;
  A1: IMyCom1;
  A2: IMyCom2;

begin
  I := TMyComCorba.Create;
  A1 := I as IMyCom1;
  A1.A1;
  A2 := I as IMyCom2;
  A2.A2;
end.
