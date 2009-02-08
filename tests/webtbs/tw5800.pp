{$IFDEF FPC}{$mode objfpc}{$ENDIF}

uses
  sysutils;

type
{$INTERFACES CORBA}
  IAny1 = interface
    ['{949041BD-BEC9-468A-93AA-96B158EF97E0}']
    procedure x;
  end;

  IAny2 = interface
    ['IANY2']
    procedure y;
  end;

{$INTERFACES COM}

  IAny3 = interface
    ['{239041BD-BEC9-468A-93AA-96B158EF97E0}']
    procedure z;
  end;

  TAny = class(TInterfacedObject, IAny1, IAny2, IAny3)
    procedure x;
    procedure y;
    procedure z;
  end;


var
  gx,gy,gz: Integer;

procedure TAny.x;
begin
  Inc(gx);
end;

procedure TAny.y;
begin
  Inc(gy);
end;

procedure TAny.z;
begin
  Inc(gz);
end;


var
  a : TAny;
  i1 : IAny1;
  i2 : IAny2;
  i3 : IAny3;
begin
  gx := 0;
  gy := 0;
  gz := 0;
  a := TAny.Create();

  if not supports(TAny,IAny1) then
    halt(1);

  if not supports(TAny,IAny2) then
    halt(1);

  if not supports(TAny,IAny3) then
    halt(1);

  if supports(a,IAny1,i1) then
    i1.x
  else
    halt(1);

  if supports(a,IAny2,i2) then
    i2.y
  else
    halt(1);

  if supports(a,IAny3,i3) then
    i3.z
  else
    halt(1);

  (a as IAny1).x;
  (a as IAny2).y;
  (a as IAny3).z;

  if (gx<>2) or (gy<>2) or (gz<>2) then
    halt(1);
end.
