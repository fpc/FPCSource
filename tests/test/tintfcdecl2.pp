program tintfcdecl2;

{$mode objfpc}{$H+}

type
  IcdeclIntf = interface
  ['{3C409C8B-3A15-44B2-B22D-6BAA2071CAAD}']
    function DoSomething : longint; cdecl;
  end;

  { TcdeclClass }

  TcdeclClass = class(TInterfacedObject,IcdeclIntf)
  private
    FCounter: integer;
  public
    function DoSomething : longint; cdecl; virtual;
  end;

{ TcdeclClass }

function TcdeclClass.DoSomething: longint; cdecl;
begin
  inc(FCounter);
  result := FCounter;
end;

var
  js: TcdeclClass;
  ji: IcdeclIntf;
  i: longint;
begin
  js := TcdeclClass.Create;

  i := js.DoSomething;

  ji := IcdeclIntf(js);
  i := ji.DoSomething;

  if i <> 2 then halt(1);
end.

