{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  FuncA = function : Integer of object;
  ObjA = class
    function Func1: Integer;
    procedure Proc1(const Arr: Array of char);
  end;

var A : ObjA;

function ObjA.Func1: Integer;
begin
  Result := 1;
end;

procedure ObjA.Proc1(const Arr: Array of char);
begin
end;

begin
  A := ObjA.Create;
  A.Proc1([A.Func1]);
  a.free;
end.
