{$ifdef fpc}
{$mode delphi}
{$endif}

type
  FuncA = function : Integer of object;
  ObjA = class
    function Func1: Integer;
    procedure Proc1(const Arr: Array of FuncA);
  end;

var A : ObjA;

procedure test(fa: funca);
begin
  if fa<>a.func1 then
    halt(2);
end;

function ObjA.Func1: Integer;
begin
  Result := 1;
end;

procedure ObjA.Proc1(const Arr: Array of FuncA);
begin
  if (low(arr)<>0) or
     (high(arr)<>1) or
     assigned(arr[0]) or
     (arr[1]<>a.func1) then
    halt(1);
end;

begin
  A := ObjA.Create;
  A.Proc1([nil,A.Func1]);
  test(a.func1);
  a.free;
end.
