{ %NORUN }

program tw41062;

{$mode delphi}{$H+}

type

  { TTest }

  TTest = class
  public
    class procedure Test; static;
    class function Test2<T>: T; static;
  end;

class procedure TTest.Test;
var
	Obj1, Obj2: TObject;
begin
  if (AtomicCmpExchange(Pointer(Obj1), Pointer(Obj2), nil) <> nil) then; // It's ok
end;

class function TTest.Test2<T>: T;
var
	Obj1, Obj2: TObject;
begin
  AtomicCmpExchange(Pointer(Obj1), Pointer(Obj2), nil); // It's ok

  if (AtomicCmpExchange(Pointer(Obj1), Pointer(Obj2), nil) <> nil) then; // Error: Operator is not overloaded: "untyped" = "^untyped"
end;

begin
end.

