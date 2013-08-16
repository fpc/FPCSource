{$ifdef fpc}{$mode objfpc}{$endif}
type
  TMyClass = class
    procedure MyAbstractMethod; virtual; abstract;
    procedure MyAbstractMethod2; virtual; abstract;
  end;

  TMyClass2 = class(TMyClass)
  end;

var
  Foo,Foo2: Pointer;
begin
  Foo := @TMyClass.MyAbstractMethod;
  Foo2 := @TMyClass.MyAbstractMethod2;
  if Foo=Foo2 then
    Halt(1);
  Foo2 := @TMyClass2.MyAbstractMethod;
  if Foo<>Foo2 then
    Halt(2);
end.
