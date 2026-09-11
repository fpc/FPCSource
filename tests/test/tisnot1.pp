{ Tests the "is not" operator, which is a short form for "not (a is b)" }
program tisnot1;

{$mode objfpc}{$H+}

type
  IFoo = interface
    procedure Foo;
  end;

  TFoo = class(TObject)
  end;

  TBar = class(TFoo)
  end;

  TBaz = class(TObject)
  end;

  TIntfObj = class(TInterfacedObject,IFoo)
    procedure Foo;
  end;

procedure TIntfObj.Foo;
begin
end;

var
  Foo: TFoo;
  Obj: TObject;
  Cls: TClass;
  Intf: IFoo;
begin
  Foo:=TFoo.Create;
  Obj:=Foo;

  { the plain form still works }
  if not (Obj is TFoo) then
    halt(1);

  { "is not" with a descendant class the instance is not of }
  if not (Obj is not TBar) then
    halt(2);

  { "is not" with the class the instance is of }
  if Obj is not TFoo then
    halt(3);

  { "is not" with an unrelated class }
  if not (Obj is not TBaz) then
    halt(4);

  { class reference variable on the right side }
  Cls:=TBar;
  if not (Obj is not Cls) then
    halt(5);
  Cls:=TFoo;
  if Obj is not Cls then
    halt(6);

  { as an expression, not only as an if condition }
  if (Obj is not TBar)<>true then
    halt(7);

  Foo.Free;

  { nil is never an instance of anything }
  Obj:=nil;
  if not (Obj is not TFoo) then
    halt(8);

  { interface reference on the left side }
  Intf:=TIntfObj.Create;
  if Intf is not TIntfObj then
    halt(9);
  if not (Intf is not TBar) then
    halt(10);
  Intf:=nil;

  writeln('ok');
end.
