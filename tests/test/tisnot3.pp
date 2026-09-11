{ %FAIL }

{ "as not" must not be accepted, only "is not" has the short form }
program tisnot3;

{$mode objfpc}{$H+}

type
  TFoo = class(TObject)
  end;

var
  Obj: TObject;
  Foo: TFoo;
begin
  Obj:=TFoo.Create;
  Foo:=Obj as not TFoo;
  if Foo=nil then ;
end.
