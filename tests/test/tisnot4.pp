{ %FAIL }

{ "is not" requires modeswitch delphislang }
program tisnot4;

{$mode objfpc}{$H+}

type
  TFoo = class(TObject)
  end;

var
  Obj: TObject;
begin
  Obj:=TFoo.Create;
  if Obj is not TFoo then ;
end.
