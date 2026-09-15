{ modeswitch delphislang, and thus "is not", is enabled by mode delphi }
program tisnot5;

{$mode delphi}

type
  TFoo = class(TObject)
  end;

  TBar = class(TFoo)
  end;

var
  Obj: TObject;
begin
  Obj:=TFoo.Create;
  if Obj is not TFoo then
    halt(1);
  if not (Obj is not TBar) then
    halt(2);
  Obj.Free;
  writeln('ok');
end.
