{ %norun% }
program tw18131;

{$mode delphi}

type
  TFoo1 = class
    type
      TFoo2 = class
        class var
          x: integer;
        constructor Create;
      end;
  end;

constructor TFoo1.TFoo2.Create;
begin
  inherited;
  inc(x);
end;

begin
  TFoo1.TFoo2.x := 0;
  TFoo1.TFoo2.Create.Destroy;
  if TFoo1.TFoo2.x<>1 then
    halt(1);
end.

