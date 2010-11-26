program tw17986;

{$mode delphi}

type
  TFoo1 = class
  public
    type
      TFoo2 = class
        procedure Proc(value: TFoo1); // was error: Type "TFoo1" is not completely defined
      end;
  end;

procedure TFoo1.TFoo2.Proc(value: TFoo1);
begin
end;

begin
end.

