program tw17952a;
{$mode delphi}

// check visibility of nested types in method headers

type
  TFoo1 = class
  public
    type
      TFoo2 = object
      end;
      TFoo3 = object
        procedure Proc(value: TFoo2);
      end;
  end;

procedure TFoo1.TFoo3.Proc(value: TFoo2); // was error: Identifier not found "TFoo2"
begin
end;

begin
end.


