program tw17952b;
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

  TFoo2 = Integer;

// delphi gives an error here. fpc does not.
// people thinks that this is a bug in delphi (QC# 89846)

procedure TFoo1.TFoo3.Proc(value: TFoo2);
begin
end;

begin
end.


