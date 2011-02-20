program tchlp55;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TTest = class
  strict private
    type
      TFooHelper = class helper for TObject
        procedure Test;
      end;
  end;

procedure TTest.TFooHelper.Test;
begin

end;

var
  o: TObject;
begin
  o := TObject.Create;
  o.Test;
end.
