{ %FAIL }
{$MODE delphi}
type
  TProc = procedure of object;
  TMyClass = class
    i: Integer;
    procedure Test;
    procedure Doit;
  end;

procedure TMyClass.Test;
begin
  WriteLn('i = ', i);
end;

procedure TMyClass.Doit;
var
  p: TProc = @Test;
begin
  i := 12345;
  p;
end;

var
  o: TMyClass;
begin
  o := TMyClass.Create;
  o.Doit;
end.
