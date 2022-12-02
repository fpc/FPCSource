{ %FAIL }
{ %OPT=-Sew }

{ Note: we are speculating for "Unreachable code" warnings here }

program tb0701;

{$mode objfpc}

type
  TTest = class
    f: LongInt;
    procedure Test;
  end;

procedure TTest.Test;
begin
  if TypeInfo(f) <> TypeInfo(LongInt) then
    Writeln('False');
end;

begin

end.
