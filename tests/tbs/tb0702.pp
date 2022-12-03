{ %NORUN }
{ %OPT=-Sew }

{ don't optimize TypeInfo comparisons if undefined types are involved }

program tb0702;

{$mode objfpc}

type
  generic TTest<S> = class
    procedure Test;
  end;

procedure TTest.Test;
begin
  if TypeInfo(S) = TypeInfo(LongInt) then
    Writeln('Test');
end;

begin

end.
