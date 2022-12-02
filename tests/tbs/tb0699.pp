{ %FAIL }
{ %OPT=-Sew }

{ Note: we are speculating for "Unreachable code" warnings here }

program tb0699;

procedure Test(aArg: LongInt);
begin
  if TypeInfo(aArg) <> TypeInfo(LongInt) then
    Writeln('False');
end;

begin

end.
