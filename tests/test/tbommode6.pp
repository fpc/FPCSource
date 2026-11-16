{ In a file with a UTF-8 signature and a mode that turns on the system code page, a literal assigned to a
  ShortString has the bytes of the system code page, like the same literal assigned to an AnsiString. On a host
  whose system code page is UTF-8 both are UTF-8 and the test cannot tell. }
{$mode delphiunicode}
program tbommode6;

const
  cUntyped = 'é';

procedure Check(const aWhat: string; const aShort: ShortString; const aAnsi: AnsiString; aExit: Integer);
var
  I: Integer;
begin
  if Length(aShort)<>Length(aAnsi) then
    begin
      Writeln(aWhat, ': ', Length(aShort), ' bytes, the AnsiString has ', Length(aAnsi));
      Halt(aExit);
    end;
  for I:=1 to Length(aShort) do
    if aShort[I]<>aAnsi[I] then
      begin
        Writeln(aWhat, ': byte ', I, ' is ', Ord(aShort[I]), ', in the AnsiString ', Ord(aAnsi[I]));
        Halt(aExit);
      end;
end;

var
  lShort: ShortString;
  lAnsi: AnsiString;
begin
  lShort:='é';
  lAnsi:='é';
  Check('literal', lShort, lAnsi, 1);
  lShort:=cUntyped;
  lAnsi:=cUntyped;
  Check('untyped constant', lShort, lAnsi, 2);
  Writeln('ok');
end.
