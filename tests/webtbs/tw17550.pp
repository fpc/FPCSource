{ %interactive }
{ %target=win32, win64 }

program consoleutf8;

{$mode objfpc}{$H+}

uses
  windows,
  sysutils;

var
  written, oldcp, newcp: LongWord;
  res: Word;
  s: String;
begin
  oldcp := GetConsoleOutputCP;
  Writeln('Old code page is: ', oldcp);
  newcp := CP_UTF8;

  if not SetConsoleOutputCP(newcp) then begin
    Writeln('Can not set output code page to ', newcp);
    Writeln('Error: ', SysErrorMessage(GetLastOSError));
  end;

  s := 'Some UTF-8 text: ÖÄÜſ' + LineEnding;
  written := 0;
  if not WriteFile(TTextRec(Output).Handle, s[1], Length(s), written, Nil) then
    Writeln('Error ', GetLastOSError, ': ', SysErrorMessage(GetLastOSError))
  else
    begin
      Writeln('Length=',Length(s),' Written=',written);
      if written<Length(s) then
        Writeln('Correct conditions to test the bug')
      else
        begin
          Writeln('Incorrect conditions to test the bug');
          Writeln('Please change Console Font to "Lucida-Console"');
          Writeln('And rerun the test');
        end;
    end;
{$I-}
  Writeln('Some UTF-8 text: ÖÄÜſ');
  res := IOResult;
  if res <> 0 then
    Writeln('IOResult was ', res);
{$I+}

  if not SetConsoleOutputCP(oldcp) then
    Writeln('Error reseting code page to ', oldcp);
  if res <> 0 then
    RunError(1)
  else
    Writeln('Test completed without error');
end.

