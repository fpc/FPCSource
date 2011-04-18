{ this tests that a helper in an implementation section takes precedence over a
  helper defined in the interface section }
program thlp43;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

uses
  uhlp43;

var
  res: Integer;
begin
  res := DoTest;
  Writeln('DoTest: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
