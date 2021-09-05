
program project1;

{$mode objfpc}{$H+}{$R+}
{$ZeroBasedStrings On}

uses sysutils;

var
  S: String = 'Cat';
  U: UnicodeString = 'Cat';
  W: WideString = 'Cat';
begin
  S[0] := 'H';
  if S<>'Hat' then
    halt(1);
  U[0] := 'H';
  if U<>'Hat' then
    halt(1);
  W[0] := 'H';
  if W<>'Hat' then
    halt(1);

  WriteLn('ok');
end.
