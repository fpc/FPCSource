{ UNICODE defined by the user with a directive before the mode is not the mode's to undefine. }
program tmodeunicode6;

{$define UNICODE}
{$mode delphi}
{$ifndef UNICODE}
  {$error mode delphi undefined the user's UNICODE}
{$endif}
{$ifdef FPC_UNICODESTRINGS}
  {$error FPC_UNICODESTRINGS is defined in mode delphi}
{$endif}

var
  s: string;
begin
  if SizeOf(s[1]) <> 1 then
    Halt(1);
  WriteLn('ok');
end.
