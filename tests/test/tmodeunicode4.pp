{ %OPT=-Municodestrings }
{ As tmodeunicode3, with a mode that sets neither string default: it too undefines the macros of the
  unicodestring default for its own module only, and a unit compiled after it still has them. }
program tmodeunicode4;

{$mode tp}

uses
  umodeunicode4;

begin
{$ifdef UNICODE}
  Halt(1);
{$endif}
  if CharSize <> 2 then
    Halt(2);
  WriteLn('ok');
end.
