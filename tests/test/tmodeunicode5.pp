{ %OPT=-dUNICODE }
{ UNICODE defined by the user, here on the command line, is not a mode's to undefine: a mode selecting the
  ansistring default leaves it, and so does turning the unicodestring default on and off again.
  FPC_UNICODESTRINGS, which only the mode switch defines, goes with it. }
program tmodeunicode5;

{$mode delphi}
{$ifndef UNICODE}
  {$error mode delphi undefined the UNICODE of the command line}
{$endif}
{$ifdef FPC_UNICODESTRINGS}
  {$error FPC_UNICODESTRINGS is defined in mode delphi}
{$endif}

{$modeswitch unicodestrings}
{$ifndef UNICODE}
  {$error the unicodestrings mode switch undefined UNICODE}
{$endif}
{$ifndef FPC_UNICODESTRINGS}
  {$error FPC_UNICODESTRINGS is not defined with the unicodestrings mode switch}
{$endif}

{$modeswitch unicodestrings-}
{$ifndef UNICODE}
  {$error turning the unicodestrings mode switch off undefined the UNICODE of the command line}
{$endif}
{$ifdef FPC_UNICODESTRINGS}
  {$error FPC_UNICODESTRINGS stayed defined after the unicodestrings mode switch was turned off}
{$endif}

begin
  WriteLn('ok');
end.
