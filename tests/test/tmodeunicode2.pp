{ %OPT=-Municodestrings }
{ A mode selecting the unicodestring default keeps its macros. }
program tmodeunicode2;

{$mode delphiunicode}

var
  s: string;
begin
{$ifndef UNICODE}
  Halt(1);
{$endif}
{$ifndef FPC_UNICODESTRINGS}
  Halt(2);
{$endif}
  if SizeOf(s[1]) <> 2 then
    Halt(3);
  WriteLn('ok');
end.
