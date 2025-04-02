{ %OPT=-O2 }
program tw41210b;
{$MODE OBJFPC}
function strspn(s, accept: pointer): integer;
var
  p: PCardinal;
  c: AnsiChar;
  d: cardinal;
begin
  // returns size of initial segment of s which are in accept
  result := 0;
  repeat
    c := PAnsiChar(s)[result];
    if c = #0 then
      break;
    p := accept;
    repeat // stop as soon as we find any character not from accept
      d := p^;
      inc(p);
      if AnsiChar(d) = c then
        break
      else if AnsiChar(d) = #0 then
        exit;
      d := d shr 8;
      if AnsiChar(d) = c then
        break
      else if AnsiChar(d) = #0 then
        exit;
      d := d shr 8;
      if AnsiChar(d) = c then
        break
      else if AnsiChar(d) = #0 then
        exit;
      d := d shr 8;
      if AnsiChar(d) = c then
        break
      else if AnsiChar(d) = #0 then
        exit;
    until false;
    inc(result);
  until false;
end;

var
  Output: integer;
begin
  Output := strspn(PAnsiChar('abcdef'), PAnsiChar('debca'));
  if Output <> 5 then
    begin
      WriteLn('FAILED: Returned ', Output, ' instead of 5');
      Halt(1);
    end;
  WriteLn('ok');
end.
