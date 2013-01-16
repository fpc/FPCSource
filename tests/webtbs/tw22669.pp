program tw22669;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$else}
  {$APPTYPE CONSOLE}
{$endif}

var buf:array[1..11] of widechar;
  s:ansistring;
begin
  buf:='isnotempty';
  s:='test';
  StringToWideChar(s,@buf[1],10);
  s:=widestring(pwidechar(@buf[1]));
  if s<>'test' then
    halt(1);
  s:='0123456789';
  StringToWideChar(s,@buf[1],10);
  s:=widestring(pwidechar(@buf[1]));
  if s<>'012345678' then
    halt(2);
 end.

